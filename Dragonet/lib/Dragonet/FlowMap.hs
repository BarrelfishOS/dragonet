-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE ExistentialQuantification,GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}
module Dragonet.FlowMap (
    -- * Introduction
    -- $intro
    -- * FlowMap
    QMap,          -- exported for documentation purposes
    FlowMap,       -- exported for documentation purposes
    FlowCache,
    FlowMapSt(..),
    initFlowMapSt,
    --
    incrConfigure,
    rebuildFlowMapSt,
    rmFlow,
    getRxQMap,
    fmGetCCs, fmGetConf,
    strFM,strFlowMap
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Configuration       as C
import qualified Dragonet.Predicate           as PR

import Dragonet.Flows (Flow, flowPred, flowStr)
import Dragonet.Conventions (isTruePort, isFalsePort, QueueId)

import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Map.Strict                as M
import qualified Data.List                      as L
import qualified Data.Set                       as S

import qualified Control.Monad.State.Strict     as SM

-- mutable state hashtable
import qualified Control.Monad.ST as ST
import qualified Data.HashTable.ST.Basic as HB
import qualified Data.HashTable.Class    as H

import Data.Maybe
import Data.Either
import Control.Exception (assert)
import Control.Applicative ((<$>))
import Control.Monad (foldM)

import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)
tr a b  = trace b a
trN a b = a

defBld = PR.predBuildDNF

-- $intro
--
-- Search optimizes user-defined cost functions. These cost functions operate on
-- a mapping of flows into queues ('QMap') The code here aims to incrementally
-- compute 'QMap' as flows come and go.
--
-- Code operates on the 'FlowMapSt' state. The state maintains a partially
-- configured PRG, which allows for /partial configuration/. In /normal/
-- configuration a CNode is replaced with normal (non-CNode) nodes. In /partial/
-- configuration a CNode is replaced with normal nodes /and/ a new CNode. A
-- partially configured PRG can be fully configured by applying an empty config
-- ('C.emptyConfig') at any point. See "Configuration" for more details.
--
-- In addition to the partially configured graph, the state maintains a
-- 'FlowMap', i.e., for each node a mapping on how different flows map to
-- different ports. The goal of the code here is to maintain a valid 'FlowMap'
-- for each node and maintain as much information as possible when changes
-- happen.


-- |Qmap is what user-defined cost functions typically operate on. It's a map of
-- flows into queues.
--
-- TODO: There is also one in Search.hs. Put them in a single file (e.g.,
-- Cost.hs)
type QMap        = [(Flow, QueueId)]

-- | A 'FlowMap' maps each node's port to a set of flows
--
-- As a potential optimization, we might consider using:
-- Data.IntMap and/or Data.Vector for this
type FlowMap = M.Map DGI.Node [(PG.NPort, [Flow])]


-- | Caching the port a flow will end up in a node. As a node representation we
-- use the label, i.e., we assume that the nodes with the same label have the
-- same predicate
type FlowCache s = HB.HashTable s (PG.NLabel, Flow) PG.NPort

-- | Flow map state.
-- This essentially represents a solution: it includes a list of configuration
-- changes and the registered flows.
data FlowMapSt s cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph        :: PG.PGraph -- ^ the partially configured PRG
                                  --   (gets updated with every state change)
    , fmOrigGraph    :: PG.PGraph -- ^ fully unconfigured PRG (initial state)

    , fmCnfState     :: [(Maybe cc, Maybe Flow)]
    -- ^ Configuration state. each configuration change typically corresponds to
    -- a new flow, and we maintain this information here.  We can add ccs that
    -- are not associated with flows as (Just cc, Nothing) and vice-versa.

    , fmFlowMap      :: FlowMap  -- ^ current flow map
    , fmFlowCache    :: FlowCache s -- ^ flow cache

    -- caching some nodes so that we do not have to look for them every time
    , fmRxEntryNid   :: DGI.Node -- ^ Entry node for the receive side
    , fmTxQueueNodes :: [(QueueId, DGI.Node)] -- ^ Tx Queue nodes
    , fmRxQueueNodes :: [(QueueId, DGI.Node)] -- ^ Rx Queue nodes

    , fmCCNodesMap   :: M.Map cc [(DGI.Node,PG.Node)]
    -- ^ This maps each configuration change to the nodes it created

    -- for incremental qmap
    -- , fmQmapOld     :: QMap -- old qmap
    -- , fmQmapNewCc   :: [cc]
    -- , fmQmapNewFls  :: [Flow]
}

-- | provide a string for configuration state
fmCnfStateStr_ :: (C.ConfChange cc) => (Maybe cc, Maybe Flow) -> String
fmCnfStateStr_ (Just cc, Nothing) = "(" ++ (C.ccShow cc) ++ " [NO FLOW])"
fmCnfStateStr_ (Just cc, Just fl) = "(" ++ (C.ccShow cc) ++ " [" ++ flowStr fl ++ "])"
fmCnfStateStr_ (Nothing, Nothing) = "([NO CONF]" ++ " [NO FLOW])"
fmCnfStateStr_ (Nothing, Just fl) = "([NO CONF]" ++ " [" ++ flowStr fl ++ "])"

-- convention: some nodes (typically sink nodes) do not have ports, so we add
-- them to the flowMap using sinkPort
fmSinkPort = ""

--
-- FlowMap helpers
--

-- | get the set of flows for a (node,port) or an error string
fmGetFlow :: FlowMap -> DGI.Node -> PG.NPort -> Either [Flow] String
fmGetFlow fm nid port =
    case M.lookup nid fm of
      Just x -> case L.lookup port x of
                  Just y  -> Left y
                  Nothing -> Right noPortMsg
      Nothing -> Right noNodeMsg
    where noNodeMsg = "node does not exist in flow map"
          noPortMsg = "port:" ++ port ++ " does not exist in flow map"

-- | helper for addFlowPort_
addFlowPort_ :: [(PG.NPort, [Flow])] -> PG.NPort -> Flow -> [(PG.NPort, [Flow])]
addFlowPort_ [] port flow = [(port, [flow])]
addFlowPort_ ((p,flows):xs) port flow
    | p == port = (p,(flow:flows)):xs
    | otherwise = (p,flows):(addFlowPort_ xs port flow)

addFlowPort :: FlowMap -> PG.PGNode -> PG.NPort -> Flow -> FlowMap
addFlowPort fm (nid,nlbl) port flow = fm'
 where fm' = M.alter updateF nid fm
       updateF :: Maybe [(PG.NPort, [Flow])] -> Maybe [(PG.NPort, [Flow])]
       -- mapping already exists
       updateF (Just portMap) = Just $ addFlowPort_ portMap port flow
       -- mapping does not exist, create a new one
       updateF Nothing  = case (nlbl,port) of
                               (PG.FNode {}, _) -> Just [(port, [flow])]
                               (PG.ONode {}, p) -> Just $ oNodeVal p flow
       --updateF Nothing = error "addFLowPort: node does not exist in flow map"
       oNodeVal port_ fl_ = [(port_, [fl_]), (notPort port_, [])]
       notPort "true" = "false"
       notPort "false" = "true"

-- update flow map traverses the graph starting from a set of given nodes and
-- updates the flow map when possible.
--
-- A set of all flows is provided, but this is only used for entry nodes, i.e.,
-- nodes that do not have any predecessors.
--
--  A node is either:
--    in the flowmap => we know its predicates and they will not change if we
--    add more configuration changes
--    not in the flowmap => we do not know its predicates
fmUpdateNewNodes :: [Flow] -> FlowMap -> PG.PGraph -> [DGI.Node] -> FlowMap
--fmUpdateNewNodes [] _ _ _ = fm -- TODO: no flows, no update needed
fmUpdateNewNodes flows fm _ [] = fm
fmUpdateNewNodes flows fm g nids = trN ret "fmUpdateNewNodes"
    where ret' = assert c1 ret
          ret = case L.break isReady nids of
              -- we found a node: compute new flow map and recurse
              (xs1,xNid:xs2) ->
                    let xNode = fromJust $ DGI.lab g xNid
                        fm'   = doUpdateFlowMap flows fm g (xNid, xNode)
                     in fmUpdateNewNodes flows fm' g (xs1 ++ xs2)
              -- no node found
              (before,[]) -> fm
          -- check if a node is ready to be inserted into the flowmap:
          --  . it must not be a cnode
          --  . all of its predecessors must be in the flowmap
          isReady :: DGI.Node -> Bool
          isReady nid = ret
              where nidPrevs = PGU.preNE g nid
                    prevsInFlowMap = all (\x -> M.member x fm) nidPrevs
                    nlbl = fromJust $ DGI.lab g nid
                    notCnode = not $ PGU.isCnode_ $ nlbl
                    txNode = isTxNode_ nlbl

                    ret_ = notCnode && (not txNode) && prevsInFlowMap
                    ret = trN ret_ rdMsg
                    rdMsg = "fmUpdateNewNodes: is node " ++ (PG.nLabel nlbl)
                            ++ " ready? " ++  (show ret_)
                            ++ " prevsInFlowMap? " ++ (show prevsInFlowMap)
                            ++ " PREVS:" ++ (show $ [ PG.nLabel $ fromJust $ DGI.lab g p | p <- nidPrevs])
          -- check1: all given nodes are keys in the (old) flowmap
          c1 = all (\nid -> M.notMember nid fm) nids

-- low level function for computing a flow map value
mkFlowMapVal :: [(PG.NPort, PR.PredExpr)]  -- (port, predicate)
             -> [(Flow, PR.PredExpr)]      -- (flow, predicate)
             -> [(PG.NPort, [Flow])]       -- how flows are mapped into ports
mkFlowMapVal ((port,portPred):rest) flowTs = ret
    where -- each recursion step deals with a single port
          -- Note that only one port will be activated for each flow, so we need
          -- only consider the unmatched flows for the rest of the ports
          ret  = ret0:recurse
          ret0 = (port, [f | (f,_) <- matchedFlowsTs ])
          recurse = mkFlowMapVal rest unmatchedFlowsTs
          -- split flows into two parts: those that SAT the port, and those who
          -- do not
          (matchedFlowsTs,unmatchedFlowsTs) = L.partition portSAT flowTs
          -- check whether a port satisfies a flow
          portSAT_ (p,pPred) (f,fPred) = isJust $ PR.dnfSAT andExpr
            where andExpr = PR.buildAND defBld [pPred, fPred]
          portSAT = portSAT_ (port, portPred)
-- terminate recursion
mkFlowMapVal [] flowTs =
  case flowTs of [] -> []
                 _  -> error "mkFlowMapVal: Flows exist that do not match node!"

-- update the flowmap to include the mappings for the given node
doUpdateFlowMap :: [Flow] -> FlowMap -> PG.PGraph -> PG.PGNode -> FlowMap
-- node is an F-node
-- We assume that:
--  - the node does *not* exist in the flow map
--  - it's predecessor (node,port) exists in the map, or if it does not then
--    there are no flows for this node
doUpdateFlowMap allFlows fm g node@(nid,fnode@(PG.FNode {})) = fm'
  -- get flow map of previous node
  where fm' = M.insertWith errExists nid fmVal fm
        errExists = error $ "doUpdateFlowMap: Node already exists in flow map: " ++  (PG.nLabel $ fromJust $ DGI.lab g nid )
        -- new flow map
        fmVal_ = mkFlowMapVal portPreds flowPreds
        fmVal = trN fmVal_ fmValMsg
        fmValMsg ="  NEW FMVAL FOR NODE: " ++ (PG.nLabel fnode)
        -- compute predicates for all flows
        -- NB: we might want to cache this
        flowPreds = [(f, flowPred f) | f <- flows]
        portPreds = [(p, (PR.portPred_ defBld fnode) p) | p <- PG.nPorts fnode]
        -- compute predicates for all ports of the node
        pres :: [(PG.PGNode, PG.NPort)]
        pres = PGU.getPrePort g node
        flows_ = case pres of
                  -- this is the entry node
                  [] -> allFlows
                  [((prevNid, prevNode), prevPort)] ->
                        -- get the flows from the flowMap.
                        case fmGetFlow fm prevNid prevPort of
                            Left fs   -> fs
                            Right msg -> []
                  otherwise -> error $ ("Fnode with >1 pres" ++ (PG.nLabel fnode))
        flows = trN flows_ flowsMsg
        flowsMsg = "FLOWS FOR: " ++ (PG.nLabel fnode) ++ " " ++ (ppShow flows_)
--
-- node is an O-node
-- As with Fnodes, we assume that:
--  - the node does *not* exist in the flow map
--  - it's predecessor (node,port) exists in the map, or if it does not then
--    there are no flows for this node
doUpdateFlowMap _ fm g node@(nid, (PG.ONode { PG.nOperator = op})) = fm'
    where fm' = M.insertWith errExists nid fmVal fm
          fmVal_ = [("true", trueFlowsOut), ("false", falseFlowsOut)]
          fmVal  = trN fmVal_ fmValMsg
          errExists = error "doUpdateFlowMap: Node already exists in flow map"
          fmValMsg = "ONODE " ++ (PG.nLabel $ snd node)
                     ++ " UPDATE:" ++ (ppShow fmVal_)
          -- get true/false operand maps:
          ops :: [DGI.Node]
          opsTrueMap :: M.Map DGI.Node PG.NPort
          (ops, opsTrueMap, opsFalseMap) = PGU.oNodeOperandsMap g node
          -- get flows for each operand
          getFlows :: M.Map DGI.Node PG.NPort -> DGI.Node -> [Flow]
          getFlows portMap nid = case M.lookup nid portMap of
                Nothing    -> [] -- operand is not connected
                Just port  -> case fmGetFlow fm nid port of
                        Left x    -> x
                        Right msg -> []
          -- true and false incoming flows
          trueFlowsIn :: [S.Set Flow]
          trueFlowsIn  = [S.fromList $ getFlows opsTrueMap nid  | nid <- ops]
          falseFlowsIn = [S.fromList $ getFlows opsFalseMap nid | nid <- ops]
          -- S.unions is defined as L.foldl S.union S.empty
          -- not sure why intersections is not defined
          intersections [] = S.empty
          intersections xs = L.foldl S.intersection (head xs) (tail xs)
          trueFlowsOut :: [Flow]
          trueFlowsOut = S.toList $ case op of
                            PG.NOpOr  -> S.unions trueFlowsIn
                            PG.NOpAnd -> intersections trueFlowsIn
          falseFlowsOut = S.toList $ case op of
                            PG.NOpOr  -> intersections falseFlowsIn
                            PG.NOpAnd -> S.unions falseFlowsIn

-- compute a flowmap from scratch
initFlowMap :: [Flow] -> PG.PGraph -> FlowMap
initFlowMap flows g = fmUpdateNewNodes flows M.empty g (DGI.nodes g)

--
-- FlowMapSt functions
--

resetFlowMapSt :: (C.ConfChange cc)
               => (FlowMapSt s cc) -> ST.ST s (FlowMapSt s cc)
resetFlowMapSt st = do
    return $ st {
            fmGraph    = g
          , fmCnfState = []
          , fmFlowMap  = initFlowMap [] g
   }
   where g = fmOrigGraph st

-- rebuild flowmap state using a new sate of flows and ccs
rebuildFlowMapSt :: (C.ConfChange cc)
                 => (FlowMapSt s cc) -> [(cc, Maybe Flow)]
                 -> ST.ST s (FlowMapSt s cc)
rebuildFlowMapSt st ccflows = do
    st1 <- resetFlowMapSt st
    st3 <- foldM incrConfigure st1 ccflows
    return st3

-- intitialize flow map state
initFlowMapSt :: (C.ConfChange cc) => PG.PGraph -> ST.ST s (FlowMapSt s cc)
initFlowMapSt g = do
    h <- H.new
    return $ FlowMapSt {
            fmGraph        = g
          , fmOrigGraph    = g
          , fmCnfState     = []
          , fmFlowMap      = fm
          , fmRxEntryNid   = rxEntryNid
          , fmTxQueueNodes = txQNs
          , fmRxQueueNodes = rxQNs
          , fmFlowCache    = h
          , fmCCNodesMap   = M.empty
   }
 where fm = initFlowMap [] g -- XXX: This call is not needed since flows are []
       -- Find the rxEntrynode to use when adding flows
       --((_,rxEntryNid,_,_),_) =  flowQueueStart
       entryNodes = [nid | nid <- DGI.nodes g, length (PGU.preNE g nid) == 0]
       rxEntryNid = case filter (isNidRxNode g) entryNodes of
         [x] -> trN x $ "Entry node: " ++ PG.nLabel (fromJust $ DGI.lab g x)
         []  -> error $ "initFlowMapSt: no entry node found" ++ "g=" ++ (ppShow $ DGI.labNodes g)
         _   -> error $ "initFlowMapSt: more than one entry node found" ++ "g=" ++ (ppShow $ DGI.labNodes g)
       lNodes = DGI.labNodes g
       -- Rx Queue nodes (we use those to calculate qmap)
       rxQNs = [(nid,qid) | (nid, nlbl) <- lNodes
                          , let mqid = PGU.nodeRxQueueId nlbl
                             , isJust mqid
                             , let Just qid = mqid]
       -- Tx Queue nodes (we do not really care about those right now)
       txQNs = [(nid,qid) | (nid, nlbl) <- lNodes
                          , let mqid = PGU.nodeTxQueueId nlbl
                          , isJust mqid
                          , let Just qid = mqid]

-- Add a flow
addFlowOnly :: (C.ConfChange cc)
        => FlowMapSt s cc -> Flow -> ST.ST s (FlowMapSt s cc)
addFlowOnly st flow = do
    let g     = fmGraph st
        fm    = fmFlowMap st
        cst   = fmCnfState st
        entry = fmRxEntryNid st
        fc    = fmFlowCache st
    fm' <- flowMapAddFlow fc g entry fm flow
    return $ st {fmFlowMap = fm',
                 fmCnfState = (Nothing, Just flow):cst}

cnfStRemoveFlow :: forall s cc. (C.ConfChange cc)
                => Flow -> [(Maybe cc, Maybe Flow)]
                -> [(Maybe cc, Maybe Flow)]
cnfStRemoveFlow fl [] = error "cnfStRemoveFlow: flow not found"
cnfStRemoveFlow fl (x@(cc,Nothing):xs) = x:cnfStRemoveFlow fl xs
cnfStRemoveFlow fl (x@(mcc, Just xfl):xs) =
    case fl == xfl of
       True  ->  (mcc, Nothing):xs
       False ->  x:(cnfStRemoveFlow fl xs)

fmGetCCs :: forall s cc. (C.ConfChange cc)
         => FlowMapSt s cc -> [cc]
fmGetCCs st = catMaybes $ map fst $ fmCnfState  st

fmGetConf :: forall s cc. (C.ConfChange cc)
          => FlowMapSt s cc -> [(cc, Maybe Flow)]
fmGetConf st = [ (cc, mfl) | (mcc,mfl) <- fmCnfState  st,
                             isJust mcc,
                             let cc = fromJust mcc ]

-- Removing a flow: Each cc in fmCnfChanges has a corresponding flow
-- There are two ways to remove a flow
--  - we can remove a flow from .fmFlows, but keep the configuration
--    around
--  - we can remove the flow *and* the corresponding configuration change. In
--  this case we need to recompute the flowmap.
--  We do the former here by rebuilding the flowmap
rmFlow :: forall s cc. (C.ConfChange cc)
       => FlowMapSt s cc -> Flow -> ST.ST s (FlowMapSt s cc)
rmFlow st fl = return $ st { fmCnfState = cnfs', fmFlowMap = fm' }
    where cnfs     = fmCnfState st
          fm       = fmFlowMap st
          g        = fmGraph st
          entryNid = fmRxEntryNid st
          cnfs'  = cnfStRemoveFlow fl cnfs
          fm'    = fmDelFlow g entryNid fm fl

qmapStr_ :: (Flow,QueueId) -> String
qmapStr_ (f,q) = (flowStr f) ++ "-> Q" ++ (show q)

qmapStr :: [(Flow,QueueId)] -> String
qmapStr flows = "QMAP:\n"
              ++ L.intercalate "\n" [ "  " ++ qmapStr_ f | f <- flows ]

sAddPr p str = L.unlines $ [ p ++ s | s <- L.lines str ]

strFlowMap :: forall s cc. C.ConfChange cc => FlowMapSt s cc -> String
strFlowMap st = unlines strs
    where g = fmGraph st
          strs = [ n ++ ":" ++ (getM nid)
                        | (nid,nlbl) <- DGI.labNodes g,
                        let n = PG.nLabel nlbl,
                        not $ isTxNode_ nlbl]
          getM nid = case M.lookup nid (fmFlowMap st) of
                  Nothing -> " Empty!"
                  Just x  -> "\n" ++ (sAddPr "\t" $ ppShow x)

strFM :: forall s cc. C.ConfChange cc => FlowMapSt s cc -> ST.ST s String
strFM st =  return $
         "STEP : " ++ (show $ length $ fmCnfState st)
         ++ " ------------------------------------------\n"
         -- ++ "CNF  : " ++ cnfStr ++ "\n"
         -- ++ "FLOWS: " ++ flsStr ++ "\n"
         -- ++ "QMAP :"  ++ (qmapStr qmap) ++ "\n"
         ++ "FMAP SIZE :"  ++ (show $ length fmapL) ++ "\n"
         ++ "FMAP :"  ++ fmapS ++ "\n"
    where --cnf    = C.foldConfChanges (fmCnfState st)
          --cnfStr = C.showConfig (undefined::cc) cnf
          --flsStr = L.intercalate "\n" [" " ++ (flowStr f) | f <- (fmFlows st)]
          --qmap   = fst $ runFM getRxQMap st
          g      = fmGraph st
          fmapL  = M.toList $ fmFlowMap st
          fmapS  = "\n\t" ++ (L.intercalate "\n\t->" $ map flmapS fmapL)
          flmapS :: (DGI.Node,  [(PG.NPort, [Flow])]) -> String
          flmapS (nid, portflows) = nlbl  ++ (ppShow portflows)
                where nlbl = PG.nLabel $ fromJust $ DGI.lab g nid

--newtype FlowMapM cc a = FlowMapM { unFlowMapM :: SM.State (FlowMapSt cc) a }
--    deriving (Monad, SM.MonadState (FlowMapSt cc), Functor)

isRxNode_ :: PG.Node -> Bool
isRxNode_ n = "Rx" `L.isPrefixOf` (PG.nLabel n)

isTxNode_ :: PG.Node -> Bool
isTxNode_ n = "Tx" `L.isPrefixOf` (PG.nLabel n)

isNidRxNode :: PG.PGraph -> DGI.Node -> Bool
isNidRxNode g nid = isRxNode_ nlbl
    where nlbl = fromJust $ DGI.lab g nid

-- | Add an incremental configuration and an associated flow to the state. This
-- is typically executed after a search step that considered a new flow.
incrConfigure :: (C.ConfChange cc)
              => FlowMapSt s cc -> (cc, Maybe Flow) -> ST.ST s (FlowMapSt s cc)
incrConfigure st (cc, mflow) =
    case C.ccIsReplace cc of
        True  -> doIncrReplace st (cc, mflow)
        False -> doIncrConfigure st (cc, mflow)

doIncrReplace :: (C.ConfChange cc)
              => FlowMapSt s cc
              -> (cc, Maybe Flow)
              -> ST.ST s (FlowMapSt s cc)
doIncrReplace st (ccNew, mflowNew) = do
    let g  = fmGraph st
        fm = fmFlowMap st
        fc    = fmFlowCache st
        entry = fmRxEntryNid st
        cnfs  = fmCnfState st
        -- get cc to replace
        ccOld = case C.ccReplacedCc ccNew of
             Nothing -> error "doIncrReplace: ccReplacedNode returned Nothing"
             Just cc -> cc
        -- get nodes created from old cc
        nodes = case M.lookup ccOld (fmCCNodesMap st) of
             Nothing -> error $ "doIncrReplace: did not find old nodes"
                              ++ " for ccOld=" ++ (ppShow ccOld)
             Just ns -> filter (not . PGU.isCnode_ . snd) ns
        -- replace cc in the graph
        (g', replacedNodes, newNodes) = C.icReplace g (ccNew, nodes)
        -- propage potential changes in flow map
    fm' <- foldM (fmUpdateNodeReplace g' fc) fm replacedNodes
    let cnfs'_ = replaceCnfState cnfs
        cnfs' = trN cnfs'_ $  ("ccOld= " ++ (C.ccShow ccOld)) ++ "\n"
                          ++ "OLD CONFIGURATION STATE:\n"
                          ++ (unlines $ map fmCnfStateStr_ cnfs)
                          ++ "NEW CONFIGURATION STATE:\n"
                          ++ (unlines $ map fmCnfStateStr_ cnfs'_)


        replaceCnfState (x:xs) = case x of
            (Just cc', Nothing) ->
                case cc' == ccOld of
                  True  -> (Just (C.ccReplaceNewCc ccNew), mflowNew):xs
                  False -> x:(replaceCnfState xs)
            otherwise ->             x:(replaceCnfState xs)
        replaceCnfState [] = error "doIncrReplace: did not find oldCc in conf state"

    -- update flowmap with new flow
    fm'' <- case mflowNew of
            Just flow -> flowMapAddFlow fc g' entry fm' flow
            Nothing -> return fm'

    return $ st { fmFlowMap = fm''
                , fmGraph = g'
                , fmCnfState = cnfs'
                , fmCCNodesMap =   M.insert ccNew newNodes
                                 $ M.delete ccOld
                                 $ fmCCNodesMap st}

-- update the flow map based on replaced nodes
fmUpdateNodeReplace :: PG.PGraph
                    -> FlowCache s
                    -> FlowMap
                    -> (DGI.Node, (PG.Node, PG.Node))
                    -> ST.ST s FlowMap
fmUpdateNodeReplace g fc fm x@(nid,(oldL,newL)) = ret
    where ret = case M.lookup nid fm of
                Nothing -> return fm -- old flowmap empty, nothing to do!
                Just v  -> fmUpdateNodeReplace_ g fc fm v x

fmUpdateNodeReplace_ :: PG.PGraph
                     -> FlowCache s
                     -> FlowMap
                     -> [(PG.NPort, [Flow])]
                     -> (DGI.Node, (PG.Node, PG.Node))
                     -> ST.ST s FlowMap
fmUpdateNodeReplace_ g fc fm fmVal x@(nid,(oldL,newL)) = ret
    where newFnode = case newL of
                n@(PG.FNode {}) -> n
                _ -> error "fmUpdateNodeReplace: not an F-node"
          ports = PG.nPorts newFnode
          allFlows = L.concat [fls | (p,fls) <- fmVal]
          flowPreds = [(f, flowPred f) | f <- allFlows]
          portPreds = [(p, (PR.portPred_ defBld newFnode) p) | p <- ports]
          fmVal' = mkFlowMapVal portPreds flowPreds
          needUpdate = any (needUpdateP fm) ports
          ret = case needUpdate of
             True -> do
                let xfm1 = foldl (fmDelFlow g nid) fm allFlows
                xfm2 <- foldM (flowMapAddFlow fc g nid) fm allFlows
                return xfm2
             False -> return fm

          needUpdateP fm p = ret
           where flsOld = S.fromList $ case L.lookup p fmVal of
                    Nothing -> error "fmUpdateNodeReplace_: port does not exist"
                    Just x  -> x
                 flsNew = S.fromList $ case L.lookup p fmVal' of
                       Nothing -> error "fmUpdateNodeReplace_: port does not exist"
                       Just x  -> x
                 ret
                    | flsOld == flsNew = False
                    | otherwise = True

          {--
          --updatePort :: FlowMap -> PG.NPort -> ST.ST s FlowMap
          updatePort fm p = fm'
            where
                  flsOld = S.fromList $ case L.lookup p fmVal of
                        Nothing -> error "fmUpdateNodeReplace_: port does not exist"
                        Just x  -> x
                  flsNew = S.fromList $ case L.lookup p fmVal' of
                        Nothing -> error "fmUpdateNodeReplace_: port does not exist"
                        Just x  -> x
                  fm'
                     | flsOld == flsNew = return fm
                     | otherwise = do
                            xfm1 <- foldM (flowMapAddFlow fc g nid) fm toAddFls
                            return die
        --}

-- Add an incremental configuration and an associated flow
doIncrConfigure :: (C.ConfChange cc)
              => FlowMapSt s cc -> (cc, Maybe Flow) -> ST.ST s (FlowMapSt s cc)
doIncrConfigure st (cc, mflow) = do
    let g  = fmGraph st
        fm = fmFlowMap st
        cnfs = fmCnfState st
        flows = catMaybes $ map snd cnfs
        fc    = fmFlowCache st
        entry = fmRxEntryNid st
    -- apply the partial configuration and update the flow map
    let (g', cnfSt) = C.icPartiallyConfigure g cc
        confNodes = PG.csNewNodes cnfSt
        fm' = fmUpdateNewNodes flows fm g' (map fst confNodes)
    -- add flow to the flow map
    fm3 <- case  mflow of
        Just flow -> flowMapAddFlow fc g' entry fm' flow
        Nothing   -> return fm'
    return $ st { fmFlowMap = fm3
                , fmCnfState = (Just cc, mflow):cnfs
                , fmGraph = g'
                , fmCCNodesMap = M.insert cc confNodes (fmCCNodesMap st) }

fmUpdateNewNodesDfs :: DGI.Node -> [Flow] -> FlowMap -> PG.PGraph -> FlowMap
fmUpdateNewNodesDfs entry flows fm g = ret
    where ret = fmUpdateNewNodes flows fm g nids
          --nids_ = [nid | nid <- DGI.nodes g, M.notMember nid fm]
          --nids_ = [nid | nid <- DGI.nodes g, M.notMember nid fm]
          nids_ = [nid | nid <- DFS.dfs [entry] g, M.notMember nid fm]
          nids = trN nids_ $ "fmUpdateNewNodesDfs: NIDS: " ++ (show nids_) ++
                            " LABELS:" ++ (show $ [PG.nLabel $ fromJust $ DGI.lab g n | n <- nids_])

getActivePort :: FlowCache s
              -> PG.PGraph
              -> M.Map DGI.Node PG.NPort
              -> PG.PGNode
              -> Flow
              -> ST.ST s PG.NPort
-- F-node: sink node
getActivePort _ _ _ (_,(PG.FNode { PG.nPorts = [] })) _ = return fmSinkPort
-- F-node (we use the flowcache here)
getActivePort fc _ _ (_,node@(PG.FNode {PG.nPorts = ports})) flow = do
    let key = (PG.nLabel node, flow)
    cachedRet <- H.lookup fc key
    case cachedRet of
        Just x  -> return $ trN x "HIT!"
        Nothing -> do
            H.insert fc key retP
            return $ trN retP "MISS!"
      where fPred    = flowPred flow
            portsSAT = [ (p, pSAT p) | p <- ports]
            -- for assertion
            check    = 1 == (length $ filter snd portsSAT)
            retP     = fst $ fromJust $ L.find snd portsSAT
            -- is port p SAT-isfied for the given flow?
            pSAT p   = isJust $ PR.dnfSAT andExpr
              where andExpr = PR.buildAND defBld [pPred, fPred]
                    pPred   = PR.portPred_ defBld node p

-- O-node: note that by this point, we have computed the mapping from all the
-- previous nodes
getActivePort fc g portmap node@(_, PG.ONode {PG.nOperator = op}) flow =
    return ret
    where ops :: [DGI.Node] -- operands
          (ops, _, _) = PGU.oNodeOperandsMap g node
          opVals = map getOpVal ops
          --
          ret = case val of {True -> "True"; False -> "False"}
          val = case op of
                  PG.NOpAnd -> and opVals
                  PG.NOpOr  -> or  opVals
          -- get operand value for this flow
          getOpVal :: DGI.Node -> Bool
          getOpVal nid = case M.lookup nid portmap of
                           Nothing -> False
                           Just p  -> if      isTruePort  p then True
                                      else if isFalsePort p then False
                                      else error "operand port is not T/F"
getActivePort _ _ _ (_, PG.CNode {}) _ = error "getActiveProt called on C-node"

-- Add a flow to a flow map (a wrapper for flowMapAddFlow_)
flowMapAddFlow :: FlowCache s
               -> PG.PGraph
               -> DGI.Node
               -> FlowMap
               -> Flow
                -> ST.ST s FlowMap
flowMapAddFlow fc g entry fm flow =
    flowMapAddFlow_ fc g M.empty [entry] flow fm

-- Add a flow to a flow map
--  This works by traversing the graph, following nodes that will be visited by
--  the given flow.
--
--  Done map is a map of nodes we have visited with this flow, and the port that
--  the flow activated.
flowMapAddFlow_ :: FlowCache s
                -> PG.PGraph
                -> M.Map DGI.Node PG.NPort -- Done map (nid -> active port)
                -> [DGI.Node]  -- available successors
                -> Flow
                -> FlowMap
                -> ST.ST s FlowMap
flowMapAddFlow_ _ g _       []       fl fm = return $ trN fm m
    where m = "NO REMAINING NODES for flow: " ++ (flowStr fl)
flowMapAddFlow_ fc g doneMap nextNids fl fm = do
    let -- a node is ready when it is not a C-node, and we have visited all its
        -- predecessors
        isReady :: DGI.Node -> Bool
        isReady nid = ret
            where ret_ = notCnode && prevsOK
                  ret = trN ret_ msgR
                  isDone x = M.member x doneMap
                  prevs = PGU.preNE g nid
                  prevsOK = all isDone prevs
                  notCnode = not $ PGU.isCnode_ nlbl
                  nlbl = fromJust $ DGI.lab g nid
                  msgR ="flowMapAddFlow_: node:" ++ (PG.nLabel nlbl)
                        ++ " ready? " ++ (show ret_)
                        -- ++ " prevsOK? " ++ (show prevsOK)
                        -- ++ " PREVS:" ++ (show $ [ PG.nLabel $ fromJust $ DGI.lab g p | p <- prevs])
        --
        nextNotFound = trN fm $ "NO VALID NODES for flow:" ++ (flowStr fl)
    case L.break isReady nextNids of
               (ns1,nid:ns2) -> do
                        let nextN     = (nid, fromJust $ DGI.lab g nid)
                        activeP_ <- getActivePort fc g doneMap nextN fl
                        let activeP   = trN activeP_ pMsg
                            doneMap'  = M.insert nid activeP doneMap
                            sucs      = PGU.sucPortNE g nid activeP
                            fm'       = addFlowPort fm nextN activeP fl
                            nextNids' = ns1 ++ ns2 ++ sucs
                            pMsg = "NEXT for flow: " ++ (flowStr fl)
                                 ++ " "  ++ (PG.nLabel $ snd  nextN)
                                 ++ " P:" ++ activeP_
                        -- recurse
                        flowMapAddFlow_ fc g doneMap' nextNids' fl fm'
               (xnids,[])    -> return nextNotFound



-- remove a flow from the flow map
fmDelFlow :: PG.PGraph -> DGI.Node -> FlowMap -> Flow -> FlowMap
fmDelFlow g entry fm f = fmDelFlow_ fm g [entry] f

-- removes a flow from a flowmap value. Returns the new val and the port that
-- corresponds to the flow
fmValDelFlow :: [(PG.NPort, [Flow])]
             -> Flow
             -> (PG.NPort, [(PG.NPort, [Flow])])
fmValDelFlow ((port,flows):rest) xflow = case xDel xflow flows of
    (flows', True) -> (port, (port,flows'):rest)
    (_, False)     -> (activeP, (port,flows):val')
            where (activeP, val') = fmValDelFlow rest xflow
    where xDel :: Flow -> [Flow] -> ([Flow], Bool)
          xDel _ [] = ([], False)
          xDel xflow (x:xs)
               | x == xflow  = (xs, True)
               | otherwise   = let (xs',ret) = xDel xflow xs in (x:xs',ret)
fmValDelFlow [] _ = error "fmValDelFlow: flow was not found in val"

fmDelFlow_ :: FlowMap -> PG.PGraph -> [DGI.Node] -> Flow -> FlowMap
fmDelFlow_ fm _ [] _ = fm
fmDelFlow_ fm g (nid:nids) flow =
 case M.lookup nid fm of
    Just v  ->  let (p,v') = fmValDelFlow v flow
                    -- insert new value with removed flow
                    alterF Nothing = error "fmDelFlow_: something's wrong..."
                    alterF (Just _) = Just v'
                    fm' = M.alter alterF nid fm
                    -- get successors that will be activated by this port
                    sucs = PGU.sucPortNE g nid p
                    nids' = nids ++ sucs
                in fmDelFlow_ fm' g nids' flow

    Nothing -> fm
    --Nothing -> error $ "fmDelFlow_: mapping does not exist "
    --                ++ "for node:" ++ (PG.nLabel $ fromJust $ DGI.lab g nid)


getRxQMap1 :: forall s cc. (C.ConfChange cc) => FlowMapSt s cc -> ST.ST s QMap
getRxQMap1 st = do
    let g  = fmGraph st
        fm = fmFlowMap st
        flows_ = catMaybes $ map snd $ fmCnfState st
        flows = tr flows_ $ "getRxQMAP: FLOWS: " ++ (show flows_)
        rxQNodes_  = fmRxQueueNodes st
        entry = fmRxEntryNid st
    -- apply the partial configuration and update the flow map
    -- 1. finalize the graph (but do not update to state)
    let cfgEmpty = C.emptyConfig (undefined::cc)
        g' = C.applyConfig cfgEmpty g
        fm' = fmUpdateNewNodesDfs entry flows fm g'
        rxQNodes = trN rxQNodes_ $ "rxQNODES:" ++ (show rxQNodes_)

    -- 2. compute qmap
    let getFlows ::  DGI.Node -> [Flow]
        getFlows nid = case fmGetFlow fm' nid "out" of
                                Left flows -> flows
                                Right msg  -> [] -- error $ "getRxQmap: getFlows failed: " ++ msg
        qmap  = L.concat [ [(f,qid) | f <- flows]
                           | (rxQNid, qid) <- rxQNodes,
                           let flows = getFlows rxQNid]
    return qmap


-- Finding the flowmap nodes from queues, seems to be a bit faster
getRxQMap2 :: forall s cc. (C.ConfChange cc) => FlowMapSt s cc -> ST.ST s QMap
getRxQMap2 st = do
    let g         = fmGraph st
        fm        = fmFlowMap st
        flows_    = catMaybes $ map snd $ fmCnfState st
        flows     = tr flows_ $ "getRxQMAP2: FLOWS: " ++ (show $ length flows_)
        rxQNodes_ = fmRxQueueNodes st
        entry     = fmRxEntryNid st

    --let getQmap :: QueueId -> [Flow]
    -- do a reverse dfs search for nodes that are not in the flow map
    -- (the starting nodes are typically the Rx Queues)
    let fmRDfs :: PG.PGraph -> [DGI.Node] -> [DGI.Node]
        fmRDfs g start = DFS.xdfsWith getNext getResult start g
            where getNext :: PG.PGContext -> [DGI.Node]
                  getNext ctx@(ins, _, _, _) = [inNid | (_, inNid) <- ins,
                                                      not $ M.member inNid fm ]
                  getResult :: PG.PGContext -> DGI.Node
                  getResult ctx@(_, nid, nlbl, _) = nid

    -- apply the partial configuration and update the flow map
    -- 1. finalize the graph (but do not update the state)
    let cfgEmpty = C.emptyConfig (undefined::cc)
        rxQNodes = trN rxQNodes_ $ "rxQNODES:" ++ (show $ length rxQNodes_)
        g' = C.applyConfig cfgEmpty g
        updateNids = fmRDfs g' [ nid | (nid,qid) <- rxQNodes ]
        fm' = fmUpdateNewNodes flows fm g' updateNids


    -- 2. compute qmap
    let getFlows ::  DGI.Node -> [Flow]
        getFlows nid = case fmGetFlow fm' nid "out" of
                                Left flows -> flows
                                Right msg  -> [] -- error $ "getRxQmap: getFlows failed: " ++ msg
        qmap  = L.concat [ [(f,qid) | f <- flows]
                           | (rxQNid, qid) <- rxQNodes,
                           let flows = getFlows rxQNid]
    return $ trN qmap ("RxQMap2: qmap size: " ++ (show $ length qmap))

-- rxQmap2 seems a bit faster
getRxQMap :: forall s cc. (C.ConfChange cc) => FlowMapSt s cc -> ST.ST s QMap
getRxQMap = getRxQMap2
