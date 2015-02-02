{-# LANGUAGE ExistentialQuantification,GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}
module Dragonet.FlowMap (
    FlowMapSt(..),
    initFlowMapSt,
    getRxQMap,
    addFlow,
    incrConfigure,
    rmFlowConf,
    rebuildFlowMapSt,
    strFM,
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

-- A Flowmap maps each node's port to a set of flows
--
-- As a potential optimization, we might consider using:
-- Data.IntMap and/or Data.Vector for this
type FlowMap = M.Map DGI.Node [(PG.NPort, [Flow])] -- flow map

-- Flow map state.
-- This essentially represents a solution: it includes a list of configuration
-- changes and the registered flows.
type FlowCache s = HB.HashTable s (PG.NLabel, Flow) PG.NPort
data FlowMapSt s cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph        :: PG.PGraph -- graph (gets updated with every conf)
    , fmOrigGraph    :: PG.PGraph -- fully unconfigured PRG
    , fmCnfChanges   :: [cc]      -- configuration changes applied
    , fmFlows        :: [Flow]
    , fmFlowMap      :: FlowMap
    , fmRxEntryNid   :: DGI.Node
    , fmTxQueueNodes :: [(QueueId, DGI.Node)]
    , fmRxQueueNodes :: [(QueueId, DGI.Node)]
    , fmFlowCache    :: FlowCache s
}

-- convention: some nodes (typically sink nodes) do not have ports, so we add
-- them to the flowMap using sinkPort
fmSinkPort = ""

--
-- FlowMap helpers
--

-- get the set of flows for a (node,port) or an error string
fmGetFlow :: FlowMap -> DGI.Node -> PG.NPort -> Either [Flow] String
fmGetFlow fm nid port =
    case M.lookup nid fm of
      Just x -> case L.lookup port x of
                  Just y  -> Left y
                  Nothing -> Right noPortMsg
      Nothing -> Right noNodeMsg
    where noNodeMsg = "node does not exist in flow map"
          noPortMsg = "port:" ++ port ++ " does not exist in flow map"

-- helper for addFlowPort_
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
updateFlowMap :: [Flow] -> FlowMap -> PG.PGraph -> [DGI.Node] -> FlowMap
--updateFlowMap [] _ _ _ = fm -- TODO: no flows, no update needed
updateFlowMap flows fm _ [] = fm
updateFlowMap flows fm g nids = trN ret "updateFlowMap"
    where ret' = assert c1 ret
          ret = case L.break isReady nids of
              -- we found a node: compute new flow map and recurse
              (xs1,xNid:xs2) ->
                    let xNode = fromJust $ DGI.lab g xNid
                        fm'   = doUpdateFlowMap flows fm g (xNid, xNode)
                     in updateFlowMap flows fm' g (xs1 ++ xs2)
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
                    rdMsg = "updateFlowMap: is node " ++ (PG.nLabel nlbl)
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
initFlowMap flows g = updateFlowMap flows M.empty g (DGI.nodes g)

--
-- FlowMapSt functions
--

resetFlowMapSt :: (C.ConfChange cc)
               => (FlowMapSt s cc) -> ST.ST s (FlowMapSt s cc)
resetFlowMapSt st = do
    return $ st {
            fmGraph        = g
          , fmCnfChanges   = []
          , fmFlows        = []
          , fmFlowMap      = initFlowMap [] g
   }
   where g = fmOrigGraph st

-- rebuild flowmap state using a new sate of flows and ccs
rebuildFlowMapSt :: (C.ConfChange cc)
                 => (FlowMapSt s cc) -> [Flow] -> [cc]
                 -> ST.ST s (FlowMapSt s cc)
rebuildFlowMapSt st flows ccs = do
    st1 <- resetFlowMapSt st
    --st2 <- foldM incrConfigure st1 ccs
    --st3 <- foldM addFlow st2 flows
    -- this seems to be faster for some reason
    st2 <- foldM addFlow st1 flows
    st3 <- foldM incrConfigure st2 ccs
    return st3

-- intitialize flow map state
initFlowMapSt :: (C.ConfChange cc) => PG.PGraph -> ST.ST s (FlowMapSt s cc)
initFlowMapSt g = do
    h <- H.new
    return $ FlowMapSt {
            fmGraph        = g
          , fmOrigGraph    = g
          , fmCnfChanges   = []
          , fmFlows        = []
          , fmFlowMap      = fm
          , fmRxEntryNid   = rxEntryNid
          , fmTxQueueNodes = txQNs
          , fmRxQueueNodes = rxQNs
          , fmFlowCache    = h
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

-- Add a flow: this will
addFlow :: (C.ConfChange cc)
        => FlowMapSt s cc -> Flow -> ST.ST s (FlowMapSt s cc)
addFlow st flow = do
    let g     = fmGraph st
        fm    = fmFlowMap st
        flows = fmFlows st
        entry = fmRxEntryNid st
        fc    = fmFlowCache st
    fm' <- flowMapAddFlow fc fm g entry flow
    return $ st {fmFlowMap = fm', fmFlows = flow:flows}

-- Removing a flow: Each cc in fmCnfChanges has a corresponding flow
-- There are two ways to remove a flow
--  - we can remove a flow from .fmFlows, but keep the configuration
--    around
--  - we can remove the flow *and* the corresponding configuration change. In
--  this case we need to recompute the flowmap.
--  We do the latter here by rebuilding the flowmap
rmFlowConf :: forall s cc. (C.ConfChange cc)
           => FlowMapSt s cc -> Flow -> ST.ST s (FlowMapSt s cc)
rmFlowConf st flow = do
    let ts = L.zip (fmCnfChanges st) (fmFlows st)
        (xt,ts') = case L.break ((==flow) . snd) ts of
            (_,[])      -> error "rmFLowConf: flow not found"
            (hs,(x:xs)) -> (x, hs++xs)
        flows' = map snd ts'
        ccs'   = map fst ts'
    -- drop all configuration and flow map
    rebuildFlowMapSt st flows' ccs'

qmapStr_ :: (Flow,QueueId) -> String
qmapStr_ (f,q) = (flowStr f) ++ "-> Q" ++ (show q)

qmapStr :: [(Flow,QueueId)] -> String
qmapStr flows = "QMAP:\n"
              ++ L.intercalate "\n" [ "  " ++ qmapStr_ f | f <- flows ]

strFM :: forall s cc. C.ConfChange cc => FlowMapSt s cc -> ST.ST s String
strFM st =  return $
         "STEP : " ++ (show $ length $ fmCnfChanges st)
         ++ " ------------------------------------------\n"
         ++ "CNF  : " ++ cnfStr ++ "\n"
         ++ "FLOWS: " ++ flsStr ++ "\n"
         -- ++ "QMAP :"  ++ (qmapStr qmap) ++ "\n"
         ++ "FMAP SIZE :"  ++ (show $ length fmapL) ++ "\n"
         ++ "FMAP :"  ++ fmapS ++ "\n"
    where cnf    = C.foldConfChanges (fmCnfChanges st)
          cnfStr = C.showConfig (undefined::cc) cnf
          flsStr = L.intercalate "\n" [" " ++ (flowStr f) | f <- (fmFlows st)]
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

incrConfigure :: (C.ConfChange cc)
              => FlowMapSt s cc -> cc -> ST.ST s (FlowMapSt s cc)
incrConfigure st cc = do
    let g  = fmGraph st
        fm = fmFlowMap st
        flows = fmFlows st
        ccs = fmCnfChanges st
    -- apply the partial configuration and update the flow map
    let (g', nids') = C.icPartiallyConfigure g cc
        fm' = updateFlowMap flows fm g' nids'
    return $ st {fmFlowMap = fm', fmCnfChanges = cc:ccs, fmGraph = g'}

updateFlowMapDfs :: DGI.Node -> [Flow] -> FlowMap -> PG.PGraph -> FlowMap
updateFlowMapDfs entry flows fm g = ret
    where ret = updateFlowMap flows fm g nids
          --nids_ = [nid | nid <- DGI.nodes g, M.notMember nid fm]
          --nids_ = [nid | nid <- DGI.nodes g, M.notMember nid fm]
          nids_ = [nid | nid <- DFS.dfs [entry] g, M.notMember nid fm]
          nids = trN nids_ $ "updateFlowMapDfs: NIDS: " ++ (show nids_) ++
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
               -> FlowMap
               -> PG.PGraph
               -> DGI.Node
               -> Flow
                -> ST.ST s FlowMap
flowMapAddFlow fc fm g entry flow =
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
flowMapDelFlow :: FlowMap -> PG.PGraph -> DGI.Node -> Flow -> FlowMap
flowMapDelFlow fm g entry f = flowMapDelFlow_ fm g [entry] f

-- removes a flow from a flowmap value. Returns the new val and the port that
-- corresponds to the flow
flowMapValDelFlow :: [(PG.NPort, [Flow])]
                  -> Flow
                  -> (PG.NPort, [(PG.NPort, [Flow])])
flowMapValDelFlow ((port,flows):rest) rmFlow = case xDel rmFlow flows of
    (flows', True) -> (port, (port,flows'):rest)
    (_, False)     -> (activeP, (port,flows):val')
            where (activeP, val') = flowMapValDelFlow rest rmFlow
    where xDel :: Flow -> [Flow] -> ([Flow], Bool)
          xDel _ [] = ([], False)
          xDel xflow (x:xs)
               | x == xflow  = (xs, True)
               | otherwise   = let (xs',ret) = xDel xflow xs in (x:xs',ret)
flowMapValDelFlow [] _ = error "flowMapValDelFlow: flow was not found in val"

flowMapDelFlow_ :: FlowMap -> PG.PGraph -> [DGI.Node] -> Flow -> FlowMap
flowMapDelFlow_ fm _ [] _ = fm
flowMapDelFlow_ fm g (nid:nids) flow = ret
    where (p, v') = case M.lookup nid fm of
                      Just v  -> flowMapValDelFlow v flow
                      Nothing -> error "flowMapDelFlow_: mapping does not exist"
          -- get successors that will be activated by this port
          sucs = PGU.sucPortNE g nid p
          nids' = nids ++ sucs
          -- insert new value with removed flow
          alterF Nothing   = error "flowMapDelFlow_: something's wrong..."
          alterF (Just _)  =  Just v'
          fm' = M.alter alterF nid fm
          ret = flowMapDelFlow_ fm' g nids' flow

-- just copy them for now. TODO: put them in a single file (e.g., Cost.hs)
type QMap        = [(Flow, QueueId)]

getRxQMap1 :: forall s cc. (C.ConfChange cc) => FlowMapSt s cc -> ST.ST s QMap
getRxQMap1 st = do
    let g  = fmGraph st
        fm = fmFlowMap st
        flows_ = fmFlows st
        flows = tr flows_ $ "getRxQMAP: FLOWS: " ++ (show flows_)
        rxQNodes_  = fmRxQueueNodes st
        entry = fmRxEntryNid st
    -- apply the partial configuration and update the flow map
    -- 1. finalize the graph (but do not update to state)
    let cfgEmpty = C.emptyConfig (undefined::cc)
        g' = C.applyConfig cfgEmpty g
        fm' = updateFlowMapDfs entry flows fm g'
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
        flows_    = fmFlows st
        flows     = tr flows_ $ "getRxQMAP2: FLOWS: " ++ (show flows_)
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
        rxQNodes = trN rxQNodes_ $ "rxQNODES:" ++ (show rxQNodes_)
        g' = C.applyConfig cfgEmpty g
        updateNids = fmRDfs g' [ nid | (nid,qid) <- rxQNodes ]
        fm' = updateFlowMap flows fm g' updateNids


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
