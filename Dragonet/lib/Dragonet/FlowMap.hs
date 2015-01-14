{-# LANGUAGE ExistentialQuantification,GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}
module Dragonet.FlowMap (
    FlowMapSt(..),
    initFlowMapSt,
    getRxQMap,
    addFlow,
    incrConfigure,
    strFM,
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Configuration       as C
import qualified Dragonet.Predicate           as PR

import Dragonet.Flows (Flow, flowPred, flowStr)
import Dragonet.Conventions (isTruePort, isFalsePort, QueueId)

import qualified Data.Graph.Inductive         as DGI
import qualified Data.Map.Strict              as M
import qualified Data.List                    as L
import qualified Data.Set                     as S

import qualified Control.Monad.State.Strict   as SM

-- mutable state hashtable
import qualified Control.Monad.ST as ST
import qualified Data.HashTable.ST.Basic as HB
import qualified Data.HashTable.Class    as H

import Data.Maybe
import Data.Either
import Control.Exception (assert)
import Control.Applicative ((<$>))

import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)
tr a b  = trace b a
trN a b = a

defBld = PR.predBuildDNF

-- As a potential optimization, we might consider using:
-- Data.IntMap and/or Data.Vector.
type FlowMap = M.Map DGI.Node [(PG.NPort, [Flow])] -- flow map

-- convention: some nodes (typically sink nodes) do not have ports, so we add
-- them to the flowMap using sinkPort
fmSinkPort = ""

fmGetFlow :: FlowMap -> DGI.Node -> PG.NPort -> Either [Flow] String
fmGetFlow fm nid port =
    case M.lookup nid fm of
      Just x -> case L.lookup port x of
                  Just y -> Left y
                  Nothing -> Right $ "port:" ++ port ++ " does not exist in flow map"
      Nothing -> Right "node does not exist in flow map"

addFlowPort_ :: [(PG.NPort, [Flow])] -> PG.NPort -> Flow -> [(PG.NPort, [Flow])]
-- port not found (could this happen?)
addFlowPort_ [] port flow = [(port, [flow])]
addFlowPort_ ((p,flows):xs) port flow
    | p == port = (p,(flow:flows)):xs
    | otherwise = (p,flows):(addFlowPort_ xs port flow)

fmAddPortFlow :: FlowMap -> PG.PGNode -> PG.NPort -> Flow -> FlowMap
fmAddPortFlow fm (nid,nlbl) port flow = fm'
    where fm' = M.alter updateF nid fm
          updateF :: Maybe [(PG.NPort, [Flow])] -> Maybe [(PG.NPort, [Flow])]
          updateF (Just portMap) = Just $ addFlowPort_ portMap port flow
          updateF Nothing        = case (nlbl,port) of
                                        (PG.FNode {}, _) -> Just [(port, [flow])]
                                        (PG.ONode {}, "true")  -> Just [("true",  [flow]), ("false", [])]
                                        (PG.ONode {}, "false") -> Just [("false", [flow]), ("true",  [])]
          --updateF Nothing        = error "fmAddPortFlow: node does not exist in flow map"

type FlowCache s = HB.HashTable s (PG.NLabel, Flow) PG.NPort

data FlowMapSt s cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph        :: PG.PGraph -- graph
    , fmCnfChanges   :: [cc]      -- configuration changes already applied
    , fmFlows        :: [Flow]
    , fmFlowMap      :: FlowMap
    , fmRxEntryNid   :: DGI.Node
    , fmTxQueueNodes :: [(QueueId, DGI.Node)]
    , fmRxQueueNodes :: [(QueueId, DGI.Node)]
    , fmFlowCache    :: FlowCache s
}

qmapStr_ :: (Flow,QueueId) -> String
qmapStr_ (f,q) = (flowStr f) ++ "-> Q" ++ (show q)

qmapStr :: [(Flow,QueueId)] -> String
qmapStr flows = "QMAP:\n" ++ L.intercalate "\n" [ "  " ++ qmapStr_ f | f <- flows ]

strFM :: forall s cc. C.ConfChange cc => FlowMapSt s cc -> ST.ST s String
strFM st =  return $
         "STEP : " ++ (show (length $ fmCnfChanges st)) ++ " ------------------------------------------\n"
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

isNidRxNode :: PG.PGraph -> DGI.Node -> Bool
isNidRxNode g nid = isRxNode_ nlbl
    where nlbl = fromJust $ DGI.lab g nid

initFlowMapSt :: (C.ConfChange cc) => PG.PGraph -> ST.ST s (FlowMapSt s cc)
initFlowMapSt g = do
    h <- H.new
    return $ FlowMapSt {
            fmGraph        = g
          , fmCnfChanges   = []
          , fmFlows        = []
          , fmFlowMap      = fm
          , fmRxEntryNid   = rxEntryNid
          , fmTxQueueNodes = txQNs
          , fmRxQueueNodes = rxQNs
          , fmFlowCache    = h
   }
    where fm = initFlowMap [] g -- XXX: This call is not needed since flows are []
          -- this gives us a place to start with a flow
          entryNodes = [ nid | nid <- DGI.nodes g
                             , length (DGI.pre g nid) == 0 ]
          rxEntryNid = case filter (isNidRxNode g) entryNodes of
            [x] -> trN x $ "Entry node: " ++ PG.nLabel (fromJust $ DGI.lab g x)
            []  -> error "initFlowMapSt: no entry node found"
            _   -> error "initFlowMapSt: more than one entry node found"
          --((_,rxEntryNid,_,_),_) =  flowQueueStart
          lNodes = DGI.labNodes g
          txQNs = [(nid,qid) | (nid, nlbl) <- lNodes
                             , let mqid = PGU.nodeTxQueueId nlbl
                             , isJust mqid
                             , let Just qid = mqid]
          rxQNs = [(nid,qid) | (nid, nlbl) <- lNodes
                             , let mqid = PGU.nodeRxQueueId nlbl
                             , isJust mqid
                             , let Just qid = mqid]

addFlow :: (C.ConfChange cc) => FlowMapSt s cc -> Flow -> ST.ST s (FlowMapSt s cc)
addFlow st flow = do
    let g     = fmGraph st
        fm    = fmFlowMap st
        flows = fmFlows st
        entry = fmRxEntryNid st
        fc    = fmFlowCache st
    fm' <- flowMapAddFlow fc fm g entry flow
    return $ st {fmFlowMap = fm', fmFlows = flow:flows}

initFlowMap :: [Flow] -> PG.PGraph -> FlowMap
initFlowMap flows g_ = trN ret "Exit initFlowMap"
    where ret = updateFlowMap flows M.empty g (DGI.nodes g)
          g = trN g_ "Entering initFlowMap"

incrConfigure :: (C.ConfChange cc) => FlowMapSt s cc -> cc -> ST.ST s (FlowMapSt s cc)
incrConfigure st cc = do
    let g  = fmGraph st
        fm = fmFlowMap st
        flows = fmFlows st
        ccs = fmCnfChanges st
    -- apply the partial configuration and update the flow map
    let (g', nids') = C.icPartiallyConfigure g cc
        fm' = updateFlowMap flows fm g' nids'
    return $ st {fmFlowMap = fm', fmCnfChanges = cc:ccs, fmGraph = g'}

updateFlowMapAll :: [Flow] -> FlowMap -> PG.PGraph -> FlowMap
updateFlowMapAll flows fm g = ret
    where ret = updateFlowMap flows fm g nids
          nids = [nid | nid <- DGI.nodes g , M.notMember nid fm]

-- update flow map
-- all predecessors of nodes have valid flowmaps
-- Do we put ONodes into the flowmap? yes
--  A node is either:
--    in the flowmap => we know its predicates and they will not change if we
--    add more configuration changes
--    not in the flowmap => we do not know its predicates
updateFlowMap :: [Flow] -> FlowMap -> PG.PGraph -> [DGI.Node] -> FlowMap
--updateFlowMap [] _ _ _ = fm -- TODO: no flows, no update needed
updateFlowMap flows fm _ [] = fm
updateFlowMap flows fm g nids = trN ret' "updateFlowMap"
    where ret' = assert c1 ret
          ret = case L.break isReady nids of
              -- we found a node: compute new flow map and recurse
              (xs1,xNid:xs2) -> let  xNode = fromJust $ DGI.lab g xNid
                                     fm' = doUpdateFlowMap flows fm g (xNid, xNode)
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
                    rxNode = isRxNode_ nlbl

                    ret_ = notCnode && rxNode && prevsInFlowMap
                    ret = trN ret_ $ "updateFlowMap: is node " ++ (PG.nLabel nlbl) ++ " ready? " ++  (show ret_)

          -- Assertions
          -- check 1: all given nodes are keys in the (old) flowmap
          c1 = all (\nid -> M.notMember nid fm) nids


mkFlowMapVal :: [(PG.NPort, PR.PredExpr)]  -- (port, predicate)
             -> [(Flow, PR.PredExpr)]      -- (flow, predicate)
             -> [(PG.NPort, [Flow])]
mkFlowMapVal ((port,portPred):rest) flowTs = ret
    where ret  = ret0:(mkFlowMapVal rest restFlowTs)
          ret0 = (port, [f | (f,_) <- portFlowTs ])
          -- split flows into two parts: those that SAT the port, and those who
          -- do not
          (portFlowTs,restFlowTs) = L.partition portSAT flowTs
          -- check whether a port satisfies a flow
          portSAT_ (p,pPred) (f,fPred) = isJust $ PR.dnfSAT andExpr
            where andExpr = PR.buildAND defBld [pPred, fPred]
          portSAT = portSAT_ (port, portPred)
          -- return element for the port
mkFlowMapVal [] flowTs = case flowTs of
                          [] -> []
                          _  -> error "mkFlowMapVal: Flows exist that do not match node!"

-- compute the p:redicate of a node we can compute all the previous predicates
doUpdateFlowMap :: [Flow] -> FlowMap -> PG.PGraph -> PG.PGNode -> FlowMap
-- F-node
doUpdateFlowMap allFlows fm g node@(nid, fnode@(PG.FNode {})) = fm'
    -- get flow map of previous node
    where  pres :: [(PG.PGNode, PG.NPort)]
           pres = PGU.getPrePort g node
           flows_ = case pres of
                     -- this is the entry node
                     [] -> allFlows
                     [((prevNid, prevNode), prevPort)] ->
                                    case fmGetFlow fm prevNid prevPort of
                                        Left fs   -> fs
                                        Right msg -> [] -- error $ "doUpdateFlowMap: node:" ++ (PG.nLabel fnode) ++ " previous node:" ++ (PG.nLabel prevNode) ++ " previous port:" ++ prevPort ++ " Error:"   ++ msg
                     otherwise -> error "Fnode with >1 pres"
           flows = trN flows_ $ "FLOWS FOR NODE: " ++ (PG.nLabel fnode) ++  " " ++ (ppShow flows_)
           flowPreds = [(f, flowPred f) | f <- flows]
           portPreds = [(p, (PR.portPred_ defBld fnode) p) | p <- PG.nPorts fnode]
           -- new flow map
           fmVal_ = mkFlowMapVal portPreds flowPreds
           fmVal = trN fmVal_ $ "  NEW FMVAL FOR NODE: " ++ (PG.nLabel fnode) ++ " " ++ (ppShow fmVal_)
           fm' = M.insertWith errExists nid fmVal fm
           errExists = error "doUpdateFlowMap: Node already exists in flow map"
-- O-node
doUpdateFlowMap _ fm g node@(nid, (PG.ONode { PG.nOperator = op})) = fm'
    where fm' = M.insertWith errExists nid fmVal fm
          fmVal_ = [("true", trueFlowsOut), ("false", falseFlowsOut)]
          fmVal  = trN fmVal_ ("ONODE " ++ (PG.nLabel $ snd node) ++ " UPDATE:" ++ (ppShow fmVal_))
          errExists = error "doUpdateFlowMap: Node already exists in flow map"

          (ops, opsTrueMap, opsFalseMap) = PGU.oNodeOperandsMap g node

          getFlows :: M.Map DGI.Node PG.NPort -> DGI.Node -> [Flow]
          getFlows portMap nid = case M.lookup nid portMap of
                Nothing    -> [] -- operand is not connected
                Just port  -> case fmGetFlow fm nid port of
                        Left x    -> x
                        -- XXX: we probably need to fix this:
                        --Right msg -> error $ "doUpdateFlowMap (onode): " ++ msg
                        Right msg -> []

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

getActivePort :: FlowCache s
              -> PG.PGraph
              -> M.Map DGI.Node PG.NPort
              -> PG.PGNode
              -> Flow
              -> ST.ST s PG.NPort
-- F-node: sink node
getActivePort _ _ _ (_,(PG.FNode { PG.nPorts = [] })) _ = return fmSinkPort
-- F-node
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

getActivePort _ _ _ (_, PG.CNode {}) _ = error "getActiveProt called for a C-node"

flowMapAddFlow fc fm g entry flow = flowMapAddFlow_ fc g M.empty [entry] flow fm

flowMapAddFlow_ :: FlowCache s
                -> PG.PGraph
                -> M.Map DGI.Node PG.NPort -- done nodes (nid -> active port)
                -> [DGI.Node]  -- available successors
                -> Flow
                -> FlowMap
                -> ST.ST s FlowMap
flowMapAddFlow_ _ g _       []       fl fm = return $ trN fm  $ "NO REMAINING NODES for flow: " ++ (flowStr fl)
flowMapAddFlow_ fc g doneMap nextNids fl fm = do
    let -- a node is ready when all is predecessors are done
        isReady :: DGI.Node -> Bool
        isReady nid = ret
            where ret_ = notCnode && prevsOK
                  ret = trN ret_ $ "flowMapAddFlow_: node:" ++ (PG.nLabel nlbl) ++ " ready? " ++ (show ret_)
                  isDone x = M.member x doneMap
                  prevsOK = all isDone (PGU.preNE g nid)
                  notCnode = not $ PGU.isCnode_ nlbl
                  nlbl = fromJust $ DGI.lab g nid
        --
        nextNotFound = trN fm $ "NO VALID NODES for flow:" ++ (flowStr fl) -- error "flowMapAddFlow_: Could not reach the end for flow"

    case L.break isReady nextNids of
               (ns1,nid:ns2) -> do

                        let nextN     = (nid, fromJust $ DGI.lab g nid)

                        activeP_ <- getActivePort fc g doneMap nextN fl

                        let activeP   = trN activeP_ $ "NEXT for flow: " ++ (flowStr fl) ++ " "  ++ (PG.nLabel $ snd  nextN) ++ " P:" ++ activeP_
                            doneMap'  = M.insert nid activeP doneMap
                            sucs      = PGU.sucPortNE g nid activeP
                            fm'       = fmAddPortFlow fm nextN activeP fl
                            nextNids' = ns1 ++ ns2 ++ sucs
                        flowMapAddFlow_ fc g doneMap' nextNids' fl fm'

               (xnids,[])    -> return nextNotFound

-- just copy them for now. TODO: put them in a single file (e.g., Cost.hs)
type QMap        = [(Flow, QueueId)]

getRxQMap :: forall s cc. (C.ConfChange cc) => FlowMapSt s cc -> ST.ST s QMap
getRxQMap st = do
    let g  = fmGraph st
        fm = fmFlowMap st
        flows_ = fmFlows st
        flows = tr flows_ $ "getRxQMAP: FLOWS: " ++ (show flows_)
        rxQNodes_  = fmRxQueueNodes st
    -- apply the partial configuration and update the flow map
    -- 1. finalize the graph (but do not update to state)
    let cfgEmpty = C.emptyConfig (undefined::cc)
        g' = C.applyConfig cfgEmpty g
        fm' = updateFlowMapAll flows fm g'
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
