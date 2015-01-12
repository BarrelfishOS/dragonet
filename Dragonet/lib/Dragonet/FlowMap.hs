{-# LANGUAGE ExistentialQuantification #-}
module Dragonet.FlowMap (
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Configuration       as C
import qualified Dragonet.Predicate           as PR

import Dragonet.Flows (Flow, flowPred)
import Dragonet.Conventions (isTruePort, isFalsePort)

import qualified Data.Graph.Inductive         as DGI
import qualified Data.Map.Strict              as M
import qualified Data.List                    as L
import qualified Data.Set                     as S
import qualified Control.Monad.State          as ST

import Data.Maybe
import Data.Either
import Control.Exception (assert)

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
                  Nothing -> Right "Port does not exist in flow map"
      Nothing -> Right "Node does not exist in flow map"

addFlowPort_ :: [(PG.NPort, [Flow])] -> PG.NPort -> Flow -> [(PG.NPort, [Flow])]
-- port not found (could this happen?)
addFlowPort_ [] port flow = [(port, [flow])]
addFlowPort_ ((p,flows):xs) port flow
    | p == port = (p,(flow:flows)):xs
    | otherwise = (p,flows):(addFlowPort_ xs port flow)

fmAddPortFlow :: FlowMap -> DGI.Node -> PG.NPort -> Flow -> FlowMap
fmAddPortFlow fm nid port flow = fm'
    where fm' = M.alter updateF nid fm
          updateF :: Maybe [(PG.NPort, [Flow])] -> Maybe [(PG.NPort, [Flow])]
          updateF (Just portMap) = Just $ addFlowPort_ portMap port flow
          updateF Nothing        = error "fmAddPortFlow: node does not exist in flow map"


data FlowMapSt cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph      :: PG.PGraph -- graph
    , fmCnfChanges :: [cc]      -- configuration changes already applied
    , fmFlows      :: [Flow]
    , fmFlowMap    :: FlowMap
    , fmEntryNid   :: DGI.Node
}

newtype FlowMapM cc a = FlowMapM { unFlowMapM :: ST.State (FlowMapSt cc) a }
    deriving (Monad, ST.MonadState (FlowMapSt cc), Functor)

initFlowMapSt g = FlowMapSt {
      fmGraph      = g
    , fmCnfChanges = []
    , fmFlows      = []
    , fmFlowMap    = fm
    , fmEntryNid   = entryNid
}
    where fm = initFlowMap g
          -- this gives us a place to start with a flow
          entryNid = case S.toList $ PGU.entryNodes g of
            [x] -> x
            []  -> error "initFlowMapSt: no entry node found"
            _   -> error "initFlowMapSt: more than one entry node found"

addFlow :: Flow -> (FlowMapM cc) ()
addFlow flow = do
    g     <- ST.gets fmGraph
    fm    <- ST.gets fmFlowMap
    flows <- ST.gets fmFlows
    entry <- ST.gets fmEntryNid
    let fm' = flowMapAddFlow fm g entry flow
    ST.modify $ \s -> s {fmFlowMap = fm', fmFlows = flow:flows}

initFlowMap :: PG.PGraph -> FlowMap
initFlowMap g = updateFlowMap M.empty g (DGI.nodes g)

incrConfigure :: (C.ConfChange cc) => cc -> (FlowMapM cc) ()
incrConfigure cc = do
    g  <- ST.gets fmGraph
    fm <- ST.gets fmFlowMap
    ccs <- ST.gets fmCnfChanges
    -- apply the partial configuration and update the flow map
    let (g', nids') = C.icPartiallyConfigure g cc
        fm' = updateFlowMap fm g' nids'
    ST.modify $ \s -> s {fmFlowMap = fm', fmCnfChanges = cc:ccs}

-- update flow map
-- all predecessors of nodes have valid flowmaps
-- Do we put ONodes into the flowmap? yes
--  A node is either:
--    in the flowmap => we know its predicates and they will not change if we
--    add more configuration changes
--    not in the flowmap => we do not know its predicates
updateFlowMap :: FlowMap -> PG.PGraph -> [DGI.Node] -> FlowMap
updateFlowMap fm _ [] = fm
updateFlowMap fm g nids = ret'
    where ret' = assert c1 ret
          ret = case L.break isReady nids of
              -- we found a node: compute new flow map and recurse
              (xs1,xNid:xs2) -> let  xNode = fromJust $ DGI.lab g xNid
                                     fm' = doUpdateFlowMap fm g (xNid, xNode)
                                 in updateFlowMap fm' g (xs1 ++ xs2)
              -- no node found
              (before,[]) -> fm

          -- check if a node is ready to be inserted into the flowmap:
          --  . it must not be a cnode
          --  . all of its predecessors must be in the flowmap
          isReady :: DGI.Node -> Bool
          isReady nid = notCnode && prevsInFlowMap
              where nidPrevs = PGU.preNE g nid
                    prevsInFlowMap =  all (\x -> M.member x fm) nidPrevs
                    notCnode = PGU.isCnode_ $ fromJust $ DGI.lab g nid

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
doUpdateFlowMap :: FlowMap -> PG.PGraph -> PG.PGNode -> FlowMap
-- F-node
doUpdateFlowMap fm g node@(nid, fnode@(PG.FNode {})) = fm'
    -- get flow map of previous node
    where ((prevNid,prevNode), prevPort) = PGU.getSinglePrePort g node
          prevPorts = PG.nPorts prevNode

          flows = case fmGetFlow fm prevNid prevPort of
                    Left fs   -> fs
                    Right msg -> error $ "doubleFlowMap: previous node:" ++ msg

          flowPreds = [(f, flowPred f) | f <- flows]
          portPreds = [(p, (PR.portPred_ defBld prevNode) p) | p <- prevPorts]

          -- new flow map
          fmVal = mkFlowMapVal portPreds flowPreds
          fm' = M.insertWith errExists nid fmVal fm

          errExists = error "doUpdateFlowMap: Node already exists in flow map"
-- O-node
doUpdateFlowMap fm g node@(nid, (PG.ONode { PG.nOperator = op})) = fm'
    where fm' = M.insertWith errExists nid fmVal fm
          fmVal = [("True", trueFlowsOut), ("False", falseFlowsOut)]
          errExists = error "doUpdateFlowMap: Node already exists in flow map"

          (ops, opsTrueMap, opsFalseMap) = PGU.oNodeOperandsMap g node

          getFlows :: M.Map DGI.Node PG.NPort -> DGI.Node -> [Flow]
          getFlows portMap nid = case M.lookup nid portMap of
                Nothing    -> [] -- operand is not connected
                Just port  -> case fmGetFlow fm nid port of
                        Left x    -> x
                        Right msg -> error $ "doUpdateFlowMap (onode):" ++ msg

          trueFlowsIn :: [S.Set Flow]
          trueFlowsIn  = [S.fromList $ getFlows opsTrueMap nid  | nid <- ops]
          falseFlowsIn = [S.fromList $ getFlows opsFalseMap nid | nid <- ops]


          -- S.unions is defined as L.foldl S.union S.empty
          -- not sure why intersections is not defined
          --intersections [] = []
          intersections xs = L.foldl S.intersection (head xs) (tail xs)

          trueFlowsOut :: [Flow]
          trueFlowsOut = S.toList $ case op of
                            PG.NOpOr  -> S.unions trueFlowsIn
                            PG.NOpAnd -> intersections trueFlowsIn
          falseFlowsOut = S.toList $ case op of
                            PG.NOpOr  -> intersections falseFlowsIn
                            PG.NOpAnd -> S.unions falseFlowsIn


getActivePort :: PG.PGraph
              -> M.Map DGI.Node PG.NPort
              -> PG.PGNode
              -> Flow
              -> PG.NPort
-- F-node: sink node
getActivePort _ _ (_,(PG.FNode { PG.nPorts = [] })) _ = fmSinkPort
-- F-node
getActivePort _ _ (_,node@(PG.FNode {PG.nPorts = ports})) flow= assert check retP
    where fPred    = flowPred flow
          portsSAT = [ (p, pSAT p) | p <- ports]
          check    = 1 == (length $ filter snd portsSAT)
          retP     = fst $ fromJust $ L.find snd portsSAT

          -- is port p SAT-isfied for the given flow?
          pSAT p   = isJust $ PR.dnfSAT andExpr
            where andExpr = PR.buildAND defBld [pPred, fPred]
                  pPred   = PR.portPred_ defBld node p

-- O-node: note that by this point, we have computed the mapping from all the
-- previous nodes
getActivePort g portmap node@(_, PG.ONode {PG.nOperator = op}) flow = ret
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

flowMapAddFlow fm g entry flow = flowMapAddFlow_ g M.empty [entry] flow fm

flowMapAddFlow_ :: PG.PGraph
               -> M.Map DGI.Node PG.NPort -- done nodes (nid -> active port)
               -> [DGI.Node]  -- available successors
               -> Flow
               -> FlowMap
               -> FlowMap
flowMapAddFlow_ g _       []       fl fm = fm
flowMapAddFlow_ g doneMap nextNids fl fm = ret
 where ret = case L.break isReady nextNids of
               (ns1,nid:ns2) -> let nextNids' = ns1 ++ ns2 ++ sucs
                                    xnode     = (nid, fromJust $ DGI.lab g nid)
                                    activeP   = getActivePort g doneMap xnode fl
                                    doneMap'  = M.insert nid activeP doneMap
                                    sucs      = PGU.sucPortNE g nid activeP
                                    fm'       = fmAddPortFlow fm nid activeP fl
                                 in flowMapAddFlow_ g doneMap' nextNids' fl fm'
               (xnids,[])    -> nextNotFound
       -- a node is ready when all is predecessors are done
       isReady :: DGI.Node -> Bool
       isReady nid = all isDone (PGU.preNE g nid)
           where isDone x = M.member x doneMap
       --
       nextNotFound = error "flowMapAddFlow_: Could not reach the end for flow"
