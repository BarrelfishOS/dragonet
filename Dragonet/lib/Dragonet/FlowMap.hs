{-# LANGUAGE ExistentialQuantification #-}
module Dragonet.FlowMap (
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Configuration       as C
import qualified Dragonet.Predicate           as PR

import Dragonet.Flows (Flow, flowPred)

import qualified Data.Graph.Inductive         as DGI
import qualified Data.Map.Strict              as M
import qualified Data.List                    as L
import qualified Control.Monad.State          as ST

import Data.Maybe
import Control.Exception (assert)

defBld = PR.predBuildDNF

-- As a potential optimization, we might consider using:
-- Data.IntMap and/or Data.Vector.
type FlowMap = M.Map DGI.Node [(PG.NPort, [Flow])] -- flow map

data FlowMapSt cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph      :: PG.PGraph -- graph
    , fmCnfChanges :: [cc]      -- configuration changes already applied
    , fmFlows      :: [Flow]
    , fmFlowMap    :: FlowMap
}

newtype FlowMapM cc a = FlowMapM { unFlowMapM :: ST.State (FlowMapSt cc) a }
    deriving (Monad, ST.MonadState (FlowMapSt cc), Functor)

initFlowMapSt g = FlowMapSt {
      fmGraph      = g
    , fmCnfChanges = []
    , fmFlows      = []
    , fmFlowMap    = M.empty
}

addFlow :: Flow -> (FlowMapM cc) ()
addFlow fl = error "NYI"

initFlowMap :: PG.PGraph -> FlowMap
initFlowMap graph = error "NYI!"

incrConfigure :: (C.ConfChange cc) => cc -> (FlowMapM cc) ()
incrConfigure cc = do
    g <- ST.gets fmGraph
    let (g', nids') = C.icPartiallyConfigure g cc
    -- do a dfs starting from nids', and update all flow maps
    error "NYI!"

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


-- compute the predicate of a node we can compute all the previous predicates
doUpdateFlowMap :: FlowMap -> PG.PGraph -> PG.PGNode -> FlowMap
-- F-node
doUpdateFlowMap fm g node@(nid, fnode@(PG.FNode {})) = fm'
    -- get flow map of previous node
    where ((prevNid,prevNode), prevPort) = PGU.getSinglePrePort g node
          prevPorts = PG.nPorts prevNode
          prevFm = case M.lookup prevNid fm of
                    Just x  -> x
                    Nothing -> err_no_node
          flows = case L.lookup prevPort prevFm of
                    Just x  -> x
                    Nothing -> err_no_port
          flowPreds = [(f, flowPred f) | f <- flows]
          portPreds = [(p, (PR.portPred_ defBld prevNode) p) | p <- prevPorts]

          -- new flow map
          fmVal = mkFlowMapVal portPreds flowPreds
          fm' = M.insertWith err_val_exists nid fmVal fm

          err_no_node = error "doUpdateFlowMap: previous node does not exist in flow map"
          err_no_port = error "doUpdateFlowMap: previous port does not exist in flow map"
          err_val_exists = error "doUpdateFlowMap: Node already exists in flow map"

-- O-node
doUpdateFlowMap fm g (nid, fnode@(PG.ONode {})) = error "NYI!"
