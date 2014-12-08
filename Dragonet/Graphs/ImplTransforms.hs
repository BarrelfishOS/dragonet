module Graphs.ImplTransforms (
    mergeSockets,
    coupleTxSockets,
    balanceAcrossRxQs,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Graph as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word (Word64)
import Data.Functor ((<$>))
import Data.Maybe

import Debug.Trace (trace)
traceN a b = b

-- At this point (i.e., when transformation functions are called), only the
-- valid RxQueues remain (i.e., the ones that will receive packets). Each of
-- this queue might contain balance nodes that steers packets from the same
-- endpoint to different sockets. Note that these balance nodes (and their
-- sockets) are duplicated across all H/W queues.
--
-- This transformation assigns a single socket of the endpoint balance nodes to
-- each pipeline. Hence, it statically balances endpoints across different
-- pipelines using a socket per pipeline.
--
-- For this to make more sense in the general case, we might want to do that
-- *only* for sockets with the same AppId.
balanceAcrossRxQs :: PG.PGraph -> PG.PGraph
balanceAcrossRxQs g = foldl balEpAcrossRxQs g (M.toList balEpsMap)
    where -- balance nodes
          balEps :: [(Integer, DGI.Node)]
          balEps = [ (eid,nid) |
                     (nid,nlbl) <- DGI.labNodes g,
                     let eid_ = PGU.balanceNodeEndpointId nlbl,
                     isJust eid_,
                     let Just eid = eid_]
          -- map from endpoint id to balance nodes
          balEpsMap :: M.Map Integer [DGI.Node]
          balEpsMap = foldl foldFn M.empty balEps
          foldFn m (eid,nid) = M.alter alterF eid m
            where alterF Nothing        = Just $ [nid]
                  alterF (Just oldnids) = Just $ nid:oldnids

balEpAcrossRxQs :: PG.PGraph -> (Integer, [DGI.Node]) -> PG.PGraph
balEpAcrossRxQs g (eid,nids@(nid0:_)) = doBalEpAcrossRxQs g nids ports
    where ports = PG.nPorts $ fromJust $ DGI.lab g nid0

doBalEpAcrossRxQs :: PG.PGraph -> [DGI.Node] -> [PG.NPort] -> PG.PGraph
-- end: same number of balance nodes (i.e., pipelines) with ports
doBalEpAcrossRxQs g [] [] = g
-- end: more pipelines than ports: remove bal nodes from these pipelines
doBalEpAcrossRxQs g eps [] = DGI.delNodes eps g
-- end: more ports than pipelines: some nodes are going to be ignored
doBalEpAcrossRxQs g [] ps = trace msg g
            where msg = "dobalEpAcrossRxQs: ignoring ports:" ++ (show ps)
-- recurse: use a single port
doBalEpAcrossRxQs g (epNid:eps) (port:ps) = doBalEpAcrossRxQs g' eps ps
   where (prevNid,prevE) = case DGI.lpre g epNid of
             [x] -> x
             []  -> error "balEpAcrossRxQs: 0 predecessor"
             _   -> error "balEpAcrossRxQs: >1 predecessors"
         prevL = fromJust $ DGI.lab g prevNid
         (nextNid,nextE) = case PGU.lsucPortNE g epNid port of
            [x] -> x
            []  -> error $ "balEpAcrossRxQs: port " ++ (show port) ++ " 0 sucs"
            _   -> error $ "balEpAcrossRxQs: port " ++ (show port) ++ " >1 sucs"
         g' = traceN msg
              $ DGI.insEdge (prevNid, nextNid, prevE)
              $ DGI.delNodes delNs g

         delNs = [ nid | nid <- DFS.dfs [epNid] g, nid /= nextNid ]
         msg = ""

-- Remove fromSocket nodes without a matching toSocket node
--
-- In the Embedding phase, all LPG nodes are duplicated.
-- Hence, all socket nodes are replicated in each pipeline.
-- During cleanup, hardware configuration eliminates toSocket nodes based
-- on queue filters.
--
-- For each queue, each toSocket has a corresponding fromSocket node (they have
-- a label {to,from}socket=id with the same id). This function eliminates
-- fromSocket nodes without a matching toSocket node.
coupleTxSockets :: PG.PGraph -> PG.PGraph
coupleTxSockets pg = DGI.delNodes badFsn pg
    where
        -- set of (queueTag, socketId) constructed for all toSocket nodes
        tsn = S.fromList $ [(PG.nTag l,sid) |
                            (_,l) <- DGI.labNodes pg,
                            let msid = PGU.toSocketId l,
                            isJust msid,
                            let Just sid = msid]

        badFsn = [n | (n,l) <- DGI.labNodes pg,
                      let msid = PGU.fromSocketId l,
                      isJust msid,
                      let Just sid = msid,
                      (PG.nTag l,sid) `S.notMember` tsn]

        label n = (PG.nTag l,PG.nLabel l)
            where l = fromJust $ DGI.lab pg n
        l = map label


-- Merge socket nodes so that there is only one instance
mergeSockets :: PG.PGraph -> PG.PGraph
mergeSockets pg = pg'
    where (pg',_,_) = foldl mergeSockNode (pg,M.empty,M.empty) $ DGI.labNodes pg


type MSNCtx = (PG.PGraph,
               -- toSocket nodes map: sid -> Node
               M.Map Word64 DGI.Node,
               -- fromSocket nodes map: sid ->
               --   Left id  -> first occurence (id of first node)
               --   Right id -> second occrence (id of balancer node)
               M.Map Word64 (Either DGI.Node DGI.Node))

mergeSockNode :: MSNCtx -> (DGI.Node,PG.Node) -> MSNCtx
mergeSockNode (pg,mt,mf) (n,l)
    -- toSocket nodes (Rx)
    | Just sid <- read <$> PGU.toSocketId l =
        case M.lookup sid mt of
            -- First occurrence of this ToSocket node:
            --  insert it to the toSocket node map
            Nothing -> (pg, M.insert sid n mt, mf)
            -- Subsequent ToSocket nodes:
            --  have all its predeccessors point to the first occurence
            --  and remove the node
            --  AKK: don't we (in theory) need an OR node here?
            Just sn ->
                let newEs = [(m,sn,el) | (m,el) <- DGI.lpre pg n]
                    pg' = DGI.insEdges newEs $ DGI.delNode n pg
                in (pg', mt, mf)
    -- fromSocket nodes (Tx)
    | Just sid <- read <$> PGU.fromSocketId l,
      Just said <- PGU.getPGNAttr l "appid" =
        case M.lookup sid mf of
            -- First occurrence of this FromSocket node
            Nothing -> (pg, mt, M.insert sid (Left n) mf)
            -- Second occurrence of this FromSocket node: add balance node
            Just (Left sn) ->
                let [bn] = DGI.newNodes 1 pg in
                (DGI.insEdge (sn,bn,PG.Edge "out") $
                    DGI.delNode n $
                    fixFSEdges bn n $ fixFSEdges bn sn $
                    DGI.insNode (bn,balanceNode sid said) pg,
                 mt,
                 M.insert sid (Right bn) mf)
            -- Subsequent FromSocket nodes
            Just (Right bn) -> (DGI.delNode n $ fixFSEdges bn n pg, mt, mf)

    | otherwise = (pg,mt,mf)
    where
        balanceNode sid said =
            PG.nAttrAdd (PG.NAttrCustom $ "appid=" ++ said) $
            PGU.balanceNode ("TxSocket" ++ show sid) []
        -- Move all edges originating at "out" port of fsn to a new port on bn
        fixFSEdges bn fsn g =
            -- Remove existing edges
            GH.delLEdges [(fsn,dn,p) | (dn,p@(PG.Edge "out")) <- sucs] $
                -- Add outgoing edges to balance node on new port
                DGI.insEdges [(bn,dn,PG.Edge port) |
                              (dn,p@(PG.Edge "out")) <- sucs] $
                -- Add new port to balance node
                GH.updateN (\l -> l { PG.nPorts = PG.nPorts l ++ [port] }) bn g
            where
                sucs = DGI.lsuc g fsn
                Just bL = DGI.lab g bn
                port = show $ length $ PG.nPorts bL

