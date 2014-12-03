module Graphs.ImplTransforms (
    mergeSockets,
    coupleTxSockets
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Graph as DGI
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word (Word64)
import Data.Functor ((<$>))
import Data.Maybe

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
                            let msid = PGU.getPGNAttr l "tosocket",
                            isJust msid,
                            let Just sid = msid]

        badFsn = [n | (n,l) <- DGI.labNodes pg,
                      let msid = PGU.getPGNAttr l "fromsocket",
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
    | Just sid <- read <$> PGU.getPGNAttr l "tosocket" =
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
    | Just sid <- read <$> PGU.getPGNAttr l "fromsocket",
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
            PG.nAttrsAdd [PG.NAttrCustom "loadbalance",
                          PG.NAttrCustom $ "appid=" ++ said] $
                PG.baseFNode ("TxBalanceSocket" ++ show sid) []
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

