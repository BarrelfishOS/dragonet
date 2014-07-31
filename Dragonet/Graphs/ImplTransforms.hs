module Graphs.ImplTransforms (
    mergeSockets
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Graph as DGI
import qualified Data.Map as M
import Data.Word (Word64)
import Data.Functor ((<$>))

type MSNCtx = (PG.PGraph,
                    M.Map Word64 DGI.Node,
                    M.Map Word64 (Either DGI.Node DGI.Node))

mergeSockNode :: MSNCtx -> (DGI.Node,PG.Node) -> MSNCtx
mergeSockNode (pg,mt,mf) (n,l)
    | Just sid <- read <$> PGU.getPGNAttr l "tosocket" =
        case M.lookup sid mt of
            -- First occurrence of this ToSocket node
            Nothing -> (pg, M.insert sid n mt, mf)
            -- Subsequent ToSocket nodes
            Just sn ->
                (DGI.insEdges [(m,sn,el) |
                               (m,el) <- DGI.lpre pg n] $ DGI.delNode n pg,
                 mt, mf)
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
        bn = DGI.newNodes 1 pg
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

-- Merge socket nodes so that there is only one instance
mergeSockets :: PG.PGraph -> PG.PGraph
mergeSockets pg = pg'
    where (pg',_,_) = foldl mergeSockNode (pg,M.empty,M.empty) $ DGI.labNodes pg


