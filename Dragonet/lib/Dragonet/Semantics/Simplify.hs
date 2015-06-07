-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE OverloadedStrings #-}
module Dragonet.Semantics.Simplify (
    reducePG
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Semantics.Solver as S
import qualified Dragonet.Semantics.HsmtlibSolver as HS
import qualified Dragonet.Semantics.Z3Solver as Z3S
import qualified Dragonet.Semantics.Cache as CS

import qualified Util.GraphHelpers as GH

import Control.Monad (foldM,forM_)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Functor ((<$>))
import qualified Data.Graph.Inductive as DGI
import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTB
import qualified Hsmtlib as H
import qualified Hsmtlib.Solver as HS
import qualified Hsmtlib.Parsers.Syntax as HPS

import qualified Control.Concurrent.STM.TVar as TV
import qualified Control.Monad.STM as STM
import Control.Monad.IO.Class (liftIO)

import Debug.Trace (trace)

traceN x y = y

addIn :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
addIn k v = M.insertWith (++) k [v]

lookupAll :: Eq a => a -> [(a,b)] -> [(a,b)]
lookupAll a as = filter ((== a) . fst) as

type EMap s = M.Map DGI.Node [(Maybe (DGI.Node,String),s)]



processNode :: (Show e, S.SolverM m e) =>
        (PG.PGraph,EMap e,S.Set DGI.Node) -> DGI.Node ->
                m (PG.PGraph,EMap e,S.Set DGI.Node)
processNode  st@(g,eM,_) n
    | not ready = do
        return st -- we'll try again later
    | PG.FNode { PG.nPorts = ports,
                 PG.nSemantics = sems,
                 PG.nLabel = name} <- nL = do
        -- Get input semantics
        inE <- S.exprOrL [s | (_,s) <- ins]
        let portAdd st' p = do
                -- Build expression for this port
                e <- case lookup p sems of
                    Just sem -> do
                        e <- S.parseSem sem
                        inE `S.exprAnd` e
                    Nothing -> return inE
                applyPortSems st' p e
        -- Process all ports
        foldM portAdd st ports
    | PG.ONode { PG.nPorts = ports,
                 PG.nOperator = op } <- nL = do
        true <- S.exprTrue
        false <- S.exprFalse
        -- Get ports and actions to combine exprs for sems
        let pas = case op of
                PG.NOpAnd ->  [("true",S.exprAndL),
                               ("false",S.exprOrL)]
                PG.NOpNAnd -> [("false",S.exprOrL),
                               ("true",S.exprAndL)]
                PG.NOpOr ->   [("true",S.exprOrL),
                               ("false",S.exprAndL)]
                PG.NOpNOr ->  [("false",S.exprAndL),
                               ("true",S.exprOrL)]
        let portAdd st' (p,a) = do
                -- combine inputs
                let l = [s | (Just (_,p') ,s) <- ins, p' == p]
                ex <- case l of
                        [] -> return false
                        es -> a es
                applyPortSems st' p ex
        foldM portAdd st pas
    where
        Just nL = DGI.lab g n
        ins = fromMaybe [] $ M.lookup n eM
        -- Check if incoming edge is in ins
        er (n,PG.Edge { PG.ePort = p }) = isJust $ lookup (Just (n,p)) ins
        er _ = True
        -- Check if all edges are in ins
        ready = all er $ DGI.lpre g n

        -- Apply semantic expression for port
        --   If unsatisfiable this will remove edges
        applyPortSems (g',eM',next) p e = do
            false <- S.exprFalse
            -- Check expression for satisfiability
            sat <- S.checkSat e
            let msg = "-------| Node=" ++ (PG.nLabel nL)
                      ++ " Tag=" ++ (PG.nTag nL)
                      ++ " Port=" ++ p
                      ++ " Sat=" ++ (show sat)
            let outEs = [(n,n',edg) |
                         (n',edg@PG.Edge { PG.ePort = ep }) <- DGI.lsuc g n,
                         ep == p]
            -- Remove edges if unsatisfiable and simplify fw expr to false
            (fwe,g'') <- case traceN msg sat of
                S.Unsatisfiable -> return (false, GH.delLEdges outEs g')
                _ -> return (e, g')
            -- Add forward expressions
            let addFWE (m,nx) (_,d,edg) =
                    (addIn d (Just (n, PG.ePort edg), fwe) m,
                     S.insert d nx)
                (eM'',next') = foldl addFWE (eM',next) outEs
            return (g'',eM'',next')

processNodes :: (Show e, S.SolverM m e) =>
        (PG.PGraph,EMap e,S.Set DGI.Node) ->
                m (PG.PGraph,EMap e,S.Set DGI.Node)
processNodes st@(pg,eM,ns)
    | Just (n,ns') <- S.minView ns = do
            let Just nL = DGI.lab pg n
            st' <- processNode (pg,eM,ns') n
            processNodes st'
    | otherwise = return st



builtins = SMT.Script [
    SMT.CmdDeclareType "Packet" 0,
    SMT.CmdDefineType  "POffset" [] (SMTB.tBitVec 16),
    --
    SMT.CmdDeclareType "L2Proto" 0,
    SMT.CmdDeclareFun "L2P.Ethernet" [] (t "L2Proto"),
    --
    SMT.CmdDeclareType "L3Proto" 0,
    SMT.CmdDeclareFun "L3P.IP4" [] (t "L3Proto"),
    SMT.CmdDeclareFun "L3P.IP6" [] (t "L3Proto"),
    SMT.CmdDeclareFun "L3P.ARP" [] (t "L3Proto"),
    SMT.CmdAssert $ v "L3P.IP4" SMTC.=/= v "L3P.IP6",
    SMT.CmdAssert $ v "L3P.IP4" SMTC.=/= v "L3P.ARP",
    SMT.CmdAssert $ v "L3P.IP6" SMTC.=/= v "L3P.ARP",
    --
    SMT.CmdDeclareType "L4Proto" 0,
    SMT.CmdDeclareFun "L4P.UDP" [] (t "L4Proto"),
    SMT.CmdDeclareFun "L4P.TCP" [] (t "L4Proto"),
    SMT.CmdDeclareFun "L4P.ICMP" [] (t "L4Proto"),
    SMT.CmdAssert $ v "L4P.UDP" SMTC.=/= v "L4P.TCP",
    SMT.CmdAssert $ v "L4P.UDP" SMTC.=/= v "L4P.ICMP",
    SMT.CmdAssert $ v "L4P.TCP" SMTC.=/= v "L4P.ICMP",
    --
    SMT.CmdDeclareFun  "plen" [t "Packet"] (SMTB.tBitVec 16),
    SMT.CmdDeclareFun  "pget" [t "Packet", SMTB.tBitVec 16] (SMTB.tBitVec 8),
    SMT.CmdDeclareFun  "pkt" [] (t "Packet"),
    SMT.CmdDeclareFun  "L2.Proto" [t "Packet"] (t "L2Proto"),
    SMT.CmdDeclareFun  "L3.Proto" [t "Packet"] (t "L3Proto"),
    SMT.CmdDeclareFun  "L4.Proto" [t "Packet"] (t "L4Proto"),
    SMT.CmdDeclareFun  "IP4.src" [t "Packet"] (SMTB.tBitVec 32),
    SMT.CmdDeclareFun  "IP4.dst" [t "Packet"] (SMTB.tBitVec 32),
    SMT.CmdDeclareFun  "UDP.src" [t "Packet"] (SMTB.tBitVec 16),
    SMT.CmdDeclareFun  "UDP.dst" [t "Packet"] (SMTB.tBitVec 16)
    ]
    where
        t n = SMT.TApp n []
        v n = SMT.app n []

reducePG' :: (Show e, S.SolverM m e) => PG.PGraph -> Sem.Helpers -> m PG.PGraph
reducePG' graph helpers = do
    S.loadHelpers $ Sem.mergeHelpers helpers builtins
    -- Get entrypoints to the graph
    let noSeG = PGU.dropSpawnEdges graph
        isSrc = null . DGI.pre noSeG
        notONode n
            | Just (PG.ONode {}) <- DGI.lab graph n = False
            | otherwise = True
        entries = filter (\n -> isSrc n && notONode n) $ DGI.nodes noSeG
        entriesL = [ fromJust $ DGI.lab noSeG x | x <- entries ]
        --msg      = "reducePG' ENTRIES" ++ (show
    -- Prepare iteration state
    true <- S.exprTrue
    let ist = M.fromList $ zip entries $ repeat [(Nothing,true)]
    (graph',_,_) <- processNodes (graph,ist,S.fromList entries)
    return graph'

reducePG :: PG.PGraph -> Sem.Helpers -> IO PG.PGraph
reducePG graph helpers =
    Z3S.runZ3Solver $ CS.runCacheSolver $ reducePG' graph helpers
    --Z3S.runZ3Solver $ reducePG' graph helpers
    --HS.runHsmtlibSolver $ CS.runCacheSolver $ reducePG' graph helpers
    --HS.runHsmtlibSolver $ reducePG' graph helpers

