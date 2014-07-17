{-# LANGUAGE OverloadedStrings #-}
module Dragonet.Semantics.Simplify (
    reducePG
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Semantics as Sem

import qualified Util.GraphHelpers as GH

import Control.Monad (foldM)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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

addIn :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
addIn k v = M.insertWith (++) k [v]

lookupAll :: Eq a => a -> [(a,b)] -> [(a,b)]
lookupAll a as = filter ((== a) . fst) as

sAnd a b
    | a == b = a
    | a == SMTC.true = b
    | b == SMTC.true = a
    | a == SMTC.false || b == SMTC.false = SMTC.false
    | otherwise = a `SMTC.and` b

sOr a b
    | a == b = a
    | a == SMTC.false = b
    | b == SMTC.false = a
    | a == SMTC.true || b == SMTC.true = SMTC.true
    | otherwise = a `SMTC.or` b

type MySolver = (HS.Solver,
                 TV.TVar (M.Map Sem.PortSemantics HPS.CheckSatResponse))

checkSat :: MySolver -> Sem.PortSemantics -> IO HPS.CheckSatResponse
checkSat (sol,cache) ps = do
    c <- M.lookup ps <$> (STM.atomically $ TV.readTVar cache)
    case c of
        Nothing -> do
            HS.CGR HPS.Success <- HS.push sol 1
            HS.CGR HPS.Success <- HS.assert sol ps
            HS.CCS res <- HS.checkSat sol
            HS.CGR HPS.Success <- HS.pop sol 1
            --putStrLn $ "checkSat (" ++ (show $ SMT.pp ps) ++ ") -> " ++ show res
            STM.atomically $ TV.modifyTVar' cache (M.insert ps res)
            return res

        Just r -> return r

type EMap = M.Map DGI.Node [(String,Sem.PortSemantics)]

processPort :: PG.PGNode -> [(String, Sem.PortSemantics)] -> String
                -> Sem.PortSemantics
processPort (n,l) ins p =
    case l of
        PG.FNode { PG.nSemantics = sems }->
            case lookup p sems of
                Just e  -> inEx `sAnd` e
                Nothing -> inEx
        _ -> inEx
    where
        inEx = case l of
                    PG.FNode {} -> fInEx
                    PG.ONode {PG.nOperator = op} -> oInEx op
                    PG.CNode {} -> SMTC.true -- We know absolutely nothing
                                             -- here, since there could be a
                                             -- source node generated
        fInEx =
            case ins of
                [] -> SMTC.true
                es -> combinePExps sOr es

        oInEx' p op =
            case lookupAll p $ ins of
                [] -> SMTC.false -- Not reachable
                es -> combinePExps op es
        oInEx op =
            case (op,p) of
                (PG.NOpAnd,"true") -> oInEx' "true" sAnd
                (PG.NOpAnd,"false") -> oInEx' "false" sOr

                (PG.NOpNAnd,"true") -> oInEx' "false" sOr
                (PG.NOpNAnd,"false") -> oInEx' "true" sAnd

                (PG.NOpOr,"true") -> oInEx' "true" sOr
                (PG.NOpOr,"false") -> oInEx' "false" sAnd

                (PG.NOpNOr,"true") -> oInEx' "false" sAnd
                (PG.NOpNOr,"false") -> oInEx' "true" sOr

        combinePExps o es = snd $ foldl1 (\(_,a) (ip,b) -> (ip,a `o` b)) es

processNode :: MySolver -> (PG.PGraph,EMap) -> PG.PGNode -> IO (PG.PGraph,EMap)
processNode solver (g,eM) fn@(n,nL) = do
    -- Get incoming constraints
    let ins = fromMaybe [] $ M.lookup n eM
    -- Get expressions for each port
    let ps = map (\p -> (p,processPort fn ins p)) $ PG.nPorts nL
    let lsucs = DGI.lsuc g n
    -- Check satisfiability of ports
    let portSat :: (PG.PGraph,EMap) -> (PG.NPort,Sem.PortSemantics)
                        -> IO (PG.PGraph,EMap)
        portSat (g',eM') (p,e) = do
            us <- checkSat solver e
            let pMatch p' PG.Edge { PG.ePort = p } = p' == p
                pMatch _ _ = False
                edges = [((n,eN,eP),e) | (eN,eP) <- lsucs, pMatch p eP]
            let updateEM e' =
                    M.delete n $
                        foldl (\m ((_,n',_),_) -> addIn n' (p,e') m) eM' edges
            return $
                case us of
                    HPS.Unsat -> (-- Port not satisfiable, drop edges
                        map fst edges `GH.delLEdges` g',
                        updateEM SMTC.false)
                    _ -> -- Satisfiable or unknown, update map
                        (g', updateEM e)
    -- Apply changes to graph and edge map
    foldM portSat (g,eM) ps

runCmd :: HS.Solver -> SMT.Command -> IO HS.Result
runCmd s (SMT.CmdDeclareType n i)    = HS.declareType s n i
runCmd s (SMT.CmdDefineType n ps t)  = HS.defineType s n ps t
runCmd s (SMT.CmdDeclareFun n ps t)  = HS.declareFun s n ps t
runCmd s (SMT.CmdDefineFun n bs t e) = HS.defineFun s n bs t e
runCmd s (SMT.CmdAssert e)           = HS.assert s e

expectSuccess :: IO HS.Result -> IO ()
expectSuccess run = do
    HS.CGR HPS.Success <- run
    return ()


builtins = [
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

reducePG :: PG.PGraph -> Sem.Helpers -> IO PG.PGraph
reducePG graph helpers = do
    sol <- H.startSolver HS.Z3 HS.Online HS.QF_AUFBV Nothing Nothing
    let SMT.Script cmds = helpers
    mapM_ (expectSuccess . runCmd sol) (builtins ++ cmds)
    -- Get topologically sorted list of nodes
    let graph' = PGU.dropSpawnEdges graph
        nodes = GH.topsortLN graph'
    -- Prepare cached solver
    cache <- STM.atomically $ TV.newTVar $ M.empty
    let sol' = (sol,cache)
    -- Iterate over nodes, keeping a map of Expr for each edge that was
    -- processed
    (graph'',_) <- foldM (processNode sol') (graph,M.empty) nodes
    HS.exit sol
    return graph''

