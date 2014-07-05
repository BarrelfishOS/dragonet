{-# LANGUAGE OverloadedStrings #-}
module Dragonet.Semantics.Simplify (
    reducePG
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem

import qualified Util.GraphHelpers as GH

import Control.Monad (foldM)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Graph.Inductive as DGI
import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTB
import qualified Hsmtlib as H
import qualified Hsmtlib.Solver as HS
import qualified Hsmtlib.Parsers.Syntax as HPS

addIn :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
addIn k v = M.insertWith (++) k [v]

lookupAll :: Eq a => a -> [(a,b)] -> [(a,b)]
lookupAll a as = filter ((== a) . fst) as

checkSat :: HS.Solver -> Sem.PortSemantics -> IO HPS.CheckSatResponse
checkSat sol ps = do
    HS.CGR HPS.Success <- HS.push sol 1
    HS.CGR HPS.Success <- HS.assert sol ps
    HS.CCS res <- HS.checkSat sol
    HS.CGR HPS.Success <- HS.pop sol 1
    --putStrLn $ "checkSat (" ++ (show $ SMT.pp ps) ++ ") -> " ++ show res
    return res

type EMap = M.Map DGI.Node [(String,Sem.PortSemantics)]

processPort :: PG.PGNode -> [(String, Sem.PortSemantics)] -> String
                -> Sem.PortSemantics
processPort (_,l) ins p =
    case lookup p $ PG.nSemantics l of
        Just e  -> inEx `SMTC.and` e
        Nothing -> inEx
    where
        inEx = case PG.nPersonality l of
                    PG.FNode   -> fInEx
                    PG.ONode _ -> oInEx
                    PG.CNode _ _  -> SMTC.true -- We know absolutely nothing
                                               -- here, since there could be a
                                               -- source node generated
        fInEx =
            case ins of
                [] -> SMTC.true
                es -> combinePExps SMTC.or es

        oInEx' p op =
            case lookupAll p $ ins of
                [] -> SMTC.false -- Not reachable
                es -> combinePExps op es
        PG.ONode op = PG.nPersonality l
        oInEx =
            case (op,p) of
                (PG.OpAnd,"true") -> oInEx' "true" SMTC.and
                (PG.OpAnd,"false") -> oInEx' "false" SMTC.or

                (PG.OpNAnd,"true") -> oInEx' "false" SMTC.or
                (PG.OpNAnd,"false") -> oInEx' "true" SMTC.and

                (PG.OpOr,"true") -> oInEx' "true" SMTC.or
                (PG.OpOr,"false") -> oInEx' "false" SMTC.and

                (PG.OpNOr,"true") -> oInEx' "false" SMTC.and
                (PG.OpNOr,"false") -> oInEx' "true" SMTC.or

        combinePExps o es = snd $ foldl1 (\(_,a) (ip,b) -> (ip,a `o` b)) es

processNode :: HS.Solver -> (PG.PGraph,EMap) -> PG.PGNode -> IO (PG.PGraph,EMap)
processNode solver (g,eM) fn@(n,nL) = do
    -- Get incoming constraints
    let ins = fromMaybe [] $ M.lookup n eM
    -- Get expressions for each port
    let ps = map (\p -> (p,processPort fn ins p)) $ PG.nPorts nL
    let lsucs = DGI.lsuc g n
    -- Check satisfiability of ports
    let portSat :: (PG.PGraph,EMap) -> (PG.Port,Sem.PortSemantics)
                        -> IO (PG.PGraph,EMap)
        portSat (g',eM') (p,e) = do
            us <- checkSat solver e
            let edges = [((n,eN,eP),e) | (eN,eP) <- lsucs, eP == p]
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
    SMT.CmdDeclareFun  "plen" [SMT.TApp "Packet" []] (SMTB.tBitVec 16),
    SMT.CmdDeclareFun  "pget"
                        [SMT.TApp "Packet" [], SMTB.tBitVec 16]
                        (SMTB.tBitVec 8),
    SMT.CmdDeclareFun  "pkt" [] (SMT.TApp "Packet" [])]
    where

reducePG :: PG.PGraph -> Sem.Helpers -> IO PG.PGraph
reducePG graph helpers = do
    sol <- H.startSolver HS.Z3 HS.Online HS.QF_AUFBV Nothing Nothing
    let SMT.Script cmds = helpers
    mapM_ (expectSuccess . runCmd sol) (builtins ++ cmds)
    -- Get topologically sorted list of nodes
    let nodes = GH.topsortLN graph
    -- Iterate over nodes, keeping a map of Expr for each edge that was
    -- processed
    (graph',_) <- foldM (processNode sol) (graph,M.empty) nodes
    HS.exit sol
    putStrLn $ show graph
    putStrLn $ show graph'
    return graph'

