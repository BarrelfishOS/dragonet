module Dragonet.Constraints(
    constrain,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.BoolExp as BE
import qualified Util.BoolExpParser as BEP
import qualified Util.GraphHelpers as GH

import qualified Data.Graph.Inductive as DGI

import qualified Data.List as L
import qualified Debug.Trace as TR
import Control.Monad

import qualified System.IO as SI
import qualified System.Cmd as SC
import System.IO.Temp
import System.Exit


-- Options for generating constraint expressions:
--   - Build normal BExp expressions, tend to get large because of redundancy,
--       expression basically doubles for each and node, but can be converted to
--       equisatisfiable CNF which should only grow it linearly
--   - CNFExp: Keeps the expression in CNF format, uses naive equivalent
--       transformation -> possibly exponential increase for not/ors,
--       but tends to reduce redundancy, because identical clauses in the CNF
--       will only be included once.
--
--  Need to enable corresponding set of functions below

--type CExp = BE.CNFBExp
type CExp = BE.BExp

cAnd :: CExp -> CExp -> CExp
cOr :: CExp -> CExp -> CExp
cNot :: CExp -> CExp
cVar :: String -> CExp
cAndL :: [CExp] -> CExp
cOrL :: [CExp] -> CExp
cVariables :: CExp -> [String]
cFromBE :: BE.BExp -> CExp
cToCNF :: CExp -> BE.CNFBExp

-- CNF exps
{-cAnd = BE.cnfAnd
cOr = BE.cnfOr
cNot = BE.cnfNot
cVar = BE.cnfVar
cAndL = BE.cnfAndL
cOrL = BE.cnfOrL
cVariables = S.toList . BE.cnfVariables
cFromBE = BE.bexp2cnf
cToCNF = id-}

-- Normal expressions
cAnd = BE.BEAnd
cOr = BE.BEOr
cNot = BE.BENot
cVar = BE.BEVar
cAndL = BE.andL
cOrL = BE.orL
cVariables = BE.variables
cFromBE = id
cToCNF = BE.toECNF


-- Constraints induced by a particular port on a node
getNPConstraints :: PG.Node i -> PG.Port -> Maybe CExp
getNPConstraints n p = do
    a <- L.find (L.isPrefixOf prefix) $ PG.nAttributes n
    parse $ drop (length prefix) a
    where
        prefix = "C." ++ p ++ ":"
        parse a = case BEP.parseExp a of
            Left _ -> error ("Error parsing constraint for node '" ++
                        PG.nLabel n ++ "'.'" ++ p ++ "'")
            Right e -> Just $ cFromBE e

-- Generates the constraints for a specific node
combineN :: GH.RecContext [(PG.Port,CExp)] PG.Port (PG.Node i) (PG.Node i) PG.Port -> [(PG.Port,CExp)]
combineN (pr,(_,n),_) = map port $ PG.nPorts n
    where
        port p = (p,case PG.nPersonality n of
            PG.ONode op -> opPort op (findIn "true") (findIn "false") p
            PG.FNode -> maybe fInExp (cAnd fInExp) (fExp p)
            PG.CNode _ -> error "Encountered CNode")

        opPort PG.OpAnd ts _ "true" = cAndL ts
        opPort PG.OpOr ts _ "true" = cOrL ts
        opPort op ts fs "false" = cNot $ opPort op ts fs "true"
        opPort _ _ _ p = error ("Unexpected port p=" ++ p ++ " on ONode")

        fExp = getNPConstraints n
        fInExp = case inExps of
            [] -> cVar "Dummy"
            ((_,e):[]) -> e
            ((_,e):_) -> TR.trace ("Warning: Multiple incoming edges on F node "
                                   ++ show n) e

        findIn p = map snd $ filter ((== p) . fst) inExps
        inExps = concatMap (\(e,(_,m)) -> filter ((== e) . fst) m) pr

-- Generate constraints for each node/port
generateConstraints :: PG.PGraph i -> [((DGI.Node,PG.Port),CExp)]
generateConstraints g =
    concatMap (\(n,l) -> map (\(p,e) -> ((n,p),e)) l) $ DGI.labNodes g'
    where g' = GH.recurseNFW combineN g

-- Additional constraints that are independent of specific nodes
additionalConstraints :: [((DGI.Node,PG.Port),CExp)] -> CExp
additionalConstraints es = cAndL (global ++ varConstr)
    where
        -- Global constraints
        global = ["TCP" `mutex` "UDP"]
        -- Look for variable constraints in the form a=b, and build list of a's
        varPrefixes =
            L.nub $ map (takeWhile ('=' /=)) $ filter (elem '=') varnames

        -- Variables in the formula
        varnames = L.nub $ concatMap cVariables $ map snd es
        varByPref p = filter (L.isPrefixOf p) varnames
        varConstr = concatMap (mutexL . varByPref) varPrefixes
        mapPairs f l = map (uncurry f) [(a,b) | a <- l, b <- l, a < b] -- a bit ugly
        mutex a b = cNot (cVar a) `cOr` cNot (cVar b)
        mutexL = mapPairs mutex

addAdditionalConstraints :: [((DGI.Node,PG.Port),CExp)] -> [((DGI.Node,PG.Port),CExp)]
addAdditionalConstraints ls = map (\(np,e) -> (np,cAnd e c)) ls
    where c = additionalConstraints ls


data Ternary = TTrue | TFalse | TZ
    deriving (Show, Eq)

-- Takes an expression and checks if it is satisfiable by writing it to a
-- file and passing it to minisat.
isSAT :: CExp -> IO Ternary
isSAT e = withSystemTempFile "toMinisat.dimac" toMinisat
    where
        toMinisat fp h = do
            SI.hPutStr h (BE.toDIMACS $ cToCNF e)
            SI.hFlush h
            rc <- SC.system ("minisat " ++ fp ++ " >/dev/null")
            return (case rc of
                (ExitFailure 10) -> TTrue  -- Satisfiable
                (ExitFailure 20) -> TFalse -- Not satisfiable
                _ -> TZ)

-- Get node/port combinations that are unsatisfiable
getUnsatisfiable :: [((DGI.Node, PG.Port), CExp)] -> IO [(DGI.Node,PG.Port)]
getUnsatisfiable es = do
    us <- filterM unsatisfiable es
    return $ map fst us
    where
        unsatisfiable (_,e) = do
            s <- isSAT e
            return (s == TFalse)

-- Remove edges originating from ports with unsatisfiable constraints
removeUnsatisfiable :: PG.PGraph i -> [(DGI.Node, PG.Port)] -> PG.PGraph i
removeUnsatisfiable g u = TR.trace (show edges) (GH.delLEdges edges g)
    where
        edges = concatMap handlePort u
        handlePort (n,p) = map (\(m,_) -> (n,m,p)) $
                                filter ((== p) . snd) $ DGI.lsuc g n

-- Simplify graph:
--    - remove nodes without incoming or outgoing edges
simplifyGraph :: PG.PGraph i -> PG.PGraph i
simplifyGraph g = DGI.delNodes orphaned g
    where
        orphaned = filter ((== 0) . DGI.deg g) $ DGI.nodes g


-- Apply constraints to graph
constrain :: PG.PGraph i -> IO (PG.PGraph i)
constrain g = do
    let constraints = addAdditionalConstraints $ generateConstraints g
    unsatisfiable <- getUnsatisfiable constraints
    putStrLn $ show g
    putStrLn $ show unsatisfiable
    return (simplifyGraph $ removeUnsatisfiable g unsatisfiable)

