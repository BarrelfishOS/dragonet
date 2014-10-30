{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonet.Predicate (
    PredExpr(..),
    predEval, predEquiv,
    predEquivHard, predEquivHard_,
    predEquivUnder,
    predTrueUnder, predTrueUnder_,
    --
    isDNF,
    --
    initPredCompSt_, initPredCompSt, PredCompSt(..),
    nodePred, computePred, computePredMany,
    --
    predBuildSimple, predBuildFold, predBuildDNF, predDoBuild,
    --
    predFnodePort, opPred, opNot,
    portPred, portPred_,
    --
    nodePredCache, PredCache,
    depPredCache,
    --
    parseStr_,
    parseStr,
    parseStrDNF,
    parseStrFold,
    --
    predGetAtoms,
    --
    PredAssignment,
    --
    dnfGetANDs,
    dnfEquiv_, dnfEquiv,
    dnfSAT,
    -- exposed for the testing module:
    PredBuild(..),
    getAllAssigns,
    --
    dnetPrShow
) where

import Dragonet.Predicate.Definitions (PredExpr(..), PredBuild(..),
                                       predBuildSimple,
                                       AtomKey, AtomVal)
import Dragonet.Predicate.Parser (parseStr_)

import Dragonet.ProtocolGraph        as PG
import qualified Util.GraphHelpers   as GH
import Dragonet.ProtocolGraph.Utils  (getFNodeByName, edgeDeps, edgePort, spawnDeps, isSpawnTarget)
import Dragonet.Conventions (isTruePort, isFalsePort)

import qualified Data.Graph.Inductive as DGI
import qualified Data.List as L
import qualified Data.Map.Strict  as M
import Data.Tuple (swap)
import Data.Maybe
import Data.Function (on)

import qualified Control.Monad.State as ST

import Text.Show.Functions -- show instance for functions, so that ConfFunction
                           -- gets Show and we can derive Show for PredBuild PredCompSt
import Debug.Trace (trace)
import Control.Exception.Base (assert)

import Text.Show.Pretty (ppShow)

-- enable (or disable) tracing
traceMessages = False
xtrace = if traceMessages then trace else \a b -> b

tr = flip $ trace
trN a b = a

-- The code here essentially provides means to express and reason with boolean
-- expressions. It's somewhat similar to https://github.com/beastaugh/hatt.

-- Path predicates:
--  A path predicate is a boolean expression with atoms in the form of: (node
--  label, port of node). A predicate characteriszes the input fed into the
--  graph (e.g., a packet) based on which choices would be made during graph
--  execution.
--
-- There are two ways to handle atoms. Typically, each atom can be either true
-- or false. In our case, we add some simple semantics to atoms.  Because only a
-- single port can be enabled in a node, if (LABEL, VAL1) is true, then (LABEL,
-- VALX) is true iff VALX == VAL1.  Hence, (LABEL, VAL1) AND (LABEL, VAL2) AND
-- VAL1 == VAL2 is false For example, if the atom (EtherType, IPv4) is true then
-- the atom (EtherType, ARP) is false.
--
-- Eventually we would like to express the atoms using offsets in the packets.
--
-- NOTE: The label does not uniqely identify a node in the graph, but we assume
-- that all nodes with the same label have the same semantics and so produce a
-- unique predicate.
--
-- The code in this module preforms similar operations with the semantic helpers
-- and the SMT solver -- both are essentially different approaches of doing the
-- same thing. Eventually, we need to choose one. The SMT solver is more
-- general, but potentially less performant.

--
------------- Helper functions ---------
-- (we might want to move them into a different file)
--

pgSpawnEdgePred :: PredBuild -> (PG.PGNode, PG.PGEdge) -> PredExpr
pgSpawnEdgePred bld (node, (_, _, ESpawn _ attrs)) = expr
    where isPredAttr ::  ESAttribute -> Bool
          isPredAttr (ESAttrPredicate _) = True -- only this one for now

          parse :: ESAttribute -> PredExpr
          parse (ESAttrPredicate x) = parseStrFold $ xtrace ("parsing: " ++ x) x

          predicates = filter isPredAttr attrs
          predicates' = xtrace ("predicates are: " ++ (show predicates)) predicates
          exprs = map parse predicates'
          -- if there are no labels, add a dummy atom to note where we need to
          -- add spawn attributes
          expr = case length exprs of
                      0 -> PredAtom ("SPAWN." ++ (nLabel $ snd node)) "XX"
                      1 -> exprs !! 0
                      otherwise -> (buildAND bld) exprs

--predicates from incoming spawn edges (combine them with or if there are more
--than one)
pgSpawnPreds :: PredBuild -> PG.PGraph -> PG.PGNode -> Maybe PredExpr
pgSpawnPreds bld pg n = case length spawn_edges of
    0 -> Nothing
    otherwise -> Just $ (buildOR bld) [ pgSpawnEdgePred bld x | x <- spawnDeps pg n]
    where mapfn = pgSpawnEdgePred
          spawn_edges = spawnDeps pg n
          nlabel = nLabel $ snd n

--
-- Atom assignments
--
type PredAssignment = (AtomKey, AtomVal, PredExpr)

mkAss :: AtomKey-> AtomVal -> PredExpr -> PredAssignment
mkAss nlabel port expr
    | predIsConst expr = (nlabel, port, expr)
    | otherwise = error "Let's keep assigments to constants unless we need otherwise"

-- group  (and validate) a list for assigments by label
groupAssignments :: [PredAssignment] -> [(AtomKey, [PredAssignment])]
groupAssignments as = as_grouped'
    where groups :: [[PredAssignment]]
          groups = L.groupBy ((==) `on` \(l,_,_) -> l) as
          as_grouped :: [(AtomKey, [PredAssignment])]
          as_grouped = [ ((\ (l,_,_) -> l) $ (x !! 0), L.nub x) | x <- groups]
          as_grouped' = map validate_group as_grouped
          validate_group x@(glbl, gassignments) = x'
            where x' = case (length true_as) `compare` 1 of
                        GT        -> error $ "label " ++ glbl ++ "Has more than 1 true assigments in " ++ (show as)
                        otherwise -> x
                  true_as = filter ( \ (_,_,e) -> e == PredTrue ) gassignments
--
------- Predicate helpers -----
--

-- is this a node that terminates the expression tree?
-- false if expression is an AND or an OR, true otherwise.
predIsTerm :: PredExpr -> Bool
predIsTerm (PredAtom _ _) = True
predIsTerm PredTrue       = True
predIsTerm PredFalse      = True
predIsTerm (PredNot x)    = predIsTerm x -- note that not(not(...)) will be folded
predIsTerm (PredAnd _)    = False
predIsTerm (PredOr  _)    = False

predIsConst :: PredExpr -> Bool
predIsConst PredTrue  = True
predIsConst PredFalse = True
predIsConst _         = False

--
------- Predicate builders -----
--

-- Folding versions: perform simple folding when possible

-- NOT folder
-- NB: Not sure if we want to do that, but we could push NOTs all the way down
-- to the leaves. We avoid doing it for now
predNotFold :: PredExpr -> PredExpr
predNotFold PredTrue    = PredFalse -- fold constants
predNotFold PredFalse   = PredTrue
predNotFold (PredNot x) = x         -- fold double NOTs
predNotFold x           = PredNot x

-- OR folder
predOrFold :: [PredExpr] -> PredExpr
predOrFold [] = error "Empty predicate list (OR)"
predOrFold x  = let x'   = xtrace ("OR args: " ++ (show x)) x
                    ret  = foldOR PredFalse x'
                    ret' = xtrace ("RESULT: " ++ (show ret)) ret
                in ret'

-- OR folder recursive function
foldOR :: PredExpr -> [PredExpr] -> PredExpr
foldOR res []     = res
foldOR res (x:xs) =
    case (res `foldOR1` x) of
      Just PredTrue -> PredTrue
      Just e        -> foldOR e xs
      Nothing       -> foldOR newres xs
      where newres = case (res, x) of
                       (PredOr l1, PredOr l2) -> foldOR res l2
                       (PredOr l1, _        ) -> PredOr $ x:l1
                       _                      -> PredOr [res,x]

-- first level of folding:
--  check only term nodes
foldOR1 :: PredExpr -> PredExpr -> Maybe PredExpr
foldOR1 PredTrue _  = Just PredTrue
foldOR1 PredFalse e = Just e
foldOR1 a1@(PredAtom _ _) a2@(PredAtom _ _)
    | a1 == a2  = Just a1 -- (A OR A) is A
    | otherwise = Nothing
foldOR1 a1@(PredAtom _ _) na2@(PredNot a2@(PredAtom _ _))
    | a1 == a2  = Just PredTrue -- A OR (NOT A) is TRUE
    | otherwise = Nothing
foldOR1 na1@(PredNot a1@(PredAtom _ _)) a2@(PredAtom _ _)
    | a1 == a2  = Just PredTrue -- (NOT A) OR A is TRUE
    | otherwise = Nothing
foldOR1 na1@(PredNot _) na2@(PredNot _)
    | na1 == na2  = Just na1 -- (NOT A) OR  (NOT A) is A
    | otherwise = Nothing
foldOR1 e1 e2
    | predIsTerm e2  = foldOR1 e2 e1
    | otherwise = Nothing

-- AND folder
predAndFold :: [PredExpr] -> PredExpr
predAndFold [] = error "Empty predicate list (AND)"
predAndFold x  = let x'  = xtrace ("AND args: " ++ (show x)) x
                     ret  = foldAND PredTrue x'
                     ret' = xtrace ("RESULT: " ++ (show ret)) ret
                in ret'

-- AND folder recursive function
foldAND :: PredExpr -> [PredExpr] -> PredExpr
foldAND res []     = res
foldAND res (x:xs) = case (res `foldAND1` x) of
      Just PredFalse -> PredFalse
      Just e        -> foldAND e xs
      Nothing       -> foldAND newres xs
      where newres = case (res, x) of
                       (PredAnd l1, PredAnd l2) -> foldAND res l2
                       (PredAnd l1, _        )  -> case predIsTerm x of
                                                     True  -> foldANDMany res x
                                                     False -> PredAnd $ x:l1
                       _                        -> PredAnd [res,x]

foldANDMany :: PredExpr -> PredExpr -> PredExpr
foldANDMany (PredAnd l1) e = foldANDMany_ [] l1 e
    where foldANDMany_ :: [PredExpr] -> [PredExpr] -> PredExpr -> PredExpr
          foldANDMany_ res [] e = PredAnd $ e:res
          foldANDMany_ res (x:xs) e =  case (e `foldAND1` x) of
                                         Just PredFalse -> PredFalse
                                         Just e'        -> foldANDMany_ res xs e'
                                         Nothing        -> foldANDMany_ (x:res) xs e

-- first level of folding:
--  check only term nodes
foldAND1 :: PredExpr -> PredExpr -> Maybe PredExpr
foldAND1 PredFalse _ = Just PredFalse
foldAND1 PredTrue e  = Just e
foldAND1 a1@(PredAtom tl1 tp1) a2@(PredAtom tl2 tp2)
    | tl1 == tl2 = case tp1 == tp2 of -- same atom label
                     True  -> Just a1    -- same port, just keep one
                     False -> Just PredFalse  -- different port, FALSE
    | otherwise = Nothing
foldAND1 a1@(PredAtom _ _) na2@(PredNot a2@(PredAtom _ _))
    | a1 == a2         = Just PredFalse   -- A AND (NOT A) is False
    | otherwise        = Nothing
foldAND1 na1@(PredNot a1@(PredAtom _ _)) a2@(PredAtom _ _)
    | a1 == a2  = Just PredFalse -- (NOT A) AND A is False
    | otherwise = Nothing
foldAND1 na1@(PredNot a1@(PredAtom _ _)) na2@(PredNot a2@(PredAtom _ _))
    | na1 == na2  = Just na1      -- same atom, just keep one
    | otherwise  = Nothing
foldAND1 e1 e2
    | predIsTerm e2  = foldAND1 e2 e1
    | otherwise = Nothing

-- evaluate (and hopefully simplify) a predicate expression given a set of atoms
-- assignments. There are ways to do that:
-- Given an assigment (label, port) = true:
-- 1. replace all those atoms with true
-- 2. replace all those atoms with true and all atoms (label, p /= port) with
--    false (rich atoms)

predEval :: PredExpr -> [PredAssignment] -> PredExpr
predEval e as = predEval_ e (groupAssignments as)

predEval_ :: PredExpr
          -> [(AtomKey, [PredAssignment])]
          -> PredExpr
-- constants
predEval_ PredTrue  _ = PredTrue
predEval_ PredFalse _ = PredFalse
-- recurse
-- NOTE on folding: we don't change the structure here, so I think it's safe to
-- use the folding builders (i.e., if we get a DNF expression, we will get a DNF
-- expression if we fold expressions based on the given assignments)
predEval_ (PredAnd l) ts = predAndFold $ [predEval_ e ts | e <- l ]
predEval_ (PredOr  l) ts = predOrFold  $ [predEval_ e ts | e <- l ]
predEval_ (PredNot p) ts = predNotFold $  predEval_ p ts
-- replace atoms when possible
predEval_ oldexpr@(PredAtom lbl port) grouped_as =
    case L.lookup lbl grouped_as of
        Nothing -> oldexpr
        Just assignments -> evalAtom oldexpr assignments

-- evaluate an atom expression using a set of assignments on the atom's label
evalAtom ::PredExpr -> [PredAssignment] -> PredExpr
evalAtom expr@(PredAtom label port) assignments = assert check ret
    where true_l = filter (\ (_,_,e) -> e == PredTrue) assignments
          matching_as = L.find (\ (_,p,_) -> p == port) assignments
          ret = case matching_as of
                -- if one the assignments matches our port, return its value
                Just (_,_,e) -> e
                -- otherwise, if there is another assignment that does not
                -- matches our port then this atom is false
                Nothing -> case length true_l of
                             0 -> expr
                             1 -> PredFalse
                             otherwise  -> error $ "seems that assigment list (" ++ (show assignments) ++ ") has more than one true assignments for the same label"
          -- assignments should be on the atom's label
          check = and $ map ((==label) . \ (l,_,_) -> l) assignments

predGetAtoms :: PredExpr -> [(NLabel,NPort)]
predGetAtoms PredTrue  = []
predGetAtoms PredFalse = []
predGetAtoms (PredAtom nlabel nport) = [(nlabel,nport)]
predGetAtoms (PredAnd l) = L.concat $ map predGetAtoms l
predGetAtoms (PredOr  l) = L.concat $ map predGetAtoms l
predGetAtoms (PredNot p) = predGetAtoms p

--
------- Disjunctive Normal Form -----
--
--
-- https://cstheory.stackexchange.com/questions/1410/why-is-cnf-used-for-sat-and-not-dnf
-- https://math.stackexchange.com/questions/159591/solving-sat-by-converting-to-disjunctive-normal-form


isDNF :: PredExpr -> Bool
isDNF x
    | predIsTerm x = True
    | otherwise = case x of
                    (PredAnd l) -> and $ map predIsTerm l
                    (PredOr  l) -> and $ map isConj l
    where isConj :: PredExpr -> Bool
          isConj x
             | predIsTerm x = True
             | otherwise = case x of
                             (PredOr _) -> False
                             (PredAnd l') -> and $ map predIsTerm l'

dnfGetANDs :: PredExpr -> [[PredExpr]]
dnfGetANDs x =  assert check ret
    where check = isDNF x
          ret = if predIsTerm x then [[x]]
                else case x of
                    (PredAnd l) -> [l]
                    (PredOr  l) -> concat $ map dnfGetANDs l

dnfTermAssign :: PredExpr -> PredAssignment
dnfTermAssign (PredAtom l p)               = (l, p, PredTrue)
dnfTermAssign ((PredNot (PredAtom l p)))   = (l, p, PredFalse)

-- arbitrary ordering for atoms
dnfAtomCmp :: PredExpr -> PredExpr -> Ordering
dnfAtomCmp (PredAtom l1 p1) (PredAtom l2 p2) = (l1,p1) `compare` (l2,p2)
dnfAtomCmp (PredNot (PredAtom l1 p1)) (PredNot (PredAtom l2 p2)) = (l1,p1) `compare` (l2, p2)
dnfAtomCmp (PredNot _) (PredAtom _ _) = LT
dnfAtomCmp (PredAtom _ _) (PredNot _) = GT

--
dnfAndlEq :: [PredExpr] -> [PredExpr] -> Bool
dnfAndlEq l1 l2 = l1' == l2'
    where l1' = L.sortBy dnfAtomCmp l1
          l2' = L.sortBy dnfAtomCmp l2

dnfEquiv_ :: PredExpr -> PredExpr -> Maybe String
dnfEquiv_ expr1 expr2 = ret
    where a1 = dnfGetANDs expr1
          a2 = dnfGetANDs expr2

          ret = case length a1 == length a2 of
            True ->  eqAndLs a1 a2
            False -> Just "dnfEquiv_: list lengths are different"

          eqAndLs :: [[PredExpr]] -> [[PredExpr]] -> Maybe String
          eqAndLs (a:as) bs  = case L.find (dnfAndlEq a) bs of
                                  Just _  -> eqAndLs as bs -- recurse
                                  Nothing -> Just $ "Could not find match for: " ++ (ppShow  a)
          eqAndLs [] bs = Nothing

dnfEquiv e1 e2 = isNothing $ dnfEquiv_ e1 e2


-- first level (only terms, and)
dnfSAT1 :: PredExpr -> [PredAssignment]
dnfSAT1 (PredAtom l p)             = [(l, p, PredTrue)]
dnfSAT1 ((PredNot (PredAtom l p))) = [(l, p, PredFalse)]
dnfSAT1 (PredAnd l)                = L.concat $ map dnfSAT1 l

dnfSAT_ :: PredExpr -> Maybe [[PredAssignment]]
-- constants
dnfSAT_ PredFalse = Nothing   -- not satisfiable
dnfSAT_ PredTrue  = Just [[]] -- always  satisfiable (tautology)
-- terminals
dnfSAT_  expr
    | PredOr  l <- expr = Just $ map dnfSAT1 l
    | otherwise         = Just $ [dnfSAT1 expr]


dnfSAT :: PredExpr -> Maybe [[PredAssignment]]
dnfSAT expr = assert (isDNF expr) $ dnfSAT_ expr  -- we should remove this check eventually

-- simplify OR for DNF
--
-- IDEA: If we consider the AND expressions under the OR of the DNF form, and
-- A_i is the set of terms of the ith AND expression, then if A_i is a subset of
-- A_j, then A_j can be removed.
--
-- Using exhaustive patterns: hopefully this would make it simpler (although
-- longer) and also catch erroneous assumptions
-- NB: I was wrong, it is horrible.
--
dnfOrFold :: [PredExpr] -> PredExpr
dnfOrFold xs = dnfOR PredFalse xs

-- dnfOR folding function
--
-- result, rest of the expression, final result
dnfOR :: PredExpr -> [PredExpr] -> PredExpr
--
-- terminate recursion
dnfOR x []                 = x
-- The elements in the list cannot be OR
dnfOR _ (x@(PredOr l):xs) = error "Unexpected OR in dnfOR"
-- fold true
dnfOR PredTrue _      = PredTrue
dnfOR _ (PredTrue:xs) = PredTrue
-- ignore false
dnfOR PredFalse (x:xs)              = dnfOR x xs
dnfOR r              (PredFalse:xs) = dnfOR r xs

-- Result is term
dnfOR res@(PredAtom _ _) (x:xs)
  | predIsTerm x =  case (res `foldOR1` x) of
                     Just PredTrue -> PredTrue
                     Just e        -> dnfOR e xs
                     Nothing       -> dnfOR (PredOr [res, x]) xs
  | (PredAnd l) <- x = case res `elem` l of
                     True         -> dnfOR res xs
                     False        -> dnfOR (PredOr [res, x]) xs

-- Result is (NOT term)
dnfOR res@(PredNot (PredAtom _ _)) (x:xs)
  | predIsTerm x = case (res `foldOR1` x) of
                   Just PredTrue -> PredTrue
                   Just e        -> dnfOR e xs
                   Nothing       -> dnfOR (PredOr [res,x]) xs
  | (PredAnd l) <- x = case res `elem` l of
                   True          -> dnfOR res xs
                   False         -> dnfOR (PredOr [res, x]) xs

-- Result is AND
----- AND with term
----- AND with NOT term
----- AND with AND
dnfOR r@(PredAnd l) (t2@(PredAtom _ _):xs) =
    case t2 `elem` l of
        True  -> dnfOR t2 xs
        False -> dnfOR (PredOr [r,t2]) xs
dnfOR r@(PredAnd l) (x2@(PredNot (PredAtom _ _)):xs) =
    case x2 `elem` l of
        True ->  dnfOR x2 xs
        False -> dnfOR (PredOr [r,x2]) xs
dnfOR r@(PredAnd l1__) ((x@(PredAnd l2__)):xs) =
    -- NB: not sure if nub is needed here, because equal terms should have been
    -- eliminated at this point. Do nub just to be on the safe side and use the
    -- the "nubbed " lists in the results
    let l1 = L.nub l1__
        l2 = L.nub l2__
    in case isSubset l1 l2 of
        1 -> dnfOR (PredAnd l1) xs
        2 -> dnfOR (PredAnd l2) xs
        0 -> dnfOR (PredOr [PredAnd l1, PredAnd l2]) xs


-- Result is OR
----- OR with term
----- OR with NOT term
----- OR with AND
dnfOR r@(PredOr _) (t@(PredAtom _ _):xs)           = dnfOR (dnfOrAddTerm r t) xs
dnfOR r@(PredOr _) (t@(PredNot (PredAtom _ _)):xs) = dnfOR (dnfOrAddTerm r t) xs
dnfOR r@(PredOr _) (e@(PredAnd _):xs)              = dnfOR (dnfOrAddAnd  r e) xs

-- Add an AND expression to an OR expression in DNF
dnfOrAddAnd :: PredExpr  -- OR expression
            -> PredExpr  -- AND expression
            -> PredExpr  -- resulting OR expression
dnfOrAddAnd (PredOr  orList)
            andExpr@(PredAnd _) = PredOr $ doAdd [] orList andExpr
  where doAdd :: [PredExpr] -- current output list (for recursion)
              -> [PredExpr] -- OR list
              -> PredExpr   -- AND expr
              -> [PredExpr] -- result
        doAdd out []     ae@(PredAnd _)  = ae:out -- stop recursion
        doAdd out l@(x:xs) ae@(PredAnd al1__) = case x of
            t@(PredAtom _ _) ->
                if t `elem` al1__ then (out ++ l) else def
            t@(PredNot (PredAtom _ _)) ->
                if t `elem` al1__ then (out ++ l) else def
            t@(PredAnd al0__) ->
                -- NB: Same as before: not sure if nub is needed here, because
                -- equal terms should have been eliminated at this point. Do nub
                -- just to be on the safe side and use the the "nubbed " lists
                -- in the results
                let al0 = L.nub al0__
                    al1 = L.nub al1__
                    x'  = PredAnd al0
                    ae' = PredAnd al1
                in case isSubset al0 al1 of
                    1 -> out ++ (x':xs)   -- ignore ae
                    2 -> doAdd out xs ae' -- ignore x
                    _ -> doAdd (x':out) xs ae'
            -- fallback
            _  -> def
            where def = doAdd (x:out) xs ae -- default

-- return:
--  0: no list is a subset of the other
--  1: 1st list is a subset of the 2nd
--  2: 2nd list is a subset of the 1st
--  NB: not sure what happens if the lists have duplicate elements
isSubset :: Eq a => [a] -> [a] -> Integer
isSubset l1 l2 =
    let l1_len = length l1
        l2_len = length l2
        t = ((1, l1, l1_len), (2, l2, l2_len))
        ((sid, small, slen), (bid, big, blen)) = if l1_len < l2_len then t else (swap t)
    in case slen `compare` (length $ L.intersect small big) of
        EQ -> sid
        LT -> error "?!?!?!?!??!"
        GT -> 0

-- add a term to a DNF OR list
dnfOrAddTerm :: PredExpr -> PredExpr -> PredExpr
dnfOrAddTerm (PredOr ol) term = PredOr $ term:ol'
    where ol' = filter (not . dorem) ol
          dorem :: PredExpr -> Bool
          -- remove ANDs that have t2 as an element, they are redundant
          dorem (PredAnd al) = term `elem` al
          -- remove terms that are the same as t2, otherwise keep
          dorem e = if term == e then True else False

--predDnfOr = predOrFold
predDnfOr = dnfOrFold

-- take two DNF expressions and create their DNF conjuction
-- note: we are using the generic folding functions, which will address cases
-- we do not handle here
predAndDNF_ :: PredExpr -> PredExpr -> PredExpr
predAndDNF_ arg1 arg2
 | (predIsTerm arg1) &&  (predIsTerm arg2) = predAndFold [arg1, arg2]
 | (predIsTerm arg2) = predAndDNF_ arg2 arg1
 | (predIsTerm arg1) = case arg2 of
     PredOr  l -> predDnfOr  $ map (predAndDNF_ arg1) l
     PredAnd l -> predAndFold $ arg1:l
 | otherwise = case (arg1, arg2) of
     -- (m1 + m2) * (m3 + m4) =  (m1 * m3) + (m1 * m4) + (m2 * m3) + (m2 * m4)
     ((PredOr  l1), (PredOr  l2)) -> predDnfOr  $ [ predAndDNF_ x1 x2 | x1 <- l1, x2 <- l2 ]
     -- (m1 * m2) * (m3 * m4) = m1 * m2 * m3 * m4
     ((PredAnd l1), (PredAnd l2)) -> predAndFold $ l1 ++ l2
     -- (m1 + m2) * (m3 * m4) = (m1 * m3 * m4) + (m2 * m3 * m4)
     ((PredOr  l1), (PredAnd l2)) -> predDnfOr  $ [ predAndDNF (x1:l2) | x1 <- l1 ]
     ((PredAnd l1), (PredOr  l2)) -> predAndDNF_ arg2 arg1

-- take a list of DNF expressions and create their DNF conjuction
predAndDNF :: [PredExpr] -> PredExpr
predAndDNF l = foldl predAndDNF_ PredTrue l

-- take two DNF expressions and create their DNF disunction
predOrDNF_ :: PredExpr -> PredExpr -> PredExpr
predOrDNF_ arg1 arg2
    | (predIsTerm arg1) && (predIsTerm arg2) = predDnfOr [arg1, arg2]
    | (predIsTerm arg2) = predOrDNF_ arg2 arg1
    | (predIsTerm arg1) =  case arg2 of
        PredOr  l     -> predDnfOr $ arg1:l
        (PredAnd _)   -> predDnfOr [arg1, arg2]
    | otherwise = case (arg1, arg2) of
        -- (m1 + m2) + (m3 + m4) = (m1 + m2 + m3 + m4)
        ((PredOr  l1), (PredOr  l2)) -> predDnfOr $ l1 ++ l2
        -- (m1 * m2) + (m3 * m4) = (m1 * m2) + (m3 * m4)
        ((PredAnd l1), (PredAnd l2)) -> predDnfOr [arg1, arg2]
        -- (m1 + m2) + (m3 * m4) = m1 + m2 + (m3 * m4)
        ((PredOr  l1), e2@(PredAnd _)) -> predDnfOr $ e2:l1
        ((PredAnd l1), (PredOr  l2))   -> predOrDNF_ arg2 arg1

-- take a list of DNF expressions and create their DNF disjunction
predOrDNF :: [PredExpr] -> PredExpr
predOrDNF l = foldl predOrDNF_ PredFalse l

-- create the negation of a DNF expression
predNotDNF :: PredExpr -> PredExpr
predNotDNF PredTrue         = PredFalse
predNotDNF PredFalse        = PredTrue
predNotDNF t@(PredAtom _ _) = PredNot t
predNotDNF (PredNot x)      = x

predNotDNF (PredAnd l)      = predDnfOr $ map predNotDNF l
predNotDNF (PredOr  l)      = predAndDNF $ map predNotDNF l

--
------- Statement equivalence -----
--

-- check whether two predicates are equivalent: simple equality for now
predEquiv :: PredExpr -> PredExpr -> Bool
predEquiv a b =  a == b

-- generate all possible assignments from a list of atoms
--  If we get atoms with the same label, we generate the following cases:
--    . just one of the atoms is true (and subsequently all others are false)
--    . all of them are false
-- The latter is because we have now way of knowing whether our list of atoms
-- expresses the whole range of assignments for a partocular label
getAllAssigns :: [(NLabel, NPort)] -> [[PredAssignment]]
getAllAssigns atoms = map concat $ sequence $ map getAssigns grouped
    where grouped :: [(NLabel, [NPort])]
          grouped = [ f a | a <- L.groupBy ((==) `on` fst) atoms ]
              where f x = (fst $ x !! 0, map snd x)
          getAssigns :: (NLabel, [NPort]) -> [[PredAssignment]]
          getAssigns (lbl, ports) = allf:[ [(lbl, p, PredTrue)] | p <- ports ]
              where allf = [ (lbl, p, PredFalse) | p <- ports ]

andErr :: [a] -> (a -> Bool) -> Maybe a
andErr [] _ = Nothing
andErr (x:xs) pred = case pred x of
    True  -> andErr xs pred
    False -> Just x

-- Just x: assighment for which the two expressions are not equivalent
-- Nothing: expressions are equivalent
predEquivHard_ :: PredExpr -> PredExpr -> Maybe [PredAssignment]
predEquivHard_ expr1 expr2 = ret
    where atoms1 = predGetAtoms expr1
          atoms2 = predGetAtoms expr2
          atoms  = L.nub $ atoms1 ++ atoms2
          all_assigns = getAllAssigns (trN atoms ("ATOMS: " ++ (ppShow atoms)))
          ret = case length atoms of
            0 -> andErr [[]] checkAssignment
            _ -> andErr all_assigns checkAssignment
          checkAssignment :: [PredAssignment] -> Bool
          checkAssignment a = trN (eval1 == eval2) $ "Checking assignment a:" ++ (ppShow a)
            where eval1 = predEval expr1 a
                  eval2 = predEval expr2 a

predEquivHard :: PredExpr -> PredExpr -> Bool
predEquivHard expr1 expr2 = case predEquivHard_ expr1 expr2 of
    Just _ -> False
    Nothing -> True

{-
predEquivHard :: PredExpr -> PredExpr -> Bool
predEquivHard expr1 expr2 = ret
    where atoms1 = predGetAtoms expr1
          atoms2 = predGetAtoms expr2
          atoms  = L.nub $ atoms1 ++ atoms2
          all_assigns = getAllAssigns atoms
          ret = case length atoms of
            0 -> checkAssignment []
            _ -> and [ checkAssignment a | a <- all_assigns]
          -- for debugging
          checkAssignment :: [PredAssignment] -> Bool
          checkAssignment a = ret
            where eval1 = predEval expr1 a
                  eval2 = predEval expr2 a
                  ret   = eval1 == eval2
                  ret'  = case ret of
                            True -> ret
                            False -> trace msg ret
                  msg   = "\n----\nassignment:" ++ (show a) ++ "\n" ++
                          " expr1:" ++ (show expr1) ++ " eval1:" ++ (show eval1) ++ "\n" ++
                          " expr2:" ++ (show expr2) ++ " eval2:" ++ (show eval1) ++ "\n" ++
                          "\n---\n"
-}

predEquivUnder__ :: PredExpr -> PredExpr -> [PredAssignment] -> Bool
predEquivUnder__ expr1 expr2 a = predEquivHard e1 e2
    where e1 = predEval expr1 a
          e2 = predEval expr2 a

predEquivUnder_ :: (PredExpr, PredExpr) -> PredExpr -> Maybe [[PredAssignment]]
predEquivUnder_ (expr1, expr2) expr_cond = ret
    where assigns_ = dnfSAT expr_cond
          assigns = case assigns_ of
            Just x  -> x
            Nothing -> error "NYI: condition is unsatisfiable"
          results = [ predEquivUnder__  expr1 expr2 a  | a <- assigns  ]
          ret = case and results of
                  True -> Nothing
                  False -> Just [ a | (r,a) <- zip results assigns,  r == False ]

predEquivUnder :: (PredExpr, PredExpr) -> PredExpr -> Bool
predEquivUnder (e1,e2) c = isNothing $ predEquivUnder_ (e1,e2) c

predTrueUnder :: PredExpr -> PredExpr -> Bool
predTrueUnder expr1 expr2 = predEquivUnder (expr1, PredTrue) expr2

predTrueUnder_ :: PredExpr -> PredExpr -> Maybe [[PredAssignment]]
predTrueUnder_ expr1 expr2 = predEquivUnder_ (expr1, PredTrue) expr2

--
------- Computing predicate epxressions -----
--

type PredCache = M.Map DGI.Node PredExpr

-- state for computing predicate expression:
-- compStop will stop the search when a particular type of node is met.
-- Normally, the search will stop when an entry node with no dependencies is
-- reached. However, when are searching in an embedded graph, we need to stop
-- when we reach the sink node.
data PredCompSt = PredCompSt {
      predGraph  :: PG.PGraph
    , predDst    :: PG.PGNode
    , predInPort :: Maybe NPort
    , compStop   :: PG.PGNode -> Bool
    , predBld    :: PredBuild
    , predCache  :: M.Map DGI.Node PredExpr
} deriving (Show)

-- initialize state
initPredCompSt_ = PredCompSt {
      predGraph  = undefined
    , predDst    = undefined
    , predInPort = Nothing
    , compStop   = \x -> False
    , predBld    = predBuildDNF -- build dnf expressions
    , predCache  = M.empty
}

-- initializer helper
initPredCompSt :: PG.PGraph -> String -> PredCompSt
initPredCompSt gr node = initPredCompSt_ {
      predGraph = gr
    , predDst   = getFNodeByName gr node
}

newtype PredComp a = PredComp {doPredComp :: ST.State PredCompSt a}
    deriving (Monad, ST.MonadState PredCompSt, Functor)


computePred st = ST.runState (doPredComp computePredM) st

computePredMany :: PG.PGraph -> [PG.PGNode] -> [PredExpr]
computePredMany gr nodes = ret
    where (ret, st) = ST.runState (doPredComp $ cPred nodes) st0
          st0 =  initPredCompSt_ { predGraph = gr }
          cPred :: [PG.PGNode] -> PredComp ([PredExpr])
          cPred nodes = do
            ST.forM nodes $ \n -> do
                ST.modify $ \s -> s { predDst = n }
                computePredM


-- simple helper for computing a predicate for reaching a particular node
nodePred :: PG.PGraph -> PG.PGNode -> PredExpr
nodePred g n = fst $ computePred $ initPredCompSt_ { predGraph = g, predDst = n }


-- same as above, but includes a cache
-- TODO: update and return cache
nodePredCache :: PG.PGraph -> PG.PGNode -> PredCache -> PredExpr
nodePredCache g n c = fst $ computePred $ initPredCompSt_ {
      predGraph = g
    , predDst   = n
    , predCache = c}

depPredCache :: PG.PGraph -> (PG.PGNode, PG.NPort) -> PredCache -> PredExpr
depPredCache g (n,p) c = fst ret
    where st = initPredCompSt_ {
                  predGraph = g
                , predCache = c}
          ret = ST.runState (doPredComp $ depGetPred__ (n,p)) st

portPred :: PredBuild -> PG.PGNode -> PG.NPort -> PredExpr
portPred bld (_,fnode@(FNode {})) port = portPred_ bld fnode port

portPred_ :: PredBuild -> PG.Node -> PG.NPort -> PredExpr
portPred_ bld fnode@(FNode {}) port = pred
    where pred' = tr pred ("node:" ++ (PG.nLabel fnode) ++ " port:" ++ port ++ " port pred:" ++ (show pred))
          -- NB: In our current semantics, if predicates are defined for a
          -- particular port, then the port selection predicates are ignored.
          -- Note that, if needed, they can be added in the port predicates
          -- (e.g., in the unicorn file)
          pred  = case L.lookup port (PG.nPredicates fnode) of
                   Just x -> predDoBuild bld x
                   Nothing -> atom
          atom  = case length (nPorts fnode) of
                    0 -> error $ "We expect a port named `" ++ port ++ "' in node:\n" ++ (ppShow fnode)
                    -- add single port predicates
                    1 -> PredAtom (PG.nLabel fnode) port
                    -- ignore single port predicates
                    -- 1 -> PredTrue
                    _ -> PredAtom (PG.nLabel fnode) port


predFnodePort :: PredBuild -> PredExpr -> (PG.PGNode, PG.NPort) -> PredExpr
predFnodePort bld in_pred (node,port) = (buildAND bld) [dep_pred, in_pred]
    where dep_pred = portPred bld node port

opPred :: PredBuild -> [PredExpr] -> (NOperator, String) -> PredExpr
opPred bld preds (op, port) = expr
    where expr = case (port, op) of
            ("true", NOpAnd)  -> bldAND preds
            ("true", NOpOr)   -> bldOR preds
            ("true", NOpNOr)  -> bldNOT $ bldOR preds
            ("true", NOpNAnd) -> bldNOT $ bldAND preds
            --("true", _)       -> error  $ "opPred for true/operator " ++ (show op) ++ " NYI"
            ("false", NOpAnd) -> bldOR  $ map bldNOT preds
            ("false", NOpOr)  -> bldAND $ map bldNOT preds
            ("false", _)      -> error  $ "opPred for false/operator " ++ (show op) ++ " NYI"
            (_, _)            -> error  $ "Expecting true/false, not:" ++ port
          bldAND  = buildAND bld
          bldOR   = buildOR  bld
          bldNOT  = buildNOT bld

-- get the predicate expression for a particular node port (dependency)
depGetPred__ :: (PG.PGNode, PG.NPort) -> PredComp (PredExpr)
-- dependency is an FNode
depGetPred__ (dep_node@(_, fnode@(FNode {})), dep_port) = do
    bld <- ST.gets predBld
    ST.modify $ \s ->  s { predDst = dep_node, predInPort = Just dep_port}
    rec <- computePredM
    let ret'= trN ret $ "getting predicate of dependency node: " ++ ((PG.nLabel . snd) dep_node) ++ " port:" ++ dep_port ++ " RET:" ++ (ppShow ret)
        ret = predFnodePort bld rec (dep_node, dep_port)
    return ret
-- dependency is an ONode
depGetPred__ (dep_node@(_, ONode {}), dep_port) = do
    ST.modify $ \s -> s {predDst = dep_node, predInPort = Just dep_port}
    computePredM

depGetPred__ (dep_node@(_, CNode {}), dep_port) = do
    error "depGetPred__ not defined for Cnodes"

-- get the predicate expression for a dependency
depGetPred :: (PG.PGNode,  PG.PGEdge) -> PredComp (PredExpr)
depGetPred (node, edge) = depGetPred__ (node, port)
    where port = edgePort edge


-- version with cache
computePredM :: PredComp (PredExpr)
computePredM = do
    (nid, _) <- ST.gets predDst
    cache    <- ST.gets predCache
    case M.lookup nid cache of
        Just pred -> return pred
        Nothing -> do
            pred <- computePred__
            let cache' = M.insert nid pred cache
            ST.modify $ \s -> s { predCache = cache' }
            return pred

computePred__ :: PredComp (PredExpr)
computePred__ = do
    (_,dst) <- ST.gets predDst
    case dst of
        (FNode {}) -> computePredFnode__
        (ONode {}) -> computePredOnode__

-- Compute a predicate expression for packets that reach predDst in predGraph
--  We assume that the graph is acyclic
--
computePredFnode__ :: PredComp (PredExpr)
-- compute predicate of src->dst, where dst is an FNode:
--  predicate of pre(dst) AND predicate of src->pre(dst)
computePredFnode__ = do
    stopfn  <- ST.gets compStop
    dst     <- ST.gets predDst
    gr      <- ST.gets predGraph
    inport  <- ST.gets predInPort
    bld     <- ST.gets predBld

    let deps = edgeDeps gr dst


    let
       -- predecesors of destintation (going backwards)
        -- (only consider normal edges)
        ndeps      = length deps
        spawn_pred = pgSpawnPreds bld gr dst

        ret
         -- we reached an entry node
         | ndeps == 0 = case spawn_pred of
              Just e  -> return e         -- there *is* a spawn predicate
              Nothing -> return PredTrue  -- this should probably be false, but it currently breaks some cases
         | ndeps > 1   = error $ "F-nodes have at most one incoming edge. Offending node:`" ++ (nLabel $ snd dst) ++ "'"
         -- | isSpawnTarget gr dst = error "NYI: both normal and spawn edges" -- combines normal and spawn edges (treat it as an OR?)
         -- recurse
         | otherwise   = do
              let dep0 = deps !! 0
              dep_pred <- depGetPred dep0
              -- we check if there is also a spawn predicate for this node. If
              -- that's the case, we OR it with the dependency predicate.
              let ret_pred   = case spawn_pred of
                      Just x  -> (buildOR bld) [dep_pred, x]
                      Nothing -> dep_pred
              case (stopfn $ fst dep0) of
                True  -> return PredTrue
                False -> return ret_pred
    ret


-- compute predicate of src->dst, where dst is an ONode:
--  OP (e.g., AND) [ predicate of src -> pre ] for each pre in pre(dst)
computePredOnode__ :: PredComp (PredExpr)
computePredOnode__ = do
   Just inport <- ST.gets predInPort
   dst <- ST.gets predDst
   g   <- ST.gets predGraph
   bld <- ST.gets predBld
   let op = PG.nOperator $ snd dst
       lbl = PG.nLabel   $ snd dst
       deps :: [(PG.PGNode, PG.PGEdge)]
       deps = edgeDeps g dst
       -- predecessors (i.e., dependencies) of destination (going backwards)
       -- We calculate the dependencies based on the true port. That is, we
       -- assume that the false port is the (NOT true port) of the same node
       (true_deps, false_deps) = onodeDepsTF deps

   t_preds_ <- case length true_deps of
                 0 -> return [PredFalse] --error $ "No true dependencies for node " ++ lbl
                 _ -> mapM depGetPred true_deps
   f_preds_ <- case length false_deps of
                 0 -> return [PredFalse]  --error $ "No false dependencies for node " ++ lbl
                 _ -> mapM depGetPred false_deps
   let t_preds = trN t_preds_ $ "Visiting node:" ++ lbl ++ " (t_preds_=" ++ (ppShow t_preds_) ++ ")"
       f_preds = f_preds_ --tr f_preds_ $ "Visiting node:" ++ lbl ++ " (expragrs=" ++ (show f_preds_) ++ ")"
       ret = case inport of
         "true"  -> return $ opPred bld t_preds (op, "true")
         "false" -> return $ opPred bld f_preds (opNot op, "true")
   ret

depPortName :: (PG.PGNode, PG.PGEdge) -> PG.NPort
depPortName (_, (_, _, Edge { ePort = eport })) = eport

isTFPort s = (isTruePort s ||) (isFalsePort s)

opNot :: PG.NOperator -> PG.NOperator
opNot PG.NOpAnd  = PG.NOpOr
opNot PG.NOpOr   = PG.NOpAnd
--
opNot PG.NOpNOr   = PG.NOpNAnd
opNot PG.NOpNAnd  = PG.NOpNOr

onodeDepsTF :: [(PGNode, PGEdge)] -> ([(PGNode, PGEdge)], [(PGNode, PGEdge)])
onodeDepsTF deps = ret
 where
    -- true/false/all other ports
    true_deps  = filter (isTruePort  .    depPortName) deps
    false_deps = filter (isFalsePort .    depPortName) deps
    other_deps = filter (not . isTFPort . depPortName) deps

    -- We assume all other ports to be True for now. This is *stupid*, and
    -- should be fixed by having proper edge labels about the in port on Onodes.
    isOtherDepTrue :: (PGNode, PGEdge) -> Bool
    isOtherDepTrue dep@(n,(_,_,PG.Edge { ePort = p })) = case length nodeDeps of
                           0 -> error "onodeDepsTF: not supposed to happen"
                           _ -> tr True $ "NOTE: port:" ++ p ++ " on node "
                                          ++ ((PG.nLabel . snd) n)
                                          ++ " is assumed a true port"
        where nodeDeps = [ d | d <- deps,  depNodeId d == xId ]
              depNodeId ((nid,_), _) = nid
              xId = depNodeId dep

    (true_deps2, unknown_deps) = L.partition isOtherDepTrue other_deps

    ret = case length unknown_deps of
        0 -> (true_deps ++ true_deps2, false_deps)
        _ -> error $ "There are onode incoming edges which cannot be calssified as a true/false port:\n"
                      ++ (ppShow other_deps)


trueONodeDeps :: [(PGNode, PGEdge)] -> [(PG.PGNode,PG.PGEdge)]
trueONodeDeps deps = true_deps
    where groups  = L.groupBy ((==) `on` depNodeId) deps
          depNodeId ((nid,_), _) = nid
          true_deps = map getTrueDep groups
          getTrueDep ds = case length depsl of
                            0 -> error $ "No true port for node: " ++ ( nLabel . snd . fst) (ds !! 0)
                            1 -> depsl !! 0
               where depsl = filter (isTruePort  . depPortName) ds

partitionMany :: [a] -> [a -> Bool] -> [[a]]
partitionMany xs [] = [xs]
partitionMany xs (f:fs) = xs_f:(partitionMany xs_rest fs)
    where (xs_f,xs_rest) = L.partition f xs

-- helper function for printing dragonet predicates
dnetPrShow :: PredExpr -> String
dnetPrShow expr = assert (isDNF expr) ret
    where ret = ret_
          ret_ = "OR\n" ++ (L.intercalate "\n" $ [ "  " ++ x | x <- ands_str ])
          ands_str = map showAndL (dnfGetANDs expr)
          showAndL :: [PredExpr] -> String
          showAndL es_ = "AND \n" ++ (L.intercalate "\n" $ [ "    " ++ (show x) | x <- es ])
            where es = L.concat parts
                  parts = [ L.sortBy (compare `on` fAtomName) x | x <- partitionMany es_ fns ]
                  fns = [f_fs, f_ethT, f_ipP, f_tx, f_rx]
                  f_ethT = (== "EthType") . fAtomName
                  f_ipP  = (== "IpProt")  . fAtomName
                  f_tx  = (L.isPrefixOf "Tx")  . fAtomName
                  f_rx  = (L.isPrefixOf "Rx")  . fAtomName
                  f_fs  = (L.isPrefixOf "FromSock")  . fAtomName

                  fAtomName :: PredExpr -> String
                  fAtomName (PredAtom n _) = n
                  fAtomName (PredNot (PredAtom n _)) = n

-- do some folding to simplify expressions
predBuildFold = PredBuild {
      buildAND    = predAndFold
    , buildOR     = predOrFold
    , buildNOT    = predNotFold
    , builderName = "Folders"
}

-- produce expressions in DNF form
predBuildDNF = PredBuild {
      buildAND    = predAndDNF
    , buildOR     = predOrDNF
    , buildNOT    = predNotDNF
    , builderName = "DNF"
}


predDoBuild :: PredBuild -> PredExpr -> PredExpr
predDoBuild _ e@(PredTrue) = e
predDoBuild _ e@(PredFalse) = e
predDoBuild _ e@(PredAtom _ _) = e
predDoBuild b@(PredBuild {buildAND = band}) (PredAnd l) =
    band $ map (predDoBuild b) l
predDoBuild b@(PredBuild {buildOR = bor}) (PredOr l) =
    bor  $ map (predDoBuild b) l
predDoBuild b@(PredBuild {buildNOT = bnot}) (PredNot e) =
    bnot $ predDoBuild b e

parseStrDNF :: String -> PredExpr
parseStrDNF = parseStr_ predBuildDNF

parseStrFold :: String -> PredExpr
parseStrFold = parseStr_ predBuildFold

parseStr :: String -> PredExpr
parseStr = parseStr_ predBuildSimple
