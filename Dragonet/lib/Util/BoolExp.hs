-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Util.BoolExp(
    BExp(..),
    andL,
    orL,
    variables,

    CNFBExp,
    CNFClause,
    CNFLiteral(..),
    
    cnfAnd,
    cnfOr,
    cnfNot,
    cnfVar,
    cnfAndL,
    cnfOrL,

    cnfVariables,
    bexp2cnf,
    cnf2bexp,
    toDIMACS,
    toECNF,
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L


-- Boolean expression
data BExp = BEVar String |
            BENot BExp |
            BEOr BExp BExp |
            BEAnd BExp BExp
    deriving (Eq,Ord)

instance Show BExp where
    show (BEVar v) = v
    show (BENot n) = "!" ++ show n
    show (BEAnd a b) = "(" ++ show a ++ "&" ++ show b ++ ")"
    show (BEOr a b) = "(" ++ show a ++ "|" ++ show b ++ ")"

-- Convert list of expressions to tree of ANDs of those expressions
andL :: [BExp] -> BExp
andL [] = error "andL on empty list"
andL l = foldl1 BEAnd l

-- Convert list of expressions to tree of ORs of those expressions
orL :: [BExp] -> BExp
orL [] = error "andL on empty list"
orL l = foldl1 BEOr l

-- Get all variable names
variables :: BExp -> [String]
variables ex =
    L.nub $ var' ex
    where
        var' (BEVar n) = [n]
        var' (BENot e) = variables e
        var' (BEAnd a b) = variables a ++ variables b
        var' (BEOr a b) = variables a ++ variables b


-- Map f to tree by applying it from the outside in
recOutIn :: (BExp -> BExp) -> BExp -> BExp
recOutIn f e =
    case f e of
        (BEAnd a b) -> BEAnd (recOutIn f a) (recOutIn f b)
        (BEOr a b) -> BEOr (recOutIn f a) (recOutIn f b)
        (BENot a) -> BENot (recOutIn f a)
        a -> a


-- Distribute ORs over ANDs
distr :: BExp -> (BExp,Bool)
distr (BEOr a (BEAnd b c)) = (fst $ distr $ BEAnd (BEOr a' b') (BEOr a' c'),True)
    where
        (a',_) = distr a
        (b',_) = distr b
        (c',_) = distr c
distr (BEOr (BEAnd b c) a) = (fst $ distr $ BEAnd (BEOr a' b') (BEOr a' c'),True)
    where
        (a',_) = distr a
        (b',_) = distr b
        (c',_) = distr c
distr (BEOr a b) = f (BEOr a' b')
    where
        (a',r1) = distr a
        (b',r2) = distr b
        f = if r1 || r2 then distr else (\x -> (x,False))
distr (BEAnd a b) = f (BEAnd a' b')
    where
        (a',r1) = distr a
        (b',r2) = distr b
        f = if r1 || r2 then distr else (\x -> (x,False))
distr a = (a,False)

-- Convert expression to CNF
toCNF :: BExp -> BExp
toCNF e = fst $ distr $ recOutIn demorgan e
    where
        demorgan (BENot (BEAnd a b)) = BEOr (BENot a) (BENot b)
        demorgan (BENot (BEOr a b)) = BEAnd (BENot a) (BENot b)
        demorgan (BENot (BENot a)) = a
        demorgan a = a

------------------------------------------------------------------------------
-- CNF expressions in set-of-clauses representation

-- CNF expression represented as Set of clauses
type CNFBExp = S.Set CNFClause
-- CNF clause represented as Set of literals
type CNFClause = S.Set CNFLiteral
data CNFLiteral = CNFLitPos String | CNFLitNeg String
    deriving (Show,Eq,Ord)

-- Return the variable name for a literal
litLabel :: CNFLiteral -> String
litLabel (CNFLitPos a) = a
litLabel (CNFLitNeg a) = a


-- Convert a boolean expression to the Set-of-clauses representation
bexp2cnf :: BExp -> CNFBExp
bexp2cnf a = toClauses $ toCNF a
    where
        toLiteral (BENot (BEVar n)) = CNFLitNeg n
        toLiteral (BEVar n) = CNFLitPos n
        toLiteral e = error ("Invalid literal: (" ++ show e ++ ")")

        toClause (BEOr b c) = toClause b `S.union` toClause c
        toClause e = S.singleton $ toLiteral e

        toClauses (BEAnd b c) = toClauses b `S.union` toClauses c
        toClauses e = S.singleton $ toClause e

-- Convert CNF expression in Set-of-clauses representation to BExp format
cnf2bexp :: CNFBExp -> BExp
cnf2bexp a =
    andL $ map fromClause $ S.toList a
    where
        fromLiteral (CNFLitPos l) = BEVar l
        fromLiteral (CNFLitNeg l) = BENot $ BEVar l

        fromClause c = orL $ map fromLiteral $ S.toList c
        
-- Get set with names of all variables ocurring in a CNF expression
cnfVariables :: CNFBExp -> S.Set String
cnfVariables e = S.foldl S.union S.empty $ S.map clause e
    where
        clause :: CNFClause -> S.Set String
        clause = S.map litLabel

-- Convert expression to equisatisfiable (but not equivalent) CNF
-- (implements the Tseitin-Transformation)
toECNF :: BExp -> CNFBExp
toECNF bexp = convert finalExp
    where
        finalExp = finalClauses ++ [[CNFLitPos finalName]]
        ((_,finalClauses),finalName) = rec (M.empty,[]) bexp
        convert = S.fromList . (map S.fromList)
        newvar m = "__tseT" ++ (show $ M.size m)
        rec ctx (BEVar n) = (ctx,n)
        rec ctx e =
            case M.lookup e m of
                Just n -> (ctx,n)
                Nothing ->
                    let n = newvar m
                        m' = M.insert e n m
                        ((m'',c'),e') = genFormula (m',c) n e
                        in ((m'', c' ++ e'),n)
            where (m,c) = ctx

        genFormula ctx n (BENot e) = (ctx',ex)
            where
                (ctx',eN) = rec ctx e
                ex = [[CNFLitNeg eN, CNFLitNeg n],
                      [CNFLitPos eN, CNFLitPos n]]

        genFormula ctx n (BEOr a b) = (ctx'',ex)
            where
                (ctx',aN) = rec ctx a
                (ctx'',bN) = rec ctx' b
                ex = [[CNFLitPos aN, CNFLitPos bN, CNFLitNeg n],
                      [CNFLitNeg aN, CNFLitPos n],
                      [CNFLitNeg bN, CNFLitPos n]]

        genFormula ctx n (BEAnd a b) = (ctx'',ex)
            where
                (ctx',aN) = rec ctx a
                (ctx'',bN) = rec ctx' b
                ex = [[CNFLitNeg aN, CNFLitNeg bN, CNFLitPos n],
                      [CNFLitPos aN, CNFLitNeg n],
                      [CNFLitPos bN, CNFLitNeg n]]

        genFormula _ _ (BEVar _) = undefined





------------------------------------------------------------------------------
-- Helpers for manipulating and combining CNF expressions and keeping them CNF

-- AND of two CNF expressions
cnfAnd :: CNFBExp -> CNFBExp -> CNFBExp
cnfAnd a b = a `S.union` b

-- OR of two CNF expressions
cnfOr :: CNFBExp -> CNFBExp -> CNFBExp
cnfOr a b = bexp2cnf $ BEOr (cnf2bexp a) (cnf2bexp b)
    {- Can be optimized
     common `S.union` different
    where
        common = a `S.intersection` b
        a' = a S.\\ common
        b' = b S.\\ common
        different = case (a',b') of
            (S.empty,S.empty) -> S.emtpy -- both are the same
            (a'',S.empty) -> a'' -- b is contained in a
            (a'',S.empty) -> BEOr (cnf2bexp a') (cnf2bexp b)
        bexp = BEOr (BEOr (cnf2bexp a) (cnf2bexp b')) (BEOr (cnf2bexp a') (cnf2bexp b))
        different = bexp2cnf bexp-}

-- Not of a CNF expression
cnfNot :: CNFBExp -> CNFBExp
cnfNot a = bexp2cnf $ BENot $ cnf2bexp a

-- CNF expression for a single variable
cnfVar :: String -> CNFBExp
cnfVar a = S.singleton $ S.singleton $ CNFLitPos a

-- Combine multiple CNF expressions using and
cnfAndL :: [CNFBExp] -> CNFBExp
cnfAndL [] = error "cnfAndL on empty list"
cnfAndL l = foldl1 cnfAnd l

-- Combine multiple CNF expressions using or
cnfOrL :: [CNFBExp] -> CNFBExp
cnfOrL [] = error "cnfAndL on empty list"
cnfOrL l = foldl1 cnfOr l



------------------------------------------------------------------------------

-- Convert CNF expression into DIMACS format (can be fed to minisat)
toDIMACS :: CNFBExp -> String
toDIMACS e = unlines (header:cs)
    where
        nVars = M.size varMap
        nClauses = S.size e
        header = "p cnf " ++ show nVars ++ " " ++ show nClauses

        cs :: [String]
        cs = map clause $ S.toList e

        clause :: CNFClause -> String
        clause c = unwords (map literal (S.toList c) ++ ["0"])

        literal :: CNFLiteral -> String
        literal (CNFLitPos l) = show $ lID l
        literal (CNFLitNeg l) = '-' : show (lID l)
        varMap :: M.Map String Int
        varMap = M.fromList $ zip (S.toList $ cnfVariables e) [1..]

        lID s = varMap M.! s


