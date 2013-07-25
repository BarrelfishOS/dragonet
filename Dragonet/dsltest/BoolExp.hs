module BoolExp(
    BExp(..),
    andL,
    orL,

    CNFBExp,
    cnfAnd,
    cnfOr,
    cnfNot,
    cnfVar,
    cnfAndL,
    cnfOrL,

    toDIMACS
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

data BExp = BEVar String |
            BENot BExp |
            BEOr BExp BExp |
            BEAnd BExp BExp
    deriving (Show,Eq)

andL :: [BExp] -> BExp
andL es = foldl1 BEAnd es

orL :: [BExp] -> BExp
orL es = foldl1 BEOr es


recInOut f (BEAnd a b) = f (BEAnd (recInOut f a) (recInOut f b))
recInOut f (BEOr a b) = f (BEOr (recInOut f a) (recInOut f b))
recInOut f (BENot a) = f (BENot (recInOut f a))
recInOut f e = f e

recOutIn f e =
    case f e of
        (BEAnd a b) -> (BEAnd (recOutIn f a) (recOutIn f b))
        (BEOr a b) -> (BEOr (recOutIn f a) (recOutIn f b))
        (BENot a) -> (BENot (recOutIn f a))
        e -> e






type CNFBExp = S.Set CNFClause
type CNFClause = S.Set CNFLiteral
data CNFLiteral = CNFLitPos String | CNFLitNeg String
    deriving (Show,Eq,Ord)


litLabel :: CNFLiteral -> String
litLabel (CNFLitPos a) = a
litLabel (CNFLitNeg a) = a


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
toCNF e = fst $ distr $ recOutIn demorgan e
    where
        demorgan (BENot (BEAnd a b)) = BEOr (BENot a) (BENot b)
        demorgan (BENot (BEOr a b)) = BEAnd (BENot a) (BENot b)
        demorgan (BENot (BENot a)) = a
        demorgan a = a
        
        dedupl (BEAnd a b) = if (a == b) then a else (BEAnd a b)
        dedupl (BEOr a b) = if (a == b) then a else (BEOr a b)
        dedupl a = a

toLiteral (BENot (BEVar n)) = CNFLitNeg n
toLiteral (BEVar n) = CNFLitPos n
toLiteral e = error ("Invalid literal: (" ++ (show e) ++ ")")

toClause (BEOr a b) = (toClause a) `S.union` (toClause b)
toClause e = S.singleton $ toLiteral e

toClauses (BEAnd a b) = (toClauses a) `S.union` (toClauses b)
toClauses e = S.singleton $ toClause e

bexp2cnf a = toClauses $ toCNF a
    where
        toLiteral (BENot (BEVar n)) = CNFLitNeg n
        toLiteral (BEVar n) = CNFLitPos n
        toLiteral e = error ("Invalid literal: (" ++ (show e) ++ ")")

        toClause (BEOr a b) = (toClause a) `S.union` (toClause b)
        toClause e = S.singleton $ toLiteral e

        toClauses (BEAnd a b) = (toClauses a) `S.union` (toClauses b)
        toClauses e = S.singleton $ toClause e

cnf2bexp a =
    andL $ map fromClause $ S.toList a
    where
        fromLiteral (CNFLitPos l) = BEVar l
        fromLiteral (CNFLitNeg l) = BENot $ BEVar l

        fromClause c = orL $ map fromLiteral $ S.toList c
        


    
cnfAnd :: CNFBExp -> CNFBExp -> CNFBExp
cnfAnd a b = a `S.union` b

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


cnfNot :: CNFBExp -> CNFBExp
cnfNot a = bexp2cnf $ BENot $ cnf2bexp a

cnfVar :: String -> CNFBExp
cnfVar a = S.singleton $ S.singleton $ CNFLitPos a


cnfAndL :: [CNFBExp] -> CNFBExp
cnfAndL es = foldl1 cnfAnd es

cnfOrL :: [CNFBExp] -> CNFBExp
cnfOrL es = foldl1 cnfOr es


-- Get set with names of all variables
cnfVariables :: CNFBExp -> S.Set String
cnfVariables e = clauses e
    where
        clauses :: CNFBExp -> S.Set String
        clauses e = S.foldl S.union S.empty $ S.map clause e
        clause :: CNFClause -> S.Set String
        clause e = S.map litLabel e


-- Convert expression into DIMACS format (can be fed to minisat)
toDIMACS :: CNFBExp -> String
toDIMACS e = unlines (header:cs)
    where
        nVars = M.size varMap
        nClauses = S.size e
        header = "p cnf " ++ (show nVars) ++ " " ++ (show nClauses)

        cs :: [String]
        cs = map clause $ S.toList e

        clause :: CNFClause -> String
        clause c = unwords ((map literal $ S.toList c) ++ ["0"])

        literal :: CNFLiteral -> String
        literal (CNFLitPos l) = show $ lID l
        literal (CNFLitNeg l) = "-" ++ (show $ lID l)

        varMap :: M.Map String Int
        varMap = M.fromList $ zip (S.toList $ cnfVariables e) [1..]

        lID s = fromJust $ M.lookup s varMap



