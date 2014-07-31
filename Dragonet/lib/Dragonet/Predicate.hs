module Dragonet.Predicate (
    PredicateExpr(..),
    predAnd, predOr,
    predEval,
    parseFile, parseStr, predGetTerms
) where

import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NLabel, NPort, Edge(..), NOperator(..))
import qualified Text.ParserCombinators.Parsec as P

import Debug.Trace (trace)
import Data.List (intercalate, lookup, concat)

-- path predicates


traceMessages = False
xtrace = if traceMessages then trace else \a b -> b

data PredicateExpr = PredicateTerm NLabel NPort |
                     PredicateOr  [PredicateExpr] |
                     PredicateAnd [PredicateExpr] |
                     PredicateNot PredicateExpr |
                     PredicateTrue | PredicateFalse |
                     PredicateUndef
    deriving (Eq)

instance Show PredicateExpr where
    show (PredicateTerm node port) = "pred(" ++ node ++ "," ++ port ++ ")"
    show (PredicateOr l)  = "or(" ++ (intercalate ","  $ map show l) ++ ")"
    show (PredicateAnd l) = "and(" ++ (intercalate "," $ map show l) ++ ")"
    show (PredicateNot e) = "not("++ (show e) ++ ")"
    show PredicateTrue    = "true"
    show PredicateFalse   = "false"
    show PredicateUndef   = "UNDEFINED"

-- create an AND predicate, and do folding when possible
predicateAND :: [PredicateExpr] -> PredicateExpr
predicateAND [] = error "Empty predicate list"
predicateAND x  = let x'   = xtrace ("AND args: " ++ (show x)) x
                      ret  = doAND PredicateUndef x'
                      ret' = xtrace ("RESULT: " ++ (show ret)) ret
                   in ret'
 where
  doAND :: PredicateExpr -> [PredicateExpr] -> PredicateExpr
  doAND result              []                  = result
  doAND _                   (PredicateFalse:xs) = PredicateFalse
  doAND PredicateUndef      (PredicateTrue:xs)  = doAND PredicateTrue xs
  doAND result              (PredicateTrue:xs)  = doAND result xs
  doAND PredicateUndef      (x:xs)              = doAND (PredicateAnd [x])   xs
  doAND PredicateTrue       (x:xs)              = doAND x  xs
  doAND (PredicateAnd [r0]) (x:xs)              = case x == r0 of
                                                    True  -> doAND x xs
                                                    False -> doAND (PredicateAnd [x,r0]) xs
  doAND (PredicateAnd r)    (x:xs)              = doAND (PredicateAnd (x:r)) xs
  doAND r                   l                   = error $ "This should not happen (AND)" ++ "result: --" ++ (show r) ++ "-- rest: --" ++ (show l) ++ "--"

-- create an OR predicate, and do folding when possible
predicateOR :: [PredicateExpr] -> PredicateExpr
predicateOR [] = error "Empty predicate list"
predicateOR x  = let x'   = xtrace ("OR args: " ++ (show x)) x
                     ret  = doOR PredicateUndef x'
                     ret' = xtrace ("RESULT: " ++ (show ret)) ret
                  in ret'
 where
  doOR :: PredicateExpr -> [PredicateExpr] -> PredicateExpr
  doOR result                []                   = result
  doOR _                     (PredicateTrue:xs)   = PredicateTrue
  doOR PredicateUndef        (PredicateFalse:xs)  = doOR PredicateFalse xs
  doOR result                (PredicateFalse:xs)  = doOR result xs
  doOR PredicateUndef        (x:xs)               = doOR (PredicateOr [x]) xs
  doOR PredicateFalse        (x:xs)               = doOR x xs
  doOR r@(PredicateTerm _ _) (x:xs)               = case x == r of
                                                    True -> doOR r xs
                                                    False -> doOR (PredicateOr [r,x]) xs
  doOR (PredicateOr [r0])    (x:xs)               = case x == r0 of
                                                    True  -> doOR r0 xs
                                                    False -> doOR (PredicateOr [r0,x]) xs
  doOR (PredicateOr r) (x:xs)                     = doOR (PredicateOr (x:r)) xs
  doOR r               l                          = error $ "This should not happen (OR) " ++ "result: --" ++ (show r) ++ "-- rest: --" ++ (show l) ++ "--"

predicateNOT :: PredicateExpr -> PredicateExpr
predicateNOT PredicateTrue = PredicateFalse
predicateNOT PredicateFalse = PredicateTrue
-- TODO: fold AND/OR
predicateNOT x = PredicateNot x

-- potentially replace them with the versions above that do folding
predAnd = predicateAND
predOr  = predicateOR
predNot = predicateNOT
--predAnd = PredicateAnd
--predOr  = PredicateOr

-- evaluate (and hopefully simplify!) a predicate expression given a set of term
-- assignments
predEval :: PredicateExpr -> [((NLabel,NPort), PredicateExpr)] -> PredicateExpr
-- replace terms when possible
predEval oldexpr@(PredicateTerm l1 p1)  terms =
    case lookup (l1,p1) terms of
        Nothing   -> oldexpr
        Just expr -> predEval expr terms -- (let's hope there are no cycles there :)
-- constants
predEval PredicateTrue _  = PredicateTrue
predEval PredicateFalse _ = PredicateFalse
-- fold
predEval (PredicateAnd l) ts = predAnd $ [predEval e ts   | e <- l ]
predEval (PredicateOr  l) ts = predOr  $ [predEval e ts   | e <- l ]
predEval (PredicateNot p) ts = predNot $  predEval p ts


predGetTerms :: PredicateExpr -> [(NLabel,NPort)]
predGetTerms PredicateTrue  = []
predGetTerms PredicateFalse = []
predGetTerms (PredicateTerm nlabel nport) = [(nlabel,nport)]
predGetTerms (PredicateAnd l) = concat $ map predGetTerms l
predGetTerms (PredicateOr  l) = concat $ map predGetTerms l
predGetTerms (PredicateNot p) = predGetTerms p


{-
 - silly parser for building predicate expressions
 -}

wspace :: P.CharParser st ()
wspace = P.skipMany (P.oneOf " \t")

parse_id :: P.CharParser st String
parse_id = do
    c <- P.letter
    cs <- P.many (P.alphaNum P.<|> P.char '_')
    return $ c:cs

kw_parse :: String -> P.CharParser st ()
kw_parse kw = do
    P.string kw
    P.notFollowedBy P.alphaNum

par_parse :: P.CharParser st (PredicateExpr)
par_parse = do
    P.char '('
    e <- expr_parse
    P.char ')'
    return e

not_parse :: P.CharParser st (PredicateExpr)
not_parse = do
    kw_parse "not"
    P.char '('
    wspace
    e <- expr_parse
    wspace
    P.char ')'
    return $ predNot e

term_parse :: P.CharParser st (PredicateExpr)
term_parse = do
    kw_parse "pred"
    P.char '('
    wspace
    nlabel <- parse_id
    wspace
    P.char ','
    wspace
    port <- parse_id
    wspace
    P.char ')'
    return $ PredicateTerm nlabel port

op_parse :: String -> ([PredicateExpr] -> PredicateExpr) -> P.CharParser st (PredicateExpr)
op_parse kw constructor = do
    kw_parse kw
    P.char '('
    l <- P.sepBy1 expr_parse (wspace >> P.char ',' >> wspace)
    P.char ')'
    return $ constructor l

expr_parse :: P.CharParser st (PredicateExpr)
expr_parse = do
              (kw_parse "true"  >> return PredicateTrue)
        P.<|> (kw_parse "false"  >> return PredicateFalse)
        P.<|> par_parse
        P.<|> not_parse
        P.<|> (op_parse "and" predAnd)
        P.<|> (op_parse "or" predOr)
        P.<|> term_parse

pred_parse :: P.CharParser st (PredicateExpr)
pred_parse = do
    wspace
    x <- expr_parse
    wspace
    P.eof
    return x

parseStr :: String -> PredicateExpr
parseStr input = case P.parse pred_parse "(top level)" input of
    Right x -> x
    Left err -> error $ "Could not parse: --" ++ input ++ "-- error:" ++ (show err)

parseFile :: FilePath -> IO (PredicateExpr)
parseFile fname = readFile fname >>= return . parseStr
