module Dragonet.Predicate (
    PredicateExpr(..),
    predAnd, predOr,
    predEval, predEquiv,
    initPredCompSt_, initPredCompSt, PredCompSt(..),
    computePred,
    parseFile, parseStr, predGetTerms
) where

import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NPort, Edge(..), NOperator(..), ESAttribute(..), NLabel)
import Dragonet.ProtocolGraph.Utils (getFNodeByName, edgeDeps, edgePort, spawnDeps, isSpawnTarget)
import qualified Util.GraphHelpers as GH

import Data.Maybe
import Debug.Trace (trace)
import Data.List (intercalate, lookup, concat)
import qualified Text.ParserCombinators.Parsec as P

-- Path predicates:
--  A path predicate is an expression with terms that in the form of:
--  (node, port of node)
--  They characterize the input (e.g., packet) of the graph based on which
--  choices would be made
--
-- The code in this module preforms similar operations with the semantic helpers
-- and the SMT solver -- both are essentially different approaches of doing the
-- same thing. Eventually, we need to choose one. The SMT solver is more
-- general, but potentially less performant.

traceMessages = False
xtrace = if traceMessages then trace else \a b -> b

pgSpawnEdgePred :: PGEdge -> PredicateExpr
pgSpawnEdgePred (_, _, ESpawn _ attrs) = expr
    where isPredAttr ::  ESAttribute -> Bool
          isPredAttr (ESAttrPredicate _) = True -- only this one for now

          parse :: ESAttribute -> PredicateExpr
          parse (ESAttrPredicate x) = parseStr $ xtrace ("parsing: " ++ x) x

          predicates = filter isPredAttr attrs
          predicates' = xtrace ("predicates are: " ++ (show predicates)) predicates
          exprs = map parse predicates'
          expr = case length exprs of
                      0 -> PredicateTrue
                      1 -> exprs !! 0
                      otherwise -> PredicateAnd exprs

--predicates from incoming spawn edges (combine them with and)
pgSpawnPreds :: PGraph -> PGNode -> PredicateExpr
pgSpawnPreds pg n = case length spawn_edges of
    0 -> PredicateTrue
    1 -> mapfn $ (spawn_edges !! 0)
    otherwise -> predAnd $ map mapfn spawn_edges
    where mapfn = (pgSpawnEdgePred . snd)
          spawn_edges = spawnDeps pg n


--
------- Path predicates -----
--

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

--
------- Predicate builders -----
--

-- interface
predAnd = predicateAND        -- these versions do folding
predOr  = predicateOR
predNot = predicateNOT
--predAnd = PredicateAnd      -- these versions do not
--predOr  = PredicateOr


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

-- evaluate (and hopefully simplify) a predicate expression given a set of term
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

--
------- Computing predicate epxressions -----
--

-- state for computing predicate expression:
-- compStop will stop the search when a particular type of node is met.
-- Normally, the search will stop when an entry node with no dependencies is
-- reached. However, when are searching in an embedded graph, we need to stop
-- when we reach the sink node.
data PredCompSt = PredCompSt {
      predGraph  :: PGraph
    , predDst    :: PGNode
    , predInPort :: Maybe NPort
    , compStop   :: PGNode -> Bool
} deriving (Show)

-- check whether two predicates are equivalent: simple equality for now
predEquiv :: PredicateExpr -> PredicateExpr -> Bool
predEquiv a b =  a == b

-- initialize state
initPredCompSt_ = PredCompSt {
      predGraph  = undefined
    , predDst    = undefined
    , predInPort = Nothing
    , compStop   = \x -> False
}

-- initializer helper
initPredCompSt :: PGraph -> String -> PredCompSt
initPredCompSt gr node = initPredCompSt_ {
      predGraph = gr
    , predDst   = getFNodeByName gr node
}

-- Compute a predicate expression for flows (packets, whatever) that start from
--  predSrc and reach predDst, in predGraph
--  (assumes no circles exist)
computePred :: PredCompSt -> PredicateExpr
-- compute predicate of src->dst, where dst is an FNode:
--  predicate of pre(dst) AND predicate of src->pre(dst)
computePred st@(PredCompSt { predDst = (_, FNode {}), compStop = stopfn})
    -- we reached an entry node
    | ndeps == 0   = pgSpawnPreds gr dst
    | ndeps > 1    = error $ "F-nodes have at most one incmming edge" ++ (nLabel $ snd dst)
    -- ndeps == 1
    | stopfn dn   = PredicateTrue
    | spawned = error "NYI: both normal and spawn edges" -- combines normal and spawn edges (treat is an OR?)
    -- recurse
    | otherwise   = expr
    where (dst', gr) = (predDst st, predGraph st)
          dst = dst' -- trace ("Visiting node: " ++ (nLabel $ snd dst')) dst'
          -- predecesors of destintation (going backwards)
          -- (only consider normal edges)
          spawned = isSpawnTarget gr dst
          deps    = edgeDeps gr dst
          ndeps   = length deps
          (dn,de) = deps !! 0
          port    = edgePort de
          nlabel  = nLabel (snd dn)
          newst   = st { predDst = dn, predInPort = Just port}
          expr'   = computePred newst
          term    = PredicateTerm nlabel port
          expr    = case dn of
                      (_, ONode {})            -> expr'
                      (_, FNode {nPorts = np}) -> if length np == 1
                                                     then expr'
                                                     else predAnd [expr', term]
-- compute predicate of src->dst, where dst is an ONode:
--  OP (e.g., AND) [ predicate of src -> pre ] for each pre in pre(dst)
computePred st@(PredCompSt { predDst = (_, ONode { nOperator = op })} ) = expr
    where inport = fromJust $ predInPort st
          -- predecessors of destintation (going backwards)
          predecessors = GH.labLPre (predGraph st) (predDst st)
          -- get predicates for all predecessors
          exprargs :: [PredicateExpr]
          exprargs = map getPred predecessors
          -- here we assume that O-nodes are not connecte in reverse
          -- (true->false, false->true)
          getPred :: (PGNode, PGEdge) -> PredicateExpr
          getPred (n,_) = computePred $ st {predDst = n, predInPort = Just "true"}
          -- predicate operand
          expr = case (inport, op) of
                  ("true", NOpAnd)  -> predAnd exprargs
                  ("true", NOpOr)   -> predOr exprargs
                  ("true", _)       -> error "NYI"
                  ("false", NOpAnd) -> predOr  $ map PredicateNot exprargs
                  ("false", NOpOr)  -> predAnd $ map PredicateNot exprargs
                  ("false", _)      -> error "NYI"
                  (_, _)            -> error $ "Expecting true/false, not:" ++ inport


--
------- silly parser for building predicate expression
--

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
