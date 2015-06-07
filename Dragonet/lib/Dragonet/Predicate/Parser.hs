-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.Predicate.Parser (
    predParser,
    parseStr_,
    parseFile_,
) where

import Dragonet.Predicate.Definitions (PredExpr(..), PredBuild(..))

import qualified Text.ParserCombinators.Parsec as P
import Control.Applicative ((<$>))

--
------- silly parser for building predicate expression
--

wspace :: P.CharParser PredBuild ()
wspace = P.skipMany (P.oneOf " \t")

parse_id :: P.CharParser PredBuild String
parse_id = do
    c <- P.letter
    cs <- P.many (P.alphaNum P.<|> P.char '_')
    return $ c:cs

kw_parse :: String -> P.CharParser PredBuild ()
kw_parse kw = do
    P.string kw
    P.notFollowedBy P.alphaNum

par_parse :: P.CharParser PredBuild (PredExpr)
par_parse = do
    P.char '('
    e <- expr_parse
    P.char ')'
    return e

not_parse :: P.CharParser PredBuild (PredExpr)
not_parse = do
    bNOT <- buildNOT <$> P.getState
    kw_parse "not"
    P.char '('
    wspace
    e <- expr_parse
    wspace
    P.char ')'
    return $ bNOT e

term_parse :: P.CharParser PredBuild (PredExpr)
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
    return $ PredAtom nlabel port

op_parse :: String -> ([PredExpr] -> PredExpr) -> P.CharParser PredBuild (PredExpr)
op_parse kw constructor = do
    kw_parse kw
    P.char '('
    l <- P.sepBy1 expr_parse (wspace >> P.char ',' >> wspace)
    P.char ')'
    return $ constructor l

expr_parse :: P.CharParser PredBuild (PredExpr)
expr_parse = do
        bAND <- buildAND <$> P.getState
        bOR  <- buildOR  <$> P.getState
        x <- (kw_parse "true"  >> return PredTrue)
                P.<|> (kw_parse "false"  >> return PredFalse)
                P.<|> par_parse
                P.<|> not_parse
                P.<|> (op_parse "and" bAND)
                P.<|> (op_parse "or" bOR)
                P.<|> term_parse
        return x

predParser :: P.CharParser PredBuild (PredExpr)
predParser = do
    wspace
    x <- expr_parse
    wspace
    P.eof
    return x

parseStr_ :: PredBuild -> String -> PredExpr
parseStr_ builders input = case P.runParser predParser builders "(top level)" input of
    Right x -> x
    Left err -> error $ "Could not parse: --" ++ input ++ "-- error:" ++ (show err)


parseFile_ :: PredBuild -> FilePath -> IO (PredExpr)
parseFile_ bld fname = readFile fname >>= return . (parseStr_ bld)
