-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Util.SMTLibParser (
    parseScriptFile,
    parseScript,
    parseTerm,

    scriptParser,
    termParser
) where

import qualified SMTLib2 as S
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Language as PL
import qualified Text.ParserCombinators.Parsec.Token as PT
import qualified Text.Parsec.Prim as PP
import Data.Ratio ((%))
import Data.Functor ((<$>))
import Data.String (fromString)

symbolChars = oneOf "+-/*=%?!.$_~&^<>@"

lexer = PT.makeTokenParser $ PL.emptyDef {
    PT.commentLine = ";",
    PT.identStart = letter <|> symbolChars,
    PT.identLetter = alphaNum <|> symbolChars,
    PT.reservedNames =
        ["!", "_", "as", "DECIMAL", "exists", "forall", "let", "par",
            -- Commands:
            "set-logic", "set-option", "set-info", "declare-sort",
            "define-sort", "declare-fun", "define-fun", "push", "pop", "assert",
            "check-sat", "get-assertions", "get-proof", "get-unsat-core",
            "get-value", "get-assignment", "get-option", "get-info", "exit",
            -- Command options:
            ":print-success", ":expand-definitions", ":interactive-mode",
            ":produce-proofs", ":produce-unsat-cores", ":produce-models",
            ":produce-assignments", ":regular-output-channel",
            ":diagnostic-output-channel", ":random-seed", ":verbosity",
            -- Info flags:
            ":error-behavior", ":name", ":authors", ":version", ":status",
            ":reason-unknown", ":all-statistics"] }

parens = PT.parens lexer


-------------------------------------------------------------------------------
-- Tokens

toUnreserved = PT.symbol lexer

toReserved = PT.reserved lexer

-- <numeral>
toNumeral = PT.decimal lexer

-- <numeral> | <decimal>
toNumOrDec = do
    i <- PT.decimal lexer
    x <- option (Left i) $ do
        char '.'
        fs <- many digit
        let f  = read fs
            tf = (10 :: Integer) ^ (length fs)
        return $ Right ((i * tf + f) % tf)
    PT.whiteSpace lexer
    return x


-- <hexadecimal>
toHexadecimal = do
    string "#x"
    digs <- many hexDigit
    let i = read ("0x" ++ digs)
    return (i, fromIntegral $ (length digs) * 4)

-- <binary>
toBinary = do
    string "#b"
    digs <- many $ oneOf "01"
    PT.whiteSpace lexer
    let i = sum $ zipWith (\a b -> (2^a) * (read [b]))  [0..] $ reverse digs
    return (i, fromIntegral $ length digs)

-- <string>
toString = PT.stringLiteral lexer

-- <symbol>
toSymbol = toSimpleSymbol <|> toQuotedSymbol
toSimpleSymbol = PT.identifier lexer
toQuotedSymbol = do
    char '|'
    manyTill (noneOf "\\") (char '|')

-- <keyword>
toKeyword = do
    char ':'
    k <- many (letter <|> symbolChars)
    PT.whiteSpace lexer
    return k


-------------------------------------------------------------------------------
-- S-expressions

-- <spec_constant>
seSpecConstant = lNumOrDec <|> lHexadecimal <|> lBinary <|> lString
    where
        lNumOrDec = do
            lr <- toNumOrDec
            return $
                case lr of
                    Left i  -> S.LitNum i
                    Right f -> S.LitFrac f
        lHexadecimal = do
            (i,b) <- toHexadecimal
            return $ S.LitBV i b
        lBinary = do
            (i,b) <- toBinary
            return $ S.LitBV i b
        lString = S.LitStr <$> toString

--seExpr = seExpConstant <|> seExpSymbol <|> seExpKeyword <|> seExpApp


-------------------------------------------------------------------------------
-- Identifiers

-- <identifier>
idIdentifier = idUnindexed <|> idIndexed

idUnindexed = do
    i <- toSymbol
    return $ S.I (S.N i) []

idIndexed = parens $ do
    toReserved "_"
    i <- toSymbol
    is <- many1 toNumeral
    return $ S.I (S.N i) is


-------------------------------------------------------------------------------
-- Sorts

-- <sort>
soSort = try soUnparameterized <|> soParameterized

soUnparameterized = do
    i <- idIdentifier
    -- TODO: Haskell representation has distinction between variable and app
    return $ S.TApp i []

soParameterized = parens $ do
    i <- idIdentifier
    ps <- many1 soSort
    return $ S.TApp i ps


-------------------------------------------------------------------------------
-- Attributes

-- <attribute_value>
atAttributeValue = atAVSpecConstant <|> atAVSymbol -- <|> atAVSExp

atAVSpecConstant = S.Lit <$> seSpecConstant

atAVSymbol = do
    s <- toSymbol
    return $ S.App (fromString s) Nothing []

-- TODO: no idea how to implement this with the current haskell representation
-- using Expr for attr values, while the specification allows more general
-- S-expressions as values (especially also with a non-trivial expression for
-- the first value in an S-expression, which is limited to an identifier in the
-- Haskell representation).
--atAVSExp = parens $ do

-- <attribute>
atAttribute = do
    kw <- toKeyword
    av <- optionMaybe $ atAttributeValue
    return $ S.Attr {
            S.attrName = S.N kw,
            S.attrVal  = av }


-------------------------------------------------------------------------------
-- Terms

-- <qual_identifier>
teQualIdentifiers = teIdentUntyped <|> (parens teIdentTyped)

-- Part of <qual_identifier>
teIdentUntyped = do
    i <- idIdentifier
    return $ S.App i Nothing []

-- Part of <qual_identifier> (without parens)
teIdentTyped = do
    toReserved "as"
    i <- idIdentifier
    s <- soSort
    return $ S.App i (Just s) []

-- <var_binding>
teVarBinding = parens $ do
    s <- toSymbol
    t <- teTerm
    return $ S.Defn {
            S.defVar  = S.N s,
            S.defExpr = t }

-- <sorted_var>
teSortedVar = parens $ do
    sy <- toSymbol
    so <- soSort
    return $ S.Bind {
            S.bindVar  = S.N sy,
            S.bindType = so }

-- <term> (restructured to avoid backtracking during parsing)
teTerm = try teSpecConst <|> try teIdentUntyped <|> parens teParTerm

-- Parts of <term> that include paretheses
teParTerm =
    teIdentTyped <|> teApplication <|> teLet <|> teQuantifier <|>
        teAttributed

teSpecConst = S.Lit <$> seSpecConstant

teApplication = do
    (S.App i t _) <- teQualIdentifiers
    ps <- many1 $ teTerm
    return $ S.App i t ps

teLet = do
    toReserved "let"
    bs <- parens $ many1 teVarBinding
    t <- teTerm
    return $ S.Let bs t

teQuantifier = do
    q <- do { toReserved "forall" ; return S.Forall } <|>
         do { toReserved "exists" ; return S.Exists }
    sv <- parens $ many1 teSortedVar
    t <- teTerm
    return $ S.Quant q sv t

teAttributed = do
    toReserved "!"
    t <- teTerm
    as <- many1  atAttribute
    return $ S.Annot t as


-------------------------------------------------------------------------------
-- Command options

-- <b_value>
coBValue =
    (do { toUnreserved "true" ; return True }) <|>
    (do { toUnreserved "false"; return False })

-- <option>
coOption = coBoolOpt <|> coStrOpt <|> coIntOpt <|> coAttrOpt

coBoolOpt = choice $ flip map boolOpts $ \(s,f) -> do
    toReserved s
    f <$> coBValue
    where
        boolOpts = [
            (":print-success", S.OptPrintSuccess),
            (":expand-definitions", S.OptExpandDefinitions),
            (":interactive-mode", S.OptInteractiveMode),
            (":produce-proofs", S.OptProduceProofs),
            (":produce-unsat-cores", S.OptProduceUnsatCores),
            (":produce-models", S.OptProduceModels),
            (":produce-assignments", S.OptProduceAssignments)]

coStrOpt = choice $ flip map strOpts $ \(s,f) -> do
    toReserved s
    f <$> toString
    where
        strOpts = [
            (":regular-output-channel", S.OptRegularOutputChannel),
            (":diagnostic-output-channel", S.OptDiagnosticOutputChannel)]

coIntOpt = choice $ flip map intOpts $ \(s,f) -> do
    toReserved s
    f <$> toNumeral
    where
        intOpts = [
            (":random-seed", S.OptRandomSeed),
            (":verbosity", S.OptVerbosity)]

coAttrOpt = S.OptAttr <$> atAttribute


-------------------------------------------------------------------------------
-- Info flags

inInfoFlag = inFixedFlag <|> inKeyword

inFixedFlag = choice $ flip map flags $ \(s,f) -> do
    toReserved s
    return f
    where
        flags = [
            (":error-behavior", S.InfoErrorBehavior),
            (":name", S.InfoName),
            (":authors", S.InfoAuthors),
            (":version", S.InfoVersion),
            (":status", S.InfoStatus),
            (":reason-unknown", S.InfoReasonUnknown),
            (":all-statistics", S.InfoAllStatistics)]

inKeyword = do
    kw <- toKeyword
    return $ S.InfoAttr $ S.Attr {
            S.attrName = S.N kw,
            S.attrVal  = Nothing }


-------------------------------------------------------------------------------
-- Commands

-- <command>
-- TODO: get-assignment is missing (because it's missing in Haskell
--       representation)
coCommand = parens $
    (coSetLogic <|> coSetOption <|> coSetInfo <|> coDeclareSort <|>
    coDefineSort <|> coDeclareFun <|> coDefineFun <|> coPush <|> coPop <|>
    coAssert <|> coCheckSat <|> coGetAssertions <|> coGetProof <|>
    coGetUnsatCore <|> coGetValue <|> coGetAssignment <|> coGetOption <|>
    coGetInfo <|> coExit)

coSetLogic = do
    toReserved "set-logic"
    sy <- toSymbol
    return $ S.CmdSetLogic $ S.N sy

coSetOption = do
    toReserved "set-option"
    opt <- coOption
    return $ S.CmdSetOption opt

coSetInfo = do
    toReserved "set-info"
    a <- atAttribute
    return $ S.CmdSetInfo a

coDeclareSort = do
    toReserved "declare-sort"
    sy <- toSymbol
    n <- toNumeral
    return $ S.CmdDeclareType (S.N sy) n

coDefineSort = do
    toReserved "define-sort"
    sy <- toSymbol
    ps <- map S.N <$> (parens $ many toSymbol)
    so <- soSort
    return $ S.CmdDefineType (S.N sy) ps so

coDeclareFun = do
    toReserved "declare-fun"
    sy <- toSymbol
    ps <- parens $ many soSort
    so <- soSort
    return $ S.CmdDeclareFun (S.N sy) ps so

coDefineFun = do
    toReserved "define-fun"
    sy <- toSymbol
    ps <- parens $ many teSortedVar
    so <- soSort
    te <- teTerm
    return $ S.CmdDefineFun (S.N sy) ps so te

coPush = do
    toReserved "push"
    i <- toNumeral
    return $ S.CmdPush i

coPop = do
    toReserved "pop"
    i <- toNumeral
    return $ S.CmdPop i

coAssert = do
    toReserved "assert"
    t <- teTerm
    return $ S.CmdAssert t

coCheckSat = do
    toReserved "check-sat"
    return S.CmdCheckSat

coGetAssertions = do
    toReserved "get-assertions"
    return S.CmdGetAssertions

coGetProof = do
    toReserved "get-proof"
    return S.CmdGetProof

coGetUnsatCore = do
    toReserved "get-unsat-core"
    return S.CmdGetUnsatCore

coGetValue = do
    toReserved "get-value"
    ts <- parens $ many1 teTerm
    return $ S.CmdGetValue ts

coGetAssignment = do
    toReserved "get-assignment"
    return S.CmdGetAssignment

coGetOption = do
    toReserved "get-option"
    kw <- toKeyword
    return $ S.CmdGetOption $ S.N kw

coGetInfo = do
    toReserved "get-info"
    i <- inInfoFlag
    return $ S.CmdGetInfo i

coExit = do
    toReserved "exit"
    return S.CmdExit

coScript = S.Script <$> many coCommand



-------------------------------------------------------------------------------
-- Public interface

parseScriptFile :: FilePath -> IO (Either ParseError S.Script)
parseScriptFile path = do
    script <- readFile path
    return $ parseScript path script

parseScript :: String -> String -> Either ParseError S.Script
parseScript name input = parse p name input
    where p = do { s <- coScript; eof; return s }

parseTerm :: String -> String -> Either ParseError S.Expr
parseTerm name input = parse teTerm name input
    where p = do { s <- teTerm; eof; return s }


scriptParser = coScript
termParser = teTerm

