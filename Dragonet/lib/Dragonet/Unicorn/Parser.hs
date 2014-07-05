{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Dragonet.Unicorn.Parser(
    Graph(..),
    Cluster(..),
    Port(..),
    Node(..),

    nAllPorts,

    parseGraph,
) where

import Data.Maybe
import Data.Either
import qualified Data.List as L
import Data.Bits (shiftL)
import Data.Functor ((<$>))

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Dragonet.ProtocolGraph (ConfType(..))

import qualified Util.SMTLibParser as SMTP
import qualified SMTLib2 as SMT

-----------------------------------------------------------------------------
-- Representing parsed code

data Graph = Graph {
        gName        :: String,
        gRootCluster :: Cluster,
        gSemHelpers  :: SMT.Script }
    deriving Show

data Cluster = Cluster {
        cName     :: String,
        cChildren :: [Cluster],
        cNodes    :: [Node] }
    deriving Show

data Port = Port {
        pName     :: String,
        pOuts     :: [String] }
    deriving Show

data Node =
    Node {
        nName     :: String,
        nPorts    :: [Port],
        nAttrs    :: [String],
        nPortSems :: [(String, SMT.Expr)] } |

    Config {
        nName     :: String,
        nPorts    :: [Port],
        nAttrs    :: [String],
        nConfFun  :: Maybe String,
        nConfType :: ConfType } |

    Boolean {
        nName     :: String,
        nPortT    :: Port,
        nPortF    :: Port,
        nAttrs    :: [String],
        nPortSems :: [(String, SMT.Expr)] } |

    And {
        nName     :: String,
        nPortT    :: Port,
        nPortF    :: Port,
        nAttrs    :: [String] } |

    NAnd {
        nName     :: String,
        nPortT    :: Port,
        nPortF    :: Port,
        nAttrs    :: [String] } |

    Or {
        nName     :: String,
        nPortT    :: Port,
        nPortF    :: Port,
        nAttrs    :: [String] } |

    NOr {
        nName     :: String,
        nPortT    :: Port,
        nPortF    :: Port,
        nAttrs    :: [String] }
    deriving Show


nAllPorts :: Node -> [Port]
nAllPorts Boolean { nPortT = t, nPortF = f } = [t, f]
nAllPorts And { nPortT = t, nPortF = f } = [t, f]
nAllPorts NAnd { nPortT = t, nPortF = f } = [t, f]
nAllPorts Or { nPortT = t, nPortF = f } = [t, f]
nAllPorts NOr { nPortT = t, nPortF = f } = [t, f]
nAllPorts n = nPorts n


-----------------------------------------------------------------------------
-- Implements the actual parser



lexer = P.makeTokenParser P.LanguageDef {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = False,
    P.identStart = Parsec.letter <|> Parsec.char '_',
    P.identLetter = Parsec.alphaNum <|> Parsec.char '_',
    P.opStart = Parsec.oneOf "",
    P.opLetter = Parsec.oneOf "",
    P.reservedNames = ["graph", "cluster", "node", "config", "boolean", "and",
                       "nand", "or", "nor", "gconfig", "port", "attr", "type",
                       "semantics", "helpers"],
    P.reservedOpNames = [],
    P.caseSensitive = True }

whitespace = P.whiteSpace lexer
identifier = P.identifier lexer
braces = P.braces lexer
brackets = P.brackets lexer
angles = P.angles lexer
parens = P.parens lexer
reserved = P.reserved lexer
stringLiteral = P.stringLiteral lexer
pInteger = P.integer lexer
pNatural = P.natural lexer
symbol = P.symbol lexer

globalIdentifier p = do
    _ <- char '.'
    if null p then
        unexpected "Invalid cluster reference (too many .)"
    else do
        n <- cIdentifier $ tail p
        return $ n

localIdentifier p = do
    n <- identifier
    return $ (concat $ reverse p) ++ n

cIdentifier p = globalIdentifier p <|> localIdentifier p
--cIdentifier p = T.trace ("cIdentifier " ++ (show p)) (cIdentifierRec p)

port p = do
    reserved "port"
    ns <- many $ identifier
    ds <- brackets $ many $ cIdentifier p
    return (ports ns ds)
    where
        ports ns ds = map (flip Port ds) ns

attributes = do
    reserved "attr"
    stringLiteral

constraint = do
    reserved "constraint"
    p <- identifier
    e <- stringLiteral
    return (p,e)

constraintAttrs :: [(String,String)] -> [String]
constraintAttrs = map (\(p,e) -> "C." ++ p ++ ":" ++ e)

semantics = do
    reserved "semantics"
    ps <- many1 identifier
    sem <- braces $ SMTP.termParser
    return $ map (\p -> (p,sem)) ps

genNaryNode p name = do
    reserved name
    n <- cIdentifier p
    (ps,as,sems) <- braces $ do
        as' <- many attributes
        ps' <- concat <$> many (port p)
        cs <- many constraint
        let attrs = as' ++ constraintAttrs cs
        sems <- concat <$> many semantics
        return (ps',attrs,sems)
    return (n,ps,as,sems)


node p = do
    (name,ports,attr,sems) <- genNaryNode p "node"
    return $ Right $ Node {
            nName     = name,
            nPorts    = ports,
            nAttrs    = attr,
            nPortSems = sems }

configFun = do
    reserved "function"
    f <- identifier
    return f


configType = do
    reserved "type"
    configType'

config p = do
    reserved "config"
    n <- cIdentifier p
    (ps,as,iF,ctt) <- braces $ do
        mbCtt <- optionMaybe configType
        iF' <- optionMaybe configFun
        as' <- many attributes
        ps' <- concat <$> many (port p)
        let ctt' = defaultType ps' `fromMaybe` mbCtt
        return (ps',as',iF',ctt')
    return $ Right $ Config {
            nName     = n,
            nPorts    = ps,
            nAttrs    = as,
            nConfFun  = iF,
            nConfType = ctt }
    where
        defaultType ps = CTEnum { ctEnumerators = map pName ps }

genBoolean p name hasConstraints = do
    reserved name
    n <- cIdentifier p
    (ps,as,sems) <- braces $ do
        as' <- many attributes
        ps' <- concat <$> many (port p)
        cs <- if hasConstraints then many constraint else return []
        sems <- if hasConstraints
                then concat <$> many semantics
                else return []
        let attrs = as' ++ constraintAttrs cs
        return (ps',attrs,sems)
    if (length ps) /= 2 then
        unexpected "Unexpected number of ports in boolean node, expect exactly 2"
    else
        if isNothing (truePort ps) then
            unexpected "true port not found in boolean node"
        else
            if isNothing (falsePort ps) then
                unexpected "false port not found in boolean node"
            else
                return ()
    return (n, (head ps), (head (tail ps)),as, sems)
    where
        isPort n p = n == pName p
        findPort n ps = L.find (isPort n) ps
        truePort = findPort "true"
        falsePort = findPort "false"

boolean p = do
    (n, t, f, a, s) <- genBoolean p "boolean" True
    return $ Right $ Boolean {
            nName     = n,
            nPortT    = t,
            nPortF    = f,
            nAttrs    = a,
            nPortSems = s }


orN p = do
    (n, t, f, a, _) <- genBoolean p "or" False
    return $ Right $ Or {
            nName  = n,
            nPortT = t,
            nPortF = f,
            nAttrs = a }


norN p = do
    (n, t, f, a, _) <- genBoolean p "nor" False
    return $ Right $ NOr {
            nName  = n,
            nPortT = t,
            nPortF = f,
            nAttrs = a }


andN p = do
    (n, t, f, a, _) <- genBoolean p "and" False
    return $ Right $ And {
            nName  = n,
            nPortT = t,
            nPortF = f,
            nAttrs = a }

nandN p = do
    (n, t, f, a, _) <- genBoolean p "nand" False
    return $ Right $ NAnd {
            nName  = n,
            nPortT = t,
            nPortF = f,
            nAttrs = a }

cluster p = do
    reserved "cluster"
    n <- identifier
    ns <- braces $ clusteredNodes (n:p)
    return $ Left $ Cluster {
            cName     = (concat $ reverse p) ++ n,
            cChildren = lefts ns,
            cNodes    = rights ns }

clusteredNodes p = do
    ns <- many (node p <|> config p <|> boolean p <|> orN p <|> norN p <|>
                andN p <|> nandN p <|> cluster p)
    return $ ns

helpers = do
    reserved "helpers"
    braces $ SMTP.scriptParser

graph = do
    whitespace
    reserved "graph"
    gn <- identifier
    (ns,helpers) <- braces $ do
        hs <- option (SMT.Script []) helpers
        ns <- clusteredNodes []
        return (ns, hs)
    eof
    return $
        Graph {
            gName        = gn,
            gRootCluster =
                Cluster {
                    cName     = "",
                    cChildren = lefts ns,
                    cNodes    = rights ns },
            gSemHelpers  = helpers }

parseGraph s = case runParser graph () "" s of
      Left err  -> fail $ show err
      Right e   -> return e



--------------------------------------------------------------------------------
-- Parser for configuration input type
--
-- Syntax:
--   Bool                        Boolean
--   Int min-value max-value     Integer in specified range
--   UInt bits                   Unsigned integer with `bits' bits
--   SInt bits                   Signed 2's complement integer with `bits' bits
--   Enum (ENUM1,ENUM2,...)      Enumeration type
--   <T>                         Maybe T (T or nothing)
--   [T]<min-len,max-len>        Ordered list of T   (<constraints> optional)
--   {T}<min-len,max-len>        Unordered list of T (<constraints> optional)
--   (lbl1: T, lbl2: T,...)      Tuple
--   |<lbl1: T, lbl2: T,...>     Sum type (tagged union)
--
-- Len constraints for lists (can be omitted completely)
-- let n = len(list) in:
--   <l,u> l <= n <= u
--   <l,>  l <= n
--   <,u>       n <= u
--   <x>    n == x
--

cfgUInt = do
    symbol "UInt"
    bits <- pNatural
    if bits < 1
        then unexpected "At least 1 bit is required for UInt"
        else return ()
    return $ CTInteger {
        ctMin = 0,
        ctMax = (shiftL 1 $ fromIntegral bits) - 1 }

cfgSInt = do
    symbol "SInt"
    bits <- pNatural
    if bits < 2
        then unexpected "At least 2 bits are required for SInt"
        else return ()
    let p = shiftL 1 $ fromIntegral (bits - 1)
    return $ CTInteger {
        ctMin = -p,
        ctMax = p - 1 }

cfgInt = do
    symbol "Int"
    lower <- pInteger
    upper <- pInteger
    if lower >= upper
        then unexpected "Lower bound must be below upper bound"
        else return ()
    return $ CTInteger {
        ctMin = lower,
        ctMax = upper }

cfgBool = do
    symbol "Bool"
    return $ CTBool

cfgMaybe = angles $ do
    t <- configType'
    return $ CTMaybe {
        ctElement = t }

-- Parse list length (n) constraints:
cfgListConstraints = do
    mb <- optionMaybe $ angles $ do
        mbL <- optionMaybe pNatural
        case mbL of
            Nothing -> do -- No initial integer, must be <,upper>
                symbol ","
                u <- pNatural
                return (Nothing, Just u)

            Just l -> do
                mbC <- optionMaybe $ symbol ","
                if isNothing mbC
                    then return (Just l, Just l)
                    else do
                        mbU <- optionMaybe pNatural
                        return (Just l, mbU)
    return $ fromMaybe (Nothing,Nothing) mb

cfgUList = do
    t <- braces $ configType'
    (mbMin,mbMax) <- cfgListConstraints
    return $ CTList {
        ctElement = t,
        ctOrdered = False,
        ctLenMin  = mbMin,
        ctLenMax  = mbMax }

cfgOList = do
    t <- brackets $ configType'
    (mbMin,mbMax) <- cfgListConstraints
    return $ CTList {
        ctElement = t,
        ctOrdered = True,
        ctLenMin  = mbMin,
        ctLenMax  = mbMax }

cfgTupleEl = do
    l <- identifier
    symbol ":"
    t <- configType'
    return (l, t)

cfgTuple = parens $ do
    es <- cfgTupleEl `sepBy1` (symbol ",")
    return $ CTTuple {
        ctElements = es }

cfgSum = parens $ do
    es <- cfgTupleEl `sepBy1` (symbol ",")
    return $ CTSum {
        ctElements = es }

cfgEnum = do
    symbol "Enum"
    cs <- parens (identifier `sepBy1` (symbol ","))
    return $ CTEnum {
        ctEnumerators = cs }

configType' =
    cfgUInt <|> cfgSInt <|> cfgInt <|> cfgBool <|> cfgMaybe <|> cfgUList
        <|> cfgOList <|> cfgTuple <|> cfgEnum <|> cfgSum
