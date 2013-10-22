{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Dragonet.Unicorn.Parser(
    Graph(..),
    Cluster(..),
    Port(..),
    Node(..),

    pName,
    nName,
    nAttrs,
    nPorts,

    parseGraph,
) where

import Data.Maybe
import Data.Either
import qualified Data.List as L

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

-----------------------------------------------------------------------------
-- Representing parsed code

data Graph = Graph String Cluster
data Cluster = Cluster String [Cluster] [Node]

data Port = Port String [String]
    deriving Show
data Node =
    Node String [Port] [String] |
    Config String [Port] [String] (Maybe String) |
    Boolean String Port Port [String] |
    And String Port Port [String] |
    NAnd String Port Port [String] |
    Or String Port Port [String] |
    NOr String Port Port [String]
    deriving Show


pName :: Port -> String
pName (Port n _) = n

nName :: Node -> String
nName (Node n _ _) = n
nName (Config n _ _ _) = n
nName (Boolean n _ _ _) = n
nName (And n _ _ _) = n
nName (NAnd n _ _ _) = n
nName (Or n _ _ _) = n
nName (NOr n _ _ _) = n

nAttrs :: Node -> [String]
nAttrs (Node _ _ as) = as
nAttrs (Config _ _ as _) = as
nAttrs (Boolean _ _ _ as) = as
nAttrs (And _ _ _ as) = as
nAttrs (NAnd _ _ _ as) = as
nAttrs (Or _ _ _ as) = as
nAttrs (NOr _ _ _ as) = as

nPorts :: Node -> [Port]
nPorts (Node _ ps _) = ps
nPorts (Config _ ps _ _) = ps
nPorts (Boolean _ a b _) = [a, b]
nPorts (And _ a b _) = [a, b]
nPorts (NAnd _ a b _) = [a, b]
nPorts (Or _ a b _) = [a, b]
nPorts (NOr _ a b _) = [a, b]


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
                       "nand", "or", "nor", "gconfig", "port", "attr"],
    P.reservedOpNames = [],
    P.caseSensitive = True }

whitespace = P.whiteSpace lexer
identifier = P.identifier lexer
braces = P.braces lexer
brackets = P.brackets lexer
reserved = P.reserved lexer
stringLiteral = P.stringLiteral lexer


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

genNaryNode p name = do
    reserved name
    n <- cIdentifier p
    (ps,as) <- braces $ do
        as' <- many attributes
        ps' <- many $ port p
        cs <- many constraint
        let attrs = as' ++ constraintAttrs cs
        return (ps',attrs)
    return (n,(concat ps),as)


node p = do
    (name,ports,attr) <- genNaryNode p "node"
    return (Right (Node name ports attr))

configFun = do
    reserved "function"
    f <- identifier
    return f

config p = do
    reserved "config"
    n <- cIdentifier p
    (ps,as,iF) <- braces $ do
        iF' <- optionMaybe configFun
        as' <- many attributes
        ps' <- many $ port p
        return (ps',as',iF')
    --return (n,(concat ps),as)
    return (Right (Config n (concat ps) as iF))

genBoolean p name hasConstraints = do
    reserved name
    n <- cIdentifier p
    (ps,as) <- braces $ do
        as' <- many attributes
        ps' <- many $ port p
        cs <- if hasConstraints then many constraint else return []
        let attrs = as' ++ constraintAttrs cs
        return (ps',attrs)
    if (length (concat ps)) /= 2 then
        unexpected "Unexpected number of ports in boolean node, expect exactly 2"
    else
        if isNothing (truePort ps) then
            unexpected "true port not found in boolean node"
        else
            if isNothing (falsePort ps) then
                unexpected "false port not found in boolean node"
            else
                return ()
    return (n, (head (concat ps)), (head (tail (concat ps))),as)
    where
        isPort n (Port m _) = (n == m)
        findPort n ps = L.find (isPort n) (concat ps)
        truePort = findPort "true"
        falsePort = findPort "false"

boolean p = do
    (n, t, f, a) <- genBoolean p "boolean" True
    return (Right (Boolean n t f a))

orN p = do
    (n, t, f, a) <- genBoolean p "or" False
    return (Right (Or n t f a))

norN p = do
    (n, t, f, a) <- genBoolean p "nor" False
    return (Right (NOr n t f a))

andN p = do
    (n, t, f, a) <- genBoolean p "and" False
    return (Right (And n t f a))

nandN p = do
    (n, t, f, a) <- genBoolean p "nand" False
    return (Right (NAnd n t f a))

cluster p = do
    reserved "cluster"
    n <- identifier
    ns <- braces $ clusteredNodes (n:p)
    return (Left (Cluster ((concat $ reverse p) ++ n) (lefts ns) (rights ns)))

clusteredNodes p = do
    ns <- many (node p <|> config p <|> boolean p <|> orN p <|> norN p <|>
                andN p <|> nandN p <|> cluster p)
    return $ ns

graph = do
    whitespace
    reserved "graph"
    gn <- identifier
    ns <- braces $ clusteredNodes []
    eof
    return (Graph gn (Cluster "" (lefts ns) (rights ns)))

parseGraph s = case runParser graph () "" s of
      Left err  -> fail $ show err
      Right e   -> return e

