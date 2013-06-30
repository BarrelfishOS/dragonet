module DragonetDSL(dragonet) where

import System.IO


import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Maybe
import qualified Data.List as L

import qualified Operations as OP

import Text.ParserCombinators.Parsec

dragonet  :: QuasiQuoter
dragonet  =  QuasiQuoter { quoteExp = undefined,
                           quotePat = undefined,
                           quoteType = undefined,
                           quoteDec = quoteMyDec
}

-- That's where the magic happens: we get the string representing 
quoteMyDec s = do
    nodes <- parseGraph s
    return (map declare nodes)
    where
        portEdge e = TH.VarE (TH.mkName e)
        port (Port _ ns) = TH.ListE (map portEdge ns)
        binEdges t f = TH.AppE (TH.ConE (TH.mkName "OP.BinaryNode")) (TH.TupE [(port t), (port f)])
        edges (Node _ ps) = TH.AppE (TH.ConE (TH.mkName "OP.NaryNode")) (TH.ListE (map port ps))
        edges (Boolean _ t f) = binEdges t f
        edges (And _ t f) = binEdges t f
        edges (Or _ t f) = binEdges t f
        decNode n = TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getDecNode")) (TH.LitE (TH.StringL (nName n)))) (TH.LitE (TH.StringL ""))) (edges n)
        opNode n op = TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getOperatorNode")) (TH.LitE (TH.StringL op))) (TH.LitE (TH.StringL ""))) (edges n)

        node n =
            case n of
                (Node _ _) -> decNode n
                (Boolean _ _ _) -> decNode n
                (And _ _ _) -> opNode n "AND"
                (Or _ _ _) -> opNode n "OR"
        declare n = TH.FunD (TH.mkName (nName n)) [(TH.Clause [] (TH.NormalB (node n)) [])]
    
-----------------------------------------------------------------------------
-- Representing parsed code

data Port = Port String [String]
    deriving Show
data Node =
    Node String [Port] |
    Boolean String Port Port |
    And String Port Port |
    Or String Port Port
    deriving Show

nName (Node n _) = n
nName (Boolean n _ _) = n
nName (And n _ _) = n
nName (Or n _ _) = n


-----------------------------------------------------------------------------
-- Implements the actual parser

comment = many (spaces >> string "--" >> manyTill anyChar newline) >> return ()

mySpaces = spaces <|> comment

identifier = do
    id <- many1 alphaNum
    mySpaces
    return id
symbol s = string s >> mySpaces

port = do
    symbol "port"
    ns <- identifier `sepBy1` mySpaces
    symbol "["
    ds <- identifier `sepBy1` mySpaces
    symbol "]"
    return (ports ns ds)
    where
        port ds n = Port n ds
        ports ns ds = map (port ds) ns

node = do
    symbol "node"
    n <- identifier
    symbol "{"
    ps <- many port
    symbol "}"
    return (Node n (concat ps))

genBoolean name = do
    symbol name
    n <- identifier
    symbol "{"
    ps <- many port
    if (length (concat ps)) /= 2 then
        unexpected "Unexpected number of ports in boolean node, expect exactly 2"
    else
        if isNothing (truePort ps) then
            unexpected "true port not found in boolean node"
        else
            if isNothing (falsePort ps) then
                unexpected "false port not found in boolean node"
            else
                symbol "}"
    return (n, (head (concat ps)), (head (tail (concat ps))))
    where
        isPort n (Port m _) = (n == m)
        findPort n ps = L.find (isPort n) (concat ps)
        truePort = findPort "true"
        falsePort = findPort "false"

boolean = do
    (n, t, f) <- genBoolean "boolean"
    return (Boolean n t f)
        
orN = do
    (n, t, f) <- genBoolean "or"
    return (Or n t f)

andN = do
    (n, t, f) <- genBoolean "and"
    return (And n t f)
  
graph = do
    mySpaces
    ns <- many (node <|> boolean <|> orN <|> andN)
    mySpaces
    eof
    return ns

parseGraph s = case runParser graph () "" s of
      Left err  -> fail $ show err
      Right e   -> return e

