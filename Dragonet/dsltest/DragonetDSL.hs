module DragonetDSL(
    dragonet,
    dragonet_f,
    dragonetImpl,
    dragonetImpl_f,

) where

import System.IO

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Maybe
import qualified Data.List as L

import qualified Operations as OP




dragonet  :: QuasiQuoter
dragonet  =  QuasiQuoter { quoteExp = undefined,
                           quotePat = undefined,
                           quoteType = undefined,
                           quoteDec = quoteMyDec
}

dragonetImpl  :: QuasiQuoter
dragonetImpl  =  QuasiQuoter { quoteExp = undefined,
                           quotePat = undefined,
                           quoteType = undefined,
                           quoteDec = quoteMyDecImpl
}


dragonet_f :: QuasiQuoter
dragonet_f = quoteFile dragonet

dragonetImpl_f :: QuasiQuoter
dragonetImpl_f = quoteFile dragonetImpl



-- That's where the magic happens: we get the string representing the DSL input
-- and generate a Haskell AST
quoteMyDec s = do
    nodes <- parseGraph s
    return (map declare nodes)

quoteMyDecImpl s = do
    nodes <- parseGraph s
    return ((map declare nodes) ++ (map implMap nodes))


declare n =
    TH.FunD (TH.mkName (nName n)) [(TH.Clause [] (TH.NormalB (node n)) [])]
    where
        portEdge e = TH.VarE (TH.mkName e)
        port (Port _ ns) = TH.ListE (map portEdge ns)
        binEdges t f =
            TH.AppE (TH.ConE (TH.mkName "OP.BinaryNode")) $
                TH.TupE [(port t), (port f)]
        lblPort p = TH.TupE [(TH.LitE $ TH.StringL name), (port p)]
            where (Port name _) = p
        edges (Node l ps) =
            TH.AppE (TH.ConE (TH.mkName "OP.NaryNode")) $
                TH.ListE $ map lblPort ps
        edges (Boolean _ t f) = binEdges t f
        edges (And _ t f) = binEdges t f
        edges (Or _ t f) = binEdges t f
        decNode n = TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getDecNode")) (TH.LitE (TH.StringL (nName n)))) (TH.LitE (TH.StringL ""))) (edges n)
        opNode n op = TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getOperatorNode")) (TH.LitE (TH.StringL op))) (TH.LitE (TH.StringL ""))) (edges n)

        node n =
            case n of
                (Node _ _) -> decNode n
                (Boolean _ _ _) -> decNode n
                (And l _ _) -> opNode n ("AND:" ++ l)
                (Or l _ _) -> opNode n ("OR:" ++ l)


implMap n =
    TH.FunD (implNName $ nName n) [(TH.Clause [] (TH.NormalB (code)) [])]
    where
        edge e = TH.VarE $ implNName e
        port (Port pn es)  = TH.TupE [TH.LitE $ TH.StringL pn, TH.ListE $ map edge es]
        plistExp = TH.ListE $ map port $ nPorts n
        nodeExp = TH.VarE $ TH.mkName $ nName n
        implExp =
            if nIsOp n then
                TH.ConE $ TH.mkName "Nothing"
            else
                TH.AppE (TH.ConE $ TH.mkName "Just") $ TH.VarE implName
        implNodeExp = TH.ConE $ TH.mkName "ImplNode"
        code = foldl TH.AppE implNodeExp [nodeExp, implExp, plistExp]
        implName = TH.mkName (nName n ++ "Impl")
        implNName m = TH.mkName  (m ++ "ImplNode")
    
    
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

nIsOp (And n _ _) = True
nIsOp (Or n _ _) = True
nIsOp _ = False

nName (Node n _) = n
nName (Boolean n _ _) = n
nName (And n _ _) = n
nName (Or n _ _) = n

nPorts :: Node -> [Port]
nPorts (Node _ ps) = ps
nPorts (Boolean _ a b) = [a, b]
nPorts (And _ a b) = [a, b]
nPorts (Or _ a b) = [a, b]


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
    P.reservedNames = ["node", "boolean", "and", "or", "port"],
    P.reservedOpNames = [],
    P.caseSensitive = True }
    

whitespace = P.whiteSpace lexer
identifier = P.identifier lexer
braces = P.braces lexer
brackets = P.brackets lexer
reserved = P.reserved lexer

port = do
    reserved "port"
    ns <- many $ identifier
    ds <- brackets $ many identifier
    return (ports ns ds)
    where
        port ds n = Port n ds
        ports ns ds = map (port ds) ns

node = do
    reserved "node"
    n <- identifier
    ps <- braces $ many port
    return (Node n (concat ps))

genBoolean name = do
    reserved name
    n <- identifier
    ps <- braces $ many port
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
    ns <- many (node <|> boolean <|> orN <|> andN)
    eof
    return ns

parseGraph s = case runParser graph () "" s of
      Left err  -> fail $ show err
      Right e   -> return e

