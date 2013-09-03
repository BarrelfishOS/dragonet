module Unicorn(
    unicorn,
    unicorn_f,
    unicornImpl,
    unicornImpl_f,
    unicornSimpleConfig
) where

import System.IO

import qualified Debug.Trace as T

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Maybe
import Data.Either
import qualified Data.List as L

import qualified Operations as OP




unicorn  :: QuasiQuoter
unicorn  =  QuasiQuoter { quoteExp = undefined,
                           quotePat = undefined,
                           quoteType = undefined,
                           quoteDec = quoteMyDec
}

unicornImpl  :: QuasiQuoter
unicornImpl  =  QuasiQuoter { quoteExp = undefined,
                           quotePat = undefined,
                           quoteType = undefined,
                           quoteDec = quoteMyDecImpl
}


unicorn_f :: QuasiQuoter
unicorn_f = quoteFile unicorn

unicornImpl_f :: QuasiQuoter
unicornImpl_f = quoteFile unicornImpl



-- That's where the magic happens: we get the string representing the DSL input
-- and generate a Haskell AST
quoteMyDec s = do
    graph <- parseGraph s
    return $ declare graph

quoteMyDecImpl s = do
    graph <- parseGraph s
    return (declare graph ++ implDec graph)


declare (Graph name cluster) =
    nodesDecl ++ clustersDecl
    where
        nodesDecl = map (declareNode name) $ cNodesDeep cluster
        clustersDecl = declareClusters name cluster

flattenCluster (Cluster cn cs ns) =
    (map (\(Cluster x _ _) -> (cn, x)) cs) ++ (concatMap flattenCluster cs)

nodeClusterMap (Cluster cn cs ns) =
    (map (\n -> (cn, n)) ns) ++ (concatMap nodeClusterMap cs)

declareClusters gn cluster =
    clusters:c2nodes:[]
    where
        clusters = TH.FunD (TH.mkName (gn ++ "Clusters")) [(TH.Clause [] (TH.NormalB clExp) [])]
        tDecl (a,b) = TH.TupE [(TH.LitE (TH.StringL a)), (TH.LitE (TH.StringL b))]
        clExp = TH.ListE $ map tDecl $ flattenCluster cluster

        c2nodes = TH.FunD (TH.mkName (gn ++ "Nodes")) [(TH.Clause [] (TH.NormalB c2nExp) [])]
        cnDecl (a,n) = TH.TupE [(TH.LitE (TH.StringL a)), (TH.VarE (TH.mkName (gn ++ (nName n))))]
        c2nExp = TH.ListE $ map cnDecl $ nodeClusterMap cluster

declareNode graph n =
    TH.FunD (TH.mkName $ fname n) [(TH.Clause [] (TH.NormalB (node n)) [])]
    where
        fname m = graph ++ (nName m)
        portEdge e = TH.VarE (TH.mkName $ graph ++ e)
        port (Port _ ns) = TH.ListE (map portEdge ns)
        binEdges t f =
            TH.AppE (TH.ConE (TH.mkName "OP.BinaryNode")) $
                TH.TupE [(port t), (port f)]
        lblPort p = TH.TupE [(TH.LitE $ TH.StringL name), (port p)]
            where (Port name _) = p
        naryEdges ps = 
            TH.AppE (TH.ConE (TH.mkName "OP.NaryNode")) $
                TH.ListE $ map lblPort ps

        edges (Node l ps as) = naryEdges ps
        edges (Config l ps as _) = naryEdges ps
        edges (Boolean _ t f _) = binEdges t f
        edges (And _ t f _) = binEdges t f
        edges (Or _ t f _) = binEdges t f

        attrs n = TH.ListE $ map (TH.LitE . TH.StringL) $ nAttrs n

        decNode n = TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getDecNode")) (TH.LitE (TH.StringL (nName n)))) (TH.LitE (TH.StringL ""))) (edges n)) (attrs n)
        confNode n f = TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getConfNode")) (TH.LitE (TH.StringL (nName n)))) (TH.LitE (TH.StringL ""))) (edges n)) (attrs n)) (TH.VarE (TH.mkName f))
        opNode n op = TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.VarE (TH.mkName "OP.getOperatorNode")) (TH.LitE (TH.StringL op))) (TH.LitE (TH.StringL ""))) (edges n)) (attrs n)

        node n =
            case n of
                (Node _ _ _) -> decNode n
                (Config _ _ _ f) -> confNode n $ fromMaybe "unicornSimpleConfig" f
                (Boolean _ _ _ _) -> decNode n
                (And l _ _ _) -> opNode n ("AND:" ++ l)
                (Or l _ _ _) -> opNode n ("OR:" ++ l)


implDec (Graph name cluster) =
    map (implMap name) $ cNodesDeep cluster

implMap graph n =
    TH.FunD (implNName $ fname n) [(TH.Clause [] (TH.NormalB (code)) [])]
    where
        fname m = graph ++ (nName m)
        edge e = TH.VarE $ implNName $ graph ++ e
        port (Port pn es)  = TH.TupE [TH.LitE $ TH.StringL pn, TH.ListE $ map edge es]
        plistExp = TH.ListE $ map port $ nPorts n
        nodeExp = TH.VarE $ TH.mkName $ fname n
        implExp =
            if nIsOp n then
                TH.ConE $ TH.mkName "Nothing"
            else
                TH.AppE (TH.ConE $ TH.mkName "Just") $ TH.VarE implName
        implNodeExp = TH.ConE $ TH.mkName "ImplNode"
        code = foldl TH.AppE implNodeExp [nodeExp, implExp, plistExp]
        implName = TH.mkName (fname n ++ "Impl")
        implNName m = TH.mkName  (m ++ "ImplNode")
    
    
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
    Or String Port Port [String]
    deriving Show

cNodesDeep :: Cluster -> [Node]
cNodesDeep (Cluster _ cs ns) = ns ++ concatMap cNodesDeep cs

gNodes (Graph _ cs) = cNodesDeep cs

nIsOp (And n _ _ _) = True
nIsOp (Or n _ _ _) = True
nIsOp _ = False

nName (Node n _ _) = n
nName (Config n _ _ _) = n
nName (Boolean n _ _ _) = n
nName (And n _ _ _) = n
nName (Or n _ _ _) = n

nAttrs (Node _ _ as) = as
nAttrs (Config _ _ as _) = as
nAttrs (Boolean _ _ _ as) = as
nAttrs (And _ _ _ as) = as
nAttrs (Or _ _ _ as) = as

nPorts :: Node -> [Port]
nPorts (Node _ ps _) = ps
nPorts (Config _ ps _ _) = ps
nPorts (Boolean _ a b _) = [a, b]
nPorts (And _ a b _) = [a, b]
nPorts (Or _ a b _) = [a, b]


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
                       "or", "gconfig", "port", "attr"],
    P.reservedOpNames = [],
    P.caseSensitive = True }
    

whitespace = P.whiteSpace lexer
identifier = P.identifier lexer
braces = P.braces lexer
brackets = P.brackets lexer
reserved = P.reserved lexer
stringLiteral = P.stringLiteral lexer


globalIdentifier p = do
    char '.'
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
        port ds n = Port n ds
        ports ns ds = map (port ds) ns

attributes = do
    reserved "attr"
    stringLiteral
    
constraint = do
    reserved "constraint"
    p <- identifier
    exp <- stringLiteral
    return (p,exp)

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

andN p = do
    (n, t, f, a) <- genBoolean p "and" False
    return (Right (And n t f a))
  
cluster p = do
    reserved "cluster"
    n <- identifier
    ns <- braces $ clusteredNodes (n:p)
    return (Left (Cluster ((concat $ reverse p) ++ n) (lefts ns) (rights ns)))

clusteredNodes p = do
    ns <- many (node p <|> config p <|> boolean p <|> orN p <|> andN p <|> cluster p)
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


-----------------------------------------------------------------------------
-- Helper functions


unicornSimpleConfig :: OP.Node -> [(OP.Node,String)] -> [(String,OP.Node)] -> String -> [(OP.Node,String,OP.Node)]
unicornSimpleConfig n inE outE cfg =
    concatMap edge inE
    where
        edge :: (OP.Node,String) -> [(OP.Node,String,OP.Node)]
        edge (n,p) = map (\x -> (n,p,x)) $ map snd $ filter ((== cfg) . fst) outE

