{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Dragonet.Unicorn(
    unicorn,
    unicorn_f,
    unicornImpl,
    unicornImpl_f,
    unicornSimpleConfig,
    unicornNode,
    unicornConfNode,
    unicornAndNode,
    unicornNAndNode,
    unicornOrNode,
    unicornNOrNode,
    unicornGraph,
) where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Maybe
import Data.Either
import qualified Data.List as L

import qualified Dragonet.ProtocolGraph as PG
import qualified Data.Graph.Inductive as DGI




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
quoteMyDec :: String -> TH.Q [TH.Dec]
quoteMyDec s = do
    gr <- parseGraph s
    return $ declare gr False

quoteMyDecImpl :: String -> TH.Q [TH.Dec]
quoteMyDecImpl s = do
    gr <- parseGraph s
    return (declare gr True)

declare :: Graph -> Bool -> [TH.Dec]
declare (Graph name cl) impl = declareClusters name cl impl

nodeClusterMap :: Cluster -> [(String, Node)]
nodeClusterMap (Cluster cn cs ns) =
    (map (\n -> (cn, n)) ns) ++ (concatMap nodeClusterMap cs)

clusterMap :: Cluster -> [String] -> [(Node,[String])]
clusterMap (Cluster cn cs ns) l =
    map (\n -> (n,l')) ns ++ concatMap (\c -> clusterMap c l') cs
    where l' = if null cn then l else (l ++ [cn])

fdec :: TH.Name -> TH.Exp -> TH.Dec
fdec n e = TH.FunD n [TH.Clause [] (TH.NormalB e) []]

callE :: TH.Exp -> [TH.Exp] -> TH.Exp
callE = foldl TH.AppE 

-- Helpers to make types more readable:
-- Type for list of t
tListOf :: TH.Type -> TH.Type
tListOf t = TH.AppT TH.ListT t
-- Type for a tuple with entries ts
tTupleOf :: [TH.Type] -> TH.Type
tTupleOf ts = foldl TH.AppT (TH.TupleT $ length ts) ts

declareClusters :: String -> Cluster -> Bool -> [TH.Dec]
declareClusters gn cl impl =
    signatures ++ definitions
    where
        -- Type signatures for the generated definitions
        --signatures = [clSig,nlSig,elSig,gSig]
        signatures = if impl then [] else [clSig,nlSig,elSig,gSig]
        gSig = TH.SigD graphName gType
        gType = TH.ConT (TH.mkName "PGraph") `TH.AppT` TH.TupleT 0
        nlSig = TH.SigD nodesName nlType
        nlType = tListOf $ tTupleOf [TH.ConT $ TH.mkName "Int",
                    (TH.ConT $TH.mkName "Node") `TH.AppT` (TH.TupleT 0)]
        elSig = TH.SigD edgesName elType
        elType = tListOf $ tTupleOf [TH.ConT $ TH.mkName "Int",
                                    TH.ConT $ TH.mkName "Int",
                                    TH.ConT $ TH.mkName "String"]
        clSig = TH.SigD clustersName clType
        clType = tListOf $ tTupleOf [TH.ConT $ TH.mkName "Int",
                    tListOf $ TH.ConT $ TH.mkName "String"]

        -- Actual definitions
        definitions = [cDef,nDef,eDef,gDef]


        nodeIdMap = zip [1..] $ nodeClusterMap cl
        lookupNode n = fst $ head' $ filter (\(_,(_,n')) -> nName n' == n) nodeIdMap
            where
                head' [] = error ("Node " ++ n ++ " not found")
                head' l = head l

        -- "g"Nodes, list of nodes
        nodesName = TH.mkName (gn ++ "Nodes")
        nDef = fdec nodesName nExps
        nDecl (i,(_,n)) = TH.TupE [TH.LitE $ TH.IntegerL i, nodeExp gn n impl]
        nExps = TH.ListE $ map nDecl nodeIdMap

        -- "g"Edges, list of edges
        edgesName = TH.mkName (gn ++ "Edges")
        eDef = fdec edgesName eExps
        eDecl (s,n,e) = TH.TupE $ map TH.LitE [TH.IntegerL s, TH.IntegerL e,
                                               TH.StringL n]
        pDecl i (Port n ds) = map (\d -> eDecl (i,n,lookupNode d)) ds
        eDecls (i,(_,n)) = concatMap (pDecl i) $ nPorts n
        eExps = TH.ListE $ concatMap eDecls nodeIdMap

        -- "g", the full graph
        graphName = TH.mkName gn
        gDef = fdec graphName $ callE (TH.VarE $ TH.mkName "unicornGraph")
                [TH.VarE nodesName, TH.VarE edgesName]

        -- "g"Clusters, List of clusters
        clustersName = TH.mkName (gn ++ "Clusters")
        cDef = fdec clustersName cExps
        cExps = TH.ListE $ map clDef $ clusterMap cl []
        clDef (n,cs) = TH.TupE [TH.LitE $ TH.IntegerL $ lookupNode $ nName n,
                               TH.ListE $ map (TH.LitE . TH.StringL) cs]



unicornNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> Maybe i -> PG.Node i
unicornNode = PG.baseFNode

unicornConfNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> PG.ConfFunction
                    -> Maybe i -> PG.Node i
unicornConfNode = PG.baseCNode

unicornAndNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> Maybe i
                    -> PG.Node i
unicornAndNode l a p = PG.baseONode l a p PG.OpAnd

unicornNAndNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> Maybe i
                    -> PG.Node i
unicornNAndNode l a p = PG.baseONode l a p PG.OpNAnd

unicornOrNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> Maybe i -> PG.Node i
unicornOrNode l a p = PG.baseONode l a p PG.OpOr

unicornNOrNode :: PG.Label -> [PG.Attribute] -> [PG.Port] -> Maybe i -> PG.Node i
unicornNOrNode l a p = PG.baseONode l a p PG.OpNOr

unicornGraph :: [(Int, PG.Node i)] -> [(Int, Int, PG.Port)] -> PG.PGraph i
unicornGraph nodes edges = DGI.mkGraph nodes edges


nodeExp gn n impl =
    callE (TH.VarE $ TH.mkName ntFun) ([labelE, attrE, portsE] ++ ntE ++ [i])
    where
        (ntFun,ntE,isfnode) =
            case n of
                (Node _ _ _) -> ("unicornNode", [],True)
                (Config _ _ _ c) -> ("unicornConfNode", [confFE c],False)
                (Boolean _ _ _ _) -> ("unicornNode", [],True)
                (And _ _ _ _) -> ("unicornAndNode", [],False)
                (NAnd _ _ _ _) -> ("unicornNAndNode", [],False)
                (Or _ _ _ _) -> ("unicornOrNode", [],False)
                (NOr _ _ _ _) -> ("unicornNOrNode", [],False)

        labelE = TH.LitE $ TH.StringL $ nName n
        attrE = TH.ListE $ map (TH.LitE . TH.StringL) $ nAttrs n
        portsE = TH.ListE $ map (TH.LitE . TH.StringL . pName) $ nPorts n
        confFE f = TH.VarE $ TH.mkName $ fromMaybe "unicornSimpleConfig" f

        i = if impl && isfnode then
                TH.AppE (TH.ConE $ TH.mkName "Just")
                    (TH.VarE $ TH.mkName $ gn ++ nName n ++ "Impl")
            else
                TH.ConE $ TH.mkName "Nothing"
        

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


-----------------------------------------------------------------------------
-- Helper functions


unicornSimpleConfig :: PG.ConfFunction
unicornSimpleConfig _ inE outE cfg =
    return $ concatMap edge inE
    where
        -- Only the out-endpoints that match the configuration
        outN = map fst $ filter ((== cfg) . snd) outE
        -- Remove labels from node in edge
        unlab ((a,_),(b,_),c) = (a,b,c)
        -- Create edge from (n,p) to all out-endpoints
        edge (n,p) = map unlab $ map (\x -> (n,x,p)) $ outN


