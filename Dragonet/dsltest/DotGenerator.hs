#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module DotGenerator(
  toDot,
  toDotClustered,
  toDotFromDLP
) where

import qualified Operations as OP
import qualified Data.List as L
import Data.Maybe


--- Filter some labels for more descriptive names
filterLabel :: String -> String
filterLabel x
  | (x == "IsDnsFlow") = "UDP/*:53"
  | (x == "IsDhcpdFlow") = "UDP/*:67"
  | otherwise  = x


-- Get string label for GNode
gLabelStr gn = filterLabel $ OP.gLabel gn

-- Declare a dot node
dotNode :: String -> String -> String -> [String] -> String
dotNode name label style ports =
    "    " ++ name ++ "[label=\"" ++ l ++ "\",shape=record" ++ s ++ "];\n"
    where
        s = if style /= "" then "," ++ style else ""
        port :: String -> String
        port pl = "|<" ++ pl ++ "> " ++ pl ++ " "
        portDecs = drop 1 $ concat $ map port ports
        l = "{" ++ label ++ "|{" ++ portDecs ++ "}}"

-- Declare a single dot edge
dotEdge :: (String,String) -> String -> String
dotEdge (from,port) to =
    "    " ++ f ++ ":e -> " ++ to ++ ":w;\n"
    where
        f = from ++ ":" ++ port

-- Declare a dot double edge
dotDoubleEdge :: (String,String) -> String -> String
dotDoubleEdge (from,_) to =
    "    " ++ from ++ ":e -> " ++ to ++ ":w[color=\"black:white:black\"];\n"

-- Get dot definition for specified node
nodeDefinition :: (OP.Node, String) -> String
nodeDefinition (n, nn) =
    case n of
        (OP.Des (OP.Decision gn)) ->
            dotNode nn (gLabelStr gn) ("style=\""++swdot++"\"") $ ports $ OP.getNodeEdges n
        (OP.Opr (OP.Operator gn)) ->
            dotNode nn (gLabelStr gn) "style=\"filled,rounded\",fillcolor=gray"
                $ ports $ OP.getNodeEdges n
        (OP.Conf (OP.Configuration gn)) ->
            dotNode nn (gLabelStr gn)
                "style=\"filled,diagonals\",fillcolor=turquoise" $
                ports $ OP.getNodeEdges n
    where
        -- Get port names from NodeEdges
        ports (OP.BinaryNode _) = ["T", "F"]
        ports (OP.NaryNode l) = map fst l
        swdot = if elem "software" (OP.nAttributes n) then "dotted" else ""

-- Convert list to list of tuples with the first element being the list
-- elements and the second element being the constant specified
sufixL :: a -> [b] -> [(b,a)]
sufixL p l = zip l $ replicate (length l) p

-- Get dot definition for edges starting at the specified node
edgeDefinition :: [(OP.Node, String)] -> (OP.Node, String) -> String
edgeDefinition dict (node, name) =
     case OP.getNodeEdges node of
        (OP.NaryNode es) ->
            edgesToEps dotEdge $ concat $ map (\(l,e) -> sufixL l e) es
        (OP.BinaryNode (ts,fs)) ->
            (edgesToEps dotDoubleEdge $ doubleEps ts fs) ++
            (edgesToEps dotEdge $ singleEps ts fs)
    where
        -- endpoints in both lists
        doubleEps ts fs = sufixL "" $ L.intersect ts fs
        -- endpoints in only one list
        singleEps ts fs = (sufixL "F" nfs) ++ (sufixL "T" nts)
            where
                dbl = L.intersect ts fs
                nts = (L.nub ts) L.\\ dbl
                nfs = (L.nub fs) L.\\ dbl
        edgesToEps f eps = concat $ map (edgeTo f) eps
        edgeTo f (n,p) = f (name,p) $ fromJust $ lookup n dict

-- Get associative list between node and their dot names, assumes each node
-- only occurs once in the list
buildNames :: [OP.Node] -> [(OP.Node,String)]
buildNames n =
    zip n names
    where
        name i = "node" ++ (show i)
        names = map name [1..(length n)]

-- Generate DOT string for specified graph (start node)
toDot :: OP.Node -> String
toDot n =
    "digraph G {\n" ++ "    rankdir=LR;\n" ++
        definitions ++ "\n" ++ edges ++ "}\n"
    where
        names = buildNames $ L.nub $ OP.nTreeNodes n
        definitions = concat $ map nodeDefinition names
        edges = concat $ map (edgeDefinition names) names

data Cluster = Cluster String [OP.Node] [Cluster]

clusterDefinition :: [(String,OP.Node)] -> [(OP.Node,String)] -> [(String,String)] -> String -> String
clusterDefinition nodes names clusters c =
    if null c then
        ndecls ++ cdecls
    else
        "subgraph cluster" ++ c ++ " {\n" ++ ndecls ++ cdecls ++
            "\n    label=\"" ++ c ++ "\";\n}\n"
    where
        findAll a as = map snd $ filter ((== a) . fst) as
        nDec n = nodeDefinition (n, (fromJust $ lookup n names))
        ndecls = concatMap nDec $ findAll c nodes
        cdecls = concatMap (clusterDefinition nodes names clusters) $ findAll c clusters

-- Generate DOT string for specified graph (start node)
toDotClustered :: [(String,String)] -> [(String,OP.Node)] -> String
toDotClustered clusters nodeMap =
    "digraph G {\n" ++ "    rankdir=LR;\n" ++
        definitions ++ "\n" ++ edges ++ "}\n"
    where
        nodes = map snd nodeMap
        names = buildNames $ nodes
        definitions = clusterDefinition nodeMap names clusters ""
        edges = concat $ map (edgeDefinition names) names



myLookup :: OP.Node -> [(OP.Node,a)] -> Maybe a
myLookup x l =
    case f of
        Nothing -> Nothing
        Just (_,r) ->  Just r
    where
        f = L.find (\(y,_) -> OP.nCompPrgLpgV2 x y) l




edgeDefinitionDL :: [(OP.Node,String)] -> (OP.Node,OP.Node) -> String
edgeDefinitionDL names (f,t) = dotEdge (fname,"") tname
    where
        fname = fromJust $ myLookup f names
        tname = fromJust $ myLookup t names


toDotFromDL :: [(OP.Node,OP.Node)] -> String
toDotFromDL ns =
    "digraph G {\n" ++ "    rankdir=LR;\n" ++
        definitions ++ "\n" ++ edges ++ "}\n"
    where
        names = buildNames $ L.nubBy OP.nCompPrgLpgV2 $ (map fst ns) ++ (map snd ns)
        definitions = concat $ map nodeDefinition names
        edges = concat $ map (edgeDefinitionDL names) ns





edgeDefinitionDLP :: [(OP.Node,String)] -> (OP.Node,String,OP.Node) -> String
edgeDefinitionDLP names (f,p,t) =
    if null p then
        dotDoubleEdge (fname,p) tname
    else
        dotEdge (fname,p) tname
    where
        fname = fromJust $ myLookup f names
        tname = fromJust $ myLookup t names


toDotFromDLP :: [(OP.Node,String,OP.Node)] -> String
toDotFromDLP ns =
    "digraph G {\n" ++ "    rankdir=LR;\n" ++
        definitions ++ "\n" ++ edges ++ "}\n"
    where
        fst' (a,_,_) = a
        third (_,_,a) = a
        names = buildNames $ L.nubBy OP.nCompPrgLpgV2 $ (map fst' ns) ++ (map third ns)
        definitions = concat $ map nodeDefinition names
        edges = concat $ map (edgeDefinitionDLP names) ns


