#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}


{-
 - Code to deal with graph related functionalities like converting into
 - a graph, generating visualizations using DoT.
 -}


--module Main (
module MyGraph (
    showDependencyGraph
    , showFlowGraph
    , showEmbeddedGraph
) where

import qualified Computations as MC
import qualified Data.List as DL
import qualified Data.Char as DC

-- MC.Gnode is the Datatype which captures single vertex of the Graph
-- and all its dependenceis
-- The DataType "a" is expected to be in instance of
--  "Show", "Eq"

type ShowEdgeFn a = MC.Edge a -> String
type ShowVertexFn a = a -> String


{-
 - Get list of all the edges from given list of MC.Gnodes
 -}
makeEdgeList :: MC.GraphNode a => a -> [a] -> [MC.Edge a]
makeEdgeList _ [] = []
makeEdgeList src (x:xs) = [(src, x)] ++ makeEdgeList src xs

getEdges ::  MC.GraphNode a => [MC.Gnode a] -> [MC.Edge a]
getEdges nlist = DL.concat $ DL.map
                    ( \ e -> makeEdgeList (fst e) (snd e)) nlist

{-
 - Replaces blank spaces with Underscores in given string
 -}
replaceSpaces :: String -> String
replaceSpaces str = map (\x-> (if DC.isAlphaNum x then x else '_' )) str

{-
 - Reversing edges to convert dependency graph into flow graph
 -}
reverseEdges :: MC.GraphNode a => [MC.Edge a] -> [MC.Edge a]
reverseEdges edgelist = DL.map (\(a, b) -> (b, a)) edgelist

{-
 - prints the MC.Edge with additional description (if needed)
 -}
showEdge :: MC.GraphNode a => MC.Edge a -> String
showEdge (from, to) =  (replaceSpaces fromV) ++ " -> " ++
            (replaceSpaces toV ) ++ " [label = \"" ++ "\"];\n"
        where
        fromV = MC.toVertex from
        toV = MC.toVertex to

{-
 - Find all AND nodes in given graph
 -}
findANDnodes :: (Eq a) => [MC.Gnode a] -> [a]
findANDnodes gnodeList = DL.nub $ DL.map fst $ DL.filter (\x -> length (snd x) > 1 ) gnodeList

{-
 - Find all OR nodes in given graph.
 - TODO: These two functions are too similar.  There should be a way
 - to write them in one function.
 -}
findORnodes :: (Eq a) => [MC.Gnode a] -> [a]
findORnodes gnodeList = DL.nub $ DL.map fst $ DL.filter (\x -> length (snd x) <= 1 ) gnodeList


{-
 - prints the vertex with information like AND or OR type (if needed)
 -}
showORnode :: MC.GraphNode a => a -> String
showORnode v
    | "IsConfSet" `DL.isPrefixOf` nodeName = nodeName ++ " [label = "
        ++ nodeName ++ ", color=gray,style=filled,shape=tab];\n"
    | "IsPartial" `DL.isPrefixOf` nodeName = nodeName ++ " [label = "
        ++ nodeName ++ ", color=turquoise,style=filled,shape=folder];\n"
    | otherwise = nodeName ++ " [label = " ++ nodeName  ++ "];\n"
    where
        nodeName = replaceSpaces $ MC.toVertex v


showANDnode :: MC.GraphNode a => a -> String
showANDnode v = nodeName ++ " [label = " ++ nodeName ++
        ", color=gray,style=filled,shape=trapezium];\n"
    where
        nodeName = replaceSpaces $ MC.toVertex v

showEmbeddednode :: MC.GraphNode a => a -> String
showEmbeddednode v = nodeName ++ " [label = " ++ nodeName ++
        ", color=green,style=filled,shape=rectangle];\n"
    where
        nodeName = replaceSpaces $ MC.toVertex v


showNode :: MC.GraphNode a => (Eq a) => [a] -> [a] -> [a] -> a -> String
showNode orList andList embList v
        | DL.elem v andList = showANDnode v
        | DL.elem v orList = showORnode v
        | DL.elem v embList = error "element in wrong list"
        | otherwise = error "element is not in any list"



showNodeGeneric :: MC.GraphNode a => (Eq a) => [([a], ShowVertexFn a)] -> a -> String
showNodeGeneric fancyList v
    | DL.length matchedElement /= 0 = ((snd $ DL.head matchedElement) v)
    | otherwise = error "element not found in fancy list"
    where
        matchedElement = DL.filter (\ aa -> DL.elem v (fst aa)) fancyList


{-
 - Prints the graph in dot format
 - Arguments are <list of vertices> <list of edges>
 -}
showGraphViz :: (MC.GraphNode a) => (Eq a) => [a] -> ShowVertexFn a ->
                    [MC.Edge a] -> ShowEdgeFn a -> String
showGraphViz vertexList svf edges sef =
    "digraph name {\n" ++
    "rankdir=LR;\n" ++
    (DL.concatMap svf vertexList) ++
    (DL.concatMap sef edges) ++
    "}\n"


{-
 - Converts given graph encoded in MC.Gnode list into DoT compatible
 -  graph notation.
 -  One can run command ``dot`` on this generated output to produce a graph.
 -}
showGenGraph ::(MC.GraphNode a) => (Eq a) => [MC.Gnode a] -> Bool -> String
showGenGraph gnodeList isDependency = showGraphViz verticesList shownodefn
                                edgesList showEdge
    where
        verticesListOR = findORnodes gnodeList
        verticesListAND = findANDnodes gnodeList
        verticesList =  verticesListOR ++ verticesListAND
        shownodefn = (showNodeGeneric [
                   (verticesListOR, showORnode)
                   , (verticesListAND, showANDnode)
                ])
        edgesList
            | isDependency = getEdges gnodeList
            | otherwise = reverseEdges $ getEdges gnodeList


showEmbeddedGraph ::(MC.GraphNode a) => (Eq a) => [MC.Gnode a] -> [MC.Gnode a] -> String
showEmbeddedGraph gbig gsmall = showGraphViz verticesList shownodefn
                                edgesList showEdge
    where
        vbigOR = findORnodes gbig
        vsmallOR = findORnodes gsmall
        vembedOR = DL.intersect vbigOR vsmallOR
        vbigAND = findANDnodes gbig
        vsmallAND = findANDnodes gsmall
        vembedAND = DL.intersect vbigAND vsmallAND
        verticesList = DL.nub (vbigOR ++ vsmallOR ++ vbigAND ++ vsmallAND)

        shownodefn = (showNodeGeneric [
                        (vembedOR, showEmbeddednode)
                        , (vbigOR, showORnode)
                        , (vsmallOR, showORnode)
                        , (vbigAND, showANDnode)
                        , (vsmallAND, showANDnode)])

        ebig = reverseEdges $ getEdges gbig
        --esmall = reverseEdges $ getEdges gsmall
        esmall = []
        edgesList = DL.nub (ebig ++ esmall)




{-
 - Converts given dependency graph encoded in MC.Gnode list into DoT compatible
 -  graph notation.
 -  One can run command ``dot`` on this generated output to produce a graph.
 -}
showDependencyGraph ::(MC.GraphNode a) => (Eq a) => [MC.Gnode a] -> String
showDependencyGraph gnodeList = showGenGraph gnodeList True


{-
 - Just like above showDependencyGraph function, converts MC.Gnode list
 - into DoT compatible graph, but it reverts the direction of all the
 - edges to generate a flow graph instead of dependency graph.
 -}
showFlowGraph ::(MC.GraphNode a) => (Eq a) => [MC.Gnode a] -> String
showFlowGraph gnodeList = showGenGraph gnodeList False

