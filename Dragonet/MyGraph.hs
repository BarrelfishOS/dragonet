#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

--module Main (
module MyGraph (
    Gnode
    , showDependencyGraph
    , showFlowGraph
) where

import qualified Data.List as DL

type Gnode a = (a, [a])
type Edge a = (a, a)

{-
 - For debugging purposes:
 - Shows the list datatype in more readable way by putting
 - every element on new line.
 -}
myShowList :: (Show a) => [a] -> String
myShowList [] = "\n"
myShowList (x:xs) = show x ++ "\n" ++ myShowList xs

{-
 - Remove the duplicates from the list
 -}
rmdups :: (Ord a) => [a] -> [a]
rmdups = DL.map DL.head . DL.group . DL.sort

{-
 - get list of valid vertices from given Gnode list.
 - This also includes the vertices which have only incoming edges
 - and no outgoing edges
-}
getVertices :: (Ord a) => [Gnode a] -> [a]
getVertices nlist = rmdups $ source_vertex ++ dest_vertex
    where
        source_vertex = DL.map fst nlist
        dest_vertex = DL.concat $ DL.map snd nlist


{-
 - Get list of all the edges from given list of Gnodes
 -}
makeEdgeList :: a -> [a] -> [Edge a]
makeEdgeList _ [] = []
makeEdgeList src (x:xs) = [(src, x)] ++ makeEdgeList src xs

getEdges :: [Gnode a] -> [Edge a]
getEdges nlist = DL.concat $ DL.map
                    ( \ e -> makeEdgeList (fst e) (snd e)) nlist

{-
 - Reversing edges to convert dependency graph into flow graph
 -}
reverseEdges :: [Edge a] -> [Edge a]
reverseEdges edgelist = DL.map (\(a, b) -> (b, a)) edgelist

{-
 - prints the edge with additional description (if needed)
 -}
showEdge :: (Show a) => Edge a -> String
showEdge (from, to) = show from ++ " -> " ++ show to ++
                   " [label = \"" ++ "\"];\n"

{-
 - Find all AND nodes in given graph
 -}
findANDnodes :: [Gnode a] -> [a]
findANDnodes gnodeList = DL.map fst $ DL.filter (\x -> length (snd x) > 1 ) gnodeList

findORnodes :: [Gnode a] -> [a]
findORnodes gnodeList = DL.map fst $ DL.filter (\x -> length (snd x) <= 1 ) gnodeList


{-
 - prints the vertex with information like AND or OR type (if needed)
 -}
showORnode :: (Show a) => a -> String
showORnode v = show v ++ " [label = " ++ (show  v) ++ "];\n"

showANDnode :: (Show a) => a -> String
showANDnode v = show v ++ " [label = " ++ (show  v) ++
        ", color=gray,style=filled,shape=trapezium];\n"


{-
 - Prints the graph in dot format
 - Arguments are <list of vertices> <list of edges>
 -}
showGraphViz :: (Show a) => [a] -> [a] -> [Edge a] -> String
showGraphViz verOR verAND edges =
    "digraph name {\n" ++
    "rankdir=LR;\n" ++
    (DL.concatMap showORnode verOR) ++
    (DL.concatMap showANDnode verAND) ++
    (DL.concatMap showEdge edges) ++
    "}\n"

showFlowGraph ::(Show a) => [Gnode a] -> String
showFlowGraph gnodeList = showGraphViz verticesListOR verticesListAND
                                    edgesList
    where
        verticesListOR = findORnodes gnodeList
        verticesListAND = findANDnodes gnodeList
        edgesList = reverseEdges $ getEdges gnodeList

showDependencyGraph ::(Show a) => [Gnode a] -> String
showDependencyGraph gnodeList = showGraphViz verticesListOR verticesListAND
                                    edgesList
    where
        verticesListOR = findORnodes gnodeList
        verticesListAND = findANDnodes gnodeList
        edgesList = reverseEdges $ getEdges gnodeList


