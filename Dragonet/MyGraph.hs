#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}


{-
 - Code to deal with graph related functionalities like converting into
 - a graph, generating visualizations using DoT.
 -}


--module Main (
module MyGraph (
    Gnode
    , showDependencyGraph
    , showFlowGraph
) where

import qualified Data.List as DL
import qualified Data.Char as DC

-- Gnode is the Datatype which captures single vertex of the Graph
-- and all its dependenceis
-- The DataType "a" is expected to be in instance of
--  "Show", "Eq", "Ord"
--  NOTE: Code depending on "Ord" is not used anymore (rmdups) and can be
--      removed.
type Gnode a = (a, [a])

type Edge a = (a, a) -- connects two vertices.

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
 - Replaces blank spaces with Underscores in given string
 -}
replaceSpaces :: String -> String
replaceSpaces str = map (\x-> (if DC.isAlphaNum x then x else '_' )) str

{-
 - Reversing edges to convert dependency graph into flow graph
 -}
reverseEdges :: [Edge a] -> [Edge a]
reverseEdges edgelist = DL.map (\(a, b) -> (b, a)) edgelist

{-
 - prints the edge with additional description (if needed)
 -}
showEdge :: (Show a) => Edge a -> String
showEdge (from, to) =  (replaceSpaces $ show from) ++ " -> " ++
            (replaceSpaces $ show to ) ++ " [label = \"" ++ "\"];\n"


{-
 - Find all AND nodes in given graph
 -}
findANDnodes :: [Gnode a] -> [a]
findANDnodes gnodeList = DL.map fst $ DL.filter (\x -> length (snd x) > 1 ) gnodeList

{-
 - Find all OR nodes in given graph.
 - TODO: These two functions are too similar.  There should be a way
 - to write them in one function.
 -}
findORnodes :: [Gnode a] -> [a]
findORnodes gnodeList = DL.map fst $ DL.filter (\x -> length (snd x) <= 1 ) gnodeList


{-
 - prints the vertex with information like AND or OR type (if needed)
 -}
showORnode :: (Show a) => a -> String
showORnode v = nodeName ++ " [label = " ++ nodeName  ++ "];\n"
    where
        nodeName = replaceSpaces $ show v

showANDnode :: (Show a) => a -> String
showANDnode v = nodeName ++ " [label = " ++ nodeName ++
        ", color=gray,style=filled,shape=trapezium];\n"
    where
        nodeName = replaceSpaces $ show v

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

{-
 - Converts given dependency graph encoded in Gnode list into DoT compatible
 -  graph notation.
 -  One can run command ``dot`` on this generated output to produce a graph.
 -}
showDependencyGraph ::(Show a) => [Gnode a] -> String
showDependencyGraph gnodeList = showGraphViz verticesListOR verticesListAND
                                    edgesList
    where
        verticesListOR = findORnodes gnodeList
        verticesListAND = findANDnodes gnodeList
        edgesList = reverseEdges $ getEdges gnodeList

{-
 - Just like above showDependencyGraph function, converts Gnode list
 - into DoT compatible graph, but it reverts the direction of all the
 - edges to generate a flow graph instead of dependency graph.
 -
 - TODO: These two functions are too similar.  There should be a way
 - to write them as one function.
 -}
showFlowGraph ::(Show a) => [Gnode a] -> String
showFlowGraph gnodeList = showGraphViz verticesListOR verticesListAND
                                    edgesList
    where
        verticesListOR = findORnodes gnodeList
        verticesListAND = findANDnodes gnodeList
        edgesList = reverseEdges $ getEdges gnodeList


