#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module DotGenerator(
  toDot
) where

import qualified Operations as OP
import qualified NetBasics as NB
import qualified Data.List as L
import Data.Maybe


-- Get string label for GNode
gLabelStr gn = NB.graphLabelStr (OP.gLabel gn)

dotNode name label shape =
    "    " ++ name ++ "[label=\"" ++ label ++ "\",shape=" ++ shape ++ "];\n"
dotEdge from to =
    "    " ++ from ++ " -> " ++ to ++ ";\n"

-- Get dot definition for specified node
nodeDefinition :: (OP.Node, String) -> String
nodeDefinition ((OP.Des (OP.Decision gn)), nn) =
    dotNode nn (gLabelStr gn) "box"
nodeDefinition ((OP.Opr (OP.Operator gn)), nn) =
    dotNode nn (gLabelStr gn) "oval"

-- Get dot definition for edges starting at the specified node
edgeDefinition :: [(OP.Node, String)] -> (OP.Node, String) -> String
edgeDefinition dict (n, nn) =
    concat (map edgeTo endpoints)
    where
        edgeTo n = dotEdge nn (fromJust (lookup n dict))
        endpoints =
            case OP.getNodeEdges n of
                (OP.BinaryNode (ts,fs)) -> ts ++ fs
                (OP.NaryNode es) -> concat es

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
        names = buildNames (L.nub (OP.nTreeNodes n))
        definitions = concat (map nodeDefinition names)
        edges = concat (map (edgeDefinition names) names)

