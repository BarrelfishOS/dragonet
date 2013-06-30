#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module DotGenerator(
  toDot
) where

import qualified Operations as OP
import qualified Data.List as L
import Data.Maybe


-- Get string label for GNode
gLabelStr gn = OP.gLabel gn

dotNode name label style nPorts =
    "    " ++ name ++ "[label=\"" ++ l ++ "\",shape=record" ++ s ++ "];\n"
    where
        s = if style /= "" then ",style=" ++ style else ""
        port i = "|<p" ++ (show i) ++ "> " ++ (show (i - 1)) ++ " "
        ports =
            case (concat (map port [1..nPorts])) of
                h:t -> t
                _ -> ""
        l = "{" ++ label ++ "|{" ++ ports ++ "}}"

dotEdge (from,port) to =
    "    " ++ f ++ " -> " ++ to ++ "[headport=\"w\"];\n"
    where
        f = from ++ ":p" ++ (show port) ++ ":e"
dotDoubleEdge (from,port) to =
    "    " ++ from ++ " -> " ++ to ++ "[color=\"black:black\"," ++
        "headport=\"w\",tailport=\"e\"];\n"

-- Number of ports required for node
nPorts (OP.BinaryNode _) = 2
nPorts (OP.NaryNode l) = length l

-- Get dot definition for specified node
nodeDefinition :: (OP.Node, String) -> String
nodeDefinition (n, nn) =
    case n of
        (OP.Des (OP.Decision gn)) ->
            dotNode nn (gLabelStr gn) "" (nPorts (OP.getNodeEdges n))
        (OP.Opr (OP.Operator gn)) ->
            dotNode nn (gLabelStr gn) "rounded" (nPorts (OP.getNodeEdges n))
        (OP.Conf (OP.Configuration gn)) ->
            dotNode nn (gLabelStr gn) "rounded" (nPorts (OP.getNodeEdges n))




-- Get dot definition for edges starting at the specified node
edgeDefinition :: [(OP.Node, String)] -> (OP.Node, String) -> String
edgeDefinition dict (n, nn) =
     case OP.getNodeEdges n of
        (OP.NaryNode es) ->
            edgesToEps dotEdge (prefixLL (concat es))
        (OP.BinaryNode (ts,fs)) ->
            (edgesToEps dotDoubleEdge (doubleEps ts fs)) ++
            (edgesToEps dotEdge (singleEps ts fs))
    where
        prefixL (l,p) = zip l (replicate (length l) p)
        prefixLL l = zip l [1..(length l)]
        -- endpoints in both lists
        doubleEps ts fs = prefixL ((L.intersect ts fs),0)
        -- endpoints in only one list
        singleEps ts fs = (prefixL (nfs, 1)) ++ (prefixL (nts, 2))
            where
                dbl = L.intersect ts fs
                nts = (L.nub ts) L.\\ dbl
                nfs = (L.nub fs) L.\\ dbl


        edgesToEps f eps = concat (map (edgeTo f) eps)
        edgeTo f (n,p) = f (nn,p) (fromJust (lookup n dict))

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

