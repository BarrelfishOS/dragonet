module Dragonet.ProtocolGraph.Utils (
    dropSpawnEdges,
    entryNodes,
    orderedSpawns,
    getPGNodeByName,
    getFNodeByName,
    getFNodeByName',
    getFNodeByNameTag',
    getPGNAttr,
    spawnDeps, isSpawnTarget, edgeDeps, edgePort, edgeSucc,
) where

import Dragonet.ProtocolGraph

import Data.Function
import Data.Maybe
import Data.List (sortBy, find, isPrefixOf)
import qualified Data.Set as S
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import Data.Functor ((<$>))

-- Remove all spawn edges from the graph
dropSpawnEdges :: PGraph -> PGraph
dropSpawnEdges g = flip DGI.gmap g $ \(ins,n,l,outs) ->
    (filter isNSpawn ins, n, l, filter isNSpawn outs)
    where
        isNSpawn (ESpawn {},_) = False
        isNSpawn (_,_) = True

entryNodes :: PGraph -> S.Set DGI.Node
entryNodes g = S.fromList $ mapMaybe getN $ DGI.labEdges g
    where
        -- The if is a very ugly hack
        getN (_,d,ESpawn {}) = Just d
        getN _ = Nothing

-- | Ordered list of spawn-handles (order is used to generate implementation
--   IDs)
orderedSpawns :: PGraph -> DGI.Node -> [(NSpawnHandle,DGI.Node)]
orderedSpawns pg n = sortBy (compare `on` fst)
        [(sh,n) | (n,ESpawn { eIdentifier = sh }) <- DGI.lsuc pg n]


getPGNodeByName :: PGraph -> NLabel -> PGNode
getPGNodeByName graph name = case GH.findNodeByL fn graph of
    Just x -> x
    Nothing -> error $ "Unable to find node with name: " ++ name
    where fn n = (nLabel n == name)

getFNodeByName' :: PGraph -> NLabel -> Maybe PGNode
getFNodeByName' graph name = GH.findNodeByL fn graph
    where fn (FNode {nLabel = label}) = (label == name)
          fn _ = False

getFNodeByNameTag' :: PGraph -> NLabel -> NTag -> Maybe PGNode
getFNodeByNameTag' graph label' tag' = GH.findNodeByL fn graph
    where fn (FNode {nLabel = label, nTag = tag }) = (label == label') && (tag == tag')
          fn _ = False

getFNodeByName :: PGraph -> NLabel -> PGNode
getFNodeByName graph name = case GH.findNodeByL fn graph of
    Just x -> x
    Nothing -> error $ "Unable to find f-node with name: " ++ name
    where fn (FNode label _ _ _ _ _ _) = (label == name)
          fn _ = False

-- Get key=value attribute with key == n
getPGNAttr :: Node -> String -> Maybe String
getPGNAttr node n = getVal <$> (find matches $ nAttributes node)
    where
        matches (NAttrCustom s) = (n ++ "=") `isPrefixOf` s
        matches _ = False
        getVal (NAttrCustom s) = drop (length n + 1) s

-- is this a normal (not spawn) edge?
isNormalEdge :: PGEdge -> Bool
isNormalEdge e = case e of
    (_, _, ESpawn _ _) -> False
    (_, _, Edge _)     -> True

isSpawnEdge = not . isNormalEdge

spawnDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
spawnDeps gr dst = filter (isSpawnEdge . snd) $ GH.labLPre gr dst

-- equality based on label: same type and same label
eqLabel :: PGNode -> PGNode -> Bool
eqLabel (_, FNode {nLabel = x1}) (_, FNode {nLabel = x2}) = x1 == x2
eqLabel (_, ONode {nLabel = x1}) (_, ONode {nLabel = x2}) = x1 == x2
eqLabel _ _ = False

edgePort :: PGEdge -> NPort
edgePort pedge = case pedge of
    (_, _, ESpawn _ _) -> error "spawn edges do not have ports"
    (_, _, Edge p)     -> p

-- normal incoming dependencies (label of connected node, edge)
edgeDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
edgeDeps gr dst = filter (isNormalEdge . snd) $ GH.labLPre gr dst


-- normal (i.e., not spawn) sucessor nodes
edgeSucc :: PGraph -> PGNode -> [(PGNode, PGEdge)]
edgeSucc gr pre = filter (isNormalEdge . snd) $ GH.labLSucc gr pre

-- is the node a traget spawn edges
isSpawnTarget :: PGraph -> PGNode -> Bool
isSpawnTarget g n = not $ null $ spawnDeps g n
