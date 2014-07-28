module Dragonet.ProtocolGraph.Utils (
    dropSpawnEdges,
    entryNodes,
    orderedSpawns,
    getPGNodeByName,
    getFNodeByName,
    getFNodeByName',
) where

import Dragonet.ProtocolGraph

import Data.Function
import Data.Maybe
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH

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

getFNodeByName :: PGraph -> NLabel -> PGNode
getFNodeByName graph name = case GH.findNodeByL fn graph of
    Just x -> x
    Nothing -> error $ "Unable to find f-node with name: " ++ name
    where fn (FNode label _ _ _ _ _) = (label == name)
          fn _ = False

