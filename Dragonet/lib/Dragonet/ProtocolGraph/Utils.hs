module Dragonet.ProtocolGraph.Utils (
    dropSpawnEdges,
    entryNodes,
    orderedSpawns
) where

import Dragonet.ProtocolGraph

import Data.Function
import Data.Maybe
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Graph.Inductive as DGI

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

