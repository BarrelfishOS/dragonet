module Dragonet.ProtocolGraph.Utils (
    dropSpawnEdges,
    entryNodes
) where

import Dragonet.ProtocolGraph

import Data.Maybe
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

