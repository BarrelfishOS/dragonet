module Util.GraphHelpers(
    filterCtx,
    labelNode,
    findNodeByL,
    elliminateConflicts,
    mergeGraphsBy,
) where

import Data.Graph.Inductive
import Data.Either
import Data.Maybe
import qualified Data.List as L
import qualified Debug.Trace as TR

-- Convert Node to LNode in graph (node must be in graph)
labelNode :: Graph gr => gr a b -> Node -> Maybe (LNode a)
labelNode g n = do { a <- lab g n; return (n,a) }

findNodeByL :: Graph gr => (a -> Bool) -> gr a b -> Maybe (LNode a)
findNodeByL f g = L.find (\(_,l) -> f l) $ labNodes g

elliminateConflicts :: DynGraph gr => gr a b -> gr a b -> gr a b
elliminateConflicts a b = gmap fixC a
    where
        diff = (snd $ nodeRange b) - (fst $ nodeRange a) + 1
        fixN n = n + diff
        fixA l = map (\(l,n) -> (l,fixN n)) l
        fixC (i,n,l,o) = (fixA i,fixN n,l,fixA o)


-- Return all contexts in graph where the specified predicate returned true
filterCtx :: Graph gr => (Context a b -> Bool) -> gr a b -> [Context a b]
filterCtx p g = ufold f [] g
    where
        f ctx l = if p ctx then ctx:l else l


-- Integrate second graph into first graph, combining nodes according to predicate
mergeGraphsBy :: (DynGraph gr, Show a, Show b, Show (gr a b)) => (a -> a -> Bool) -> gr a b -> gr a b -> gr a b
mergeGraphsBy nC a b = flip insEdges gNodes $ map convertEdge $ labEdges b
    where
        na = labNodes a
        nb = labNodes b

        -- Build tuple with node ID the second graph and
        -- Either (Left a) (Right Node), left is for nodes not existing in the
        -- first graph and specifies the label, while right specifies a node ID
        -- from the first graph
        assocN ln = (n, maybe (Left l) (Right . fst) $ findNodeByL (nC l) a)
            where (n,l) = ln

        -- Allocate node in the first graph if no match from second graph was
        -- found. Return new graph and lists of tuples mapping from old ID to
        -- new ID.
        mapNode (g,l) (x,(Left y)) = (g',(x,Left n):l)
            where
                g' = insNode (n,y) g
                n = head $ newNodes 1 g
        mapNode (g,l) (x,(Right y)) = (g,(x,Right y):l)

        -- gNodes = graph with new nodes (but no edges yet)
        -- nMap = association list of old node IDs to Either Node Node. Left is
        -- used for newly allocated nodes, while right is used for existing
        -- nodes.
        nMap :: [(Node,Either Node Node)]
        (gNodes,nMap) = foldl mapNode (a,[]) $ map assocN nb
        convertNode n = either id id $ fromJust $ lookup n nMap
        convertEdge (nA,nB,l) = (convertNode nA, convertNode nB, l)


