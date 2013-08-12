module Util.GraphHelpers(
    delLEdges,
    delDupEdges,
    filterCtx,
    reduceNodes,
    labelNode,
    findNodeByL,
    elliminateConflicts,
    mergeGraphsBy,
    RecContext,
    recurseNFW,
) where

import Data.Graph.Inductive
import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Either
import Data.Maybe
import qualified Data.List as L
import qualified Debug.Trace as TR

delLEdges :: (DynGraph gr, Eq b) => [LEdge b] -> gr a b -> gr a b
delLEdges es g = foldr delLEdge g es

-- Removes duplicate edges between two nodes (if the edges have the same label)
delDupEdges :: (DynGraph gr, Eq b) => gr a b -> gr a b
delDupEdges = gmap fixC
    where
        fixA l = L.nub l
        fixC (i,n,l,o) = (fixA i,n,l,fixA o)

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

-- Remove all nodes not in the specified list
reduceNodes :: DynGraph gr => gr a b -> [Node] -> gr a b
reduceNodes g ns = delNodes toDelete g
    where
        toDelete = filter (flip notElem ns) $ nodes g

-- Integrate second graph into first graph, combining nodes according to predicate
mergeGraphsBy :: DynGraph gr => (a -> a -> Bool) -> gr a b -> gr a b -> gr a b
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

-- Adjacency list for context
type LAdj n e = [(e,n)]


type RecContext i ie n o oe = (LAdj (LNode i) ie,LNode n,LAdj (LNode o) oe)

-- Similar to nmap, but passes context (includes new labels of predecessors and
-- old labels of successors).
recurseNFW :: (DynGraph gr, Show nn, Show e, Show on) => (RecContext nn e on on e -> nn) -> gr on e
                                -> gr nn e
recurseNFW f g = gmap (\(ia,n,_,oa) -> (ia,n,fromJust $ lookup n assocL,oa)) g
    where
        -- Association list from nodes to new labels
        assocL = foldl nfun [] $ DFS.topsort g
        -- Helper for generating association list of node to new label
        nfun l n = l ++ [(n,f $ recCtx l n)]
        recCtx l n = (inA',(n,nl),outA')
            where
                lblAdj lf = map (\(e,m) -> (e,(m,lf m))) inA
                inA' = lblAdj (\m -> fromJust $ lookup m l)
                outA' = lblAdj (\m -> fromJust $ lab g n)
                (inA,_,nl,outA) = context g n

