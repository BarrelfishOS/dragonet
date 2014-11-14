-- Generic FGL graph helpers
module Util.GraphHelpers(
    delLEdges,
    delDupEdges,
    filterCtx,
    reduceNodes,
    labelNode,
    findNodeByL, findNode,
    filterNodesByL,
    elliminateConflicts,
    mergeGraphsBy,
    MergeDecision(..),
    mergeGraphsBy',
    RecContext,
    recurseNFW,
    topsortLN,
    updateN,
    labPre, labSuc, labLPre, labLSucc,
    ledgeToEdge,
    labReachable,
    rdfsStop,
    nodeToLNode,
) where

import Data.Graph.Inductive
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.List as L
import qualified Control.Arrow as A
import Data.Maybe (mapMaybe,fromJust)

delLEdges :: (DynGraph gr, Eq b) => [LEdge b] -> gr a b -> gr a b
delLEdges es g = foldr delLEdge g es

-- Removes duplicate edges between two nodes (if the edges have the same label)
delDupEdges :: (DynGraph gr, Eq b) => gr a b -> gr a b
delDupEdges = gmap fixC
    where
        fixC (i,n,l,o) = (L.nub i,n,l,L.nub o)

-- Convert Node to LNode in graph (node must be in graph)
labelNode :: Graph gr => gr a b -> Node -> Maybe (LNode a)
labelNode g n = do { a <- lab g n; return (n,a) }

-- find based on its label
findNodeByL :: Graph gr => (a -> Bool) -> gr a b -> Maybe (LNode a)
findNodeByL f g = L.find (\(_,l) -> f l) $ labNodes g

findNode :: Graph gr => (LNode a -> Bool) -> gr a b -> Maybe (LNode a)
findNode f g = L.find f $ labNodes g

filterNodesByL :: Graph gr => (a -> Bool) -> gr a b -> [LNode a]
filterNodesByL f g = filter (\(_,l) -> f l) $ labNodes g

-- Make sure the node set of graph a is disjoint with the node set of b
elliminateConflicts :: DynGraph gr => gr a b -> gr a b -> gr a b
elliminateConflicts a b = gmap fixC a
    where
        diff = snd (nodeRange b) - fst (nodeRange a) + 1
        fixN n = n + diff
        fixA = map $ A.second fixN
        fixC (i,n,l,o) = (fixA i,fixN n,l,fixA o)


-- Return all contexts in graph where the specified predicate returned true
filterCtx :: Graph gr => (Context a b -> Bool) -> gr a b -> [Context a b]
filterCtx p = ufold f []
    where
        f ctx l = if p ctx then ctx:l else l

-- Remove all nodes not in the specified list
reduceNodes :: DynGraph gr => gr a b -> [Node] -> gr a b
reduceNodes g ns = delNodes toDelete g
    where
        toDelete = filter (`notElem` ns) $ nodes g

-- Adjacency list for context
type LAdj n e = [(e,n)]


type RecContext i ie n o oe = (LAdj (LNode i) ie,LNode n,LAdj (LNode o) oe)

-- Similar to nmap, but passes context (includes new labels of predecessors and
-- old labels of successors).
recurseNFW :: (DynGraph gr, Show nn, Show e, Show on) => (RecContext nn e on on e -> nn) -> gr on e
                                -> gr nn e
recurseNFW f g = gmap (\(ia,n,_,oa) -> let (Just lbl) = lookup n assocL
                                           in (ia,n,lbl,oa)) g
    where
        -- Association list from nodes to new labels
        assocL = foldl nfun [] $ DFS.topsort g
        -- Helper for generating association list of node to new label
        nfun l n = l ++ [(n,f $ recCtx l n)]
        recCtx l n = (inA',(n,nl),outA')
            where
                lblAdj lf = map (\(e,m) -> (e,(m,lf m)))
                inA' = lblAdj (\m -> let (Just a) = lookup m l in a) inA
                outA' = lblAdj (\m -> let (Just a) = lab g m in a) outA
                (inA,_,nl,outA) = context g n

-- Get lnodes in graph sorted in topological order
topsortLN :: Graph gr => gr a b -> [LNode a]
topsortLN g = map (\x -> let (Just y) = L.find ((== x) . fst) ln in y) $ topsort g
    where ln = labNodes g

-- Update the label of a particular node
updateN :: DynGraph gr => (a -> a) -> Node -> gr a b -> gr a b
updateN f n g = gmap change g
    where
        change ctx@(i,n',l,o)
            | n' == n = (i,n',f l,o)
            | otherwise = ctx


data MergeDecision =
    MergeNo |  -- Don't merge nodes
    Merge   |  -- Merge nodes, but drop all incoming and outgoing edges from b
    MergeIn |  -- Merge nodes, but only add incoming edges from new graph
    MergeInOut -- Merge nodes and add both incoming and outgoing edges

-- Integrate second graph into first graph, combining nodes according to predicate
mergeGraphsBy' :: DynGraph gr => (a -> a -> MergeDecision) -> gr a b -> gr a b -> gr a b
mergeGraphsBy' nC a b = flip insEdges gNodes $ mapMaybe convertEdge $ labEdges b
    where
        nb = labNodes b
        pred x y = case nC x y of
                    MergeNo -> False
                    _ -> True

        -- Build tuple with node ID the second graph and
        -- Either (Left a) (Right (Node,Bool)), left is for nodes not existing
        -- in the first graph and specifies the label, while right specifies a
        -- node ID from the first graph and the merge decision
        assocN ln =
            case findNodeByL (pred l) a of
                Nothing -> (n, Left l)
                Just (mn,ml) -> (n, Right (mn, nC l ml))
            where (n,l) = ln

        -- Allocate node in the first graph if no match from second graph was
        -- found. Return new graph and lists of tuples mapping from old ID to
        -- new ID.
        mapNode (g,l) (x,Left y) = (g',(x,Left n):l)
            where
                g' = insNode (n,y) g
                n = head $ newNodes 1 g
        mapNode (g,l) (x,Right y) = (g,(x,Right y):l)

        -- gNodes = graph with new nodes (but no edges yet)
        -- nMap = association list of old node IDs to Either Node Node. Left is
        -- used for newly allocated nodes, while right is used for existing
        -- nodes.
        nMap :: [(Node,Either Node (Node,MergeDecision))]
        (gNodes,nMap) = foldl mapNode (a,[]) $ map assocN nb

        -- Convert a node from an edge. Will return Nothing if this node is the
        -- edge source, but outgoing edges should not be added, same for
        -- incoming edeges
        convertNode n out = either Just convRight e'
            where
                (Just e') = lookup n nMap
                convRight (n',Merge) = Nothing
                convRight (n',MergeIn) = if out then Just n' else Nothing
                convRight (n',MergeInOut) = Just n'

        -- Convert edge if it should be added
        convertEdge (nA,nB,l) = do
            iN <- convertNode nA False
            oN <- convertNode nB True
            return (iN, oN, l)

-- Integrate second graph into first graph, combining nodes according to predicate
mergeGraphsBy :: DynGraph gr => (a -> a -> Bool) -> gr a b -> gr a b -> gr a b
mergeGraphsBy nC = mergeGraphsBy' ((fromPred .) . nC)
    where
        fromPred True = MergeInOut
        fromPred False = MergeNo

-- silly helper
nodeToLNode :: Graph gr => gr a b -> Node -> LNode a
nodeToLNode g n = (n, fromJust $ lab g n)

ledgeToEdge :: LEdge a -> Edge
ledgeToEdge (a,b,_) = (a,b)

-- labeled node predecessors on a graph
labPre :: Graph gr => gr a b -> LNode a -> [LNode a]
labPre g n = map (nodeToLNode g) $ pre g (fst n)

-- labeled node successors on a graph
labSuc :: Graph gr => gr  a b -> LNode a -> [LNode a]
labSuc g n = map (nodeToLNode g) $ suc g (fst n)

-- same as above, but includes edge labels
labLPre :: Graph gr => gr a b -> LNode a -> [(LNode a, LEdge b)]
labLPre graph node = map mapfn $ inn graph (fst node)
    where mapfn edge@(src, _, _) = (nodeToLNode graph src, edge)

-- labaled node sucessors on a graph
labLSucc :: Graph gr => gr a b -> LNode a -> [(LNode a, LEdge b)]
labLSucc graph node = map mapfn $ out graph (fst node)
    where mapfn edge@(_, dst, _) = (nodeToLNode graph dst, edge)

-- DFS.reachable on labled nodes
labReachable :: Graph gr => gr a b -> LNode a -> [LNode a]
labReachable gr src = map (nodeToLNode gr) $ DFS.reachable (fst src) gr


-- similar to xdfsWith, but the search is stopped when a particular node is
-- found. The node is included in the results
xdfsWithStop ::
    Graph gr =>
    DFS.CFun a b [Node] ->  -- function to get a list of nodes out of a context
                            -- (e.g,. pre', suc')
    DFS.CFun a b c ->       -- function to get a return item from a context
                            -- (e.g., node')
    (LNode a -> Bool) ->    -- function for deciding when to stop
    [LNode a] ->            -- starting points for search
    gr a b ->               -- graph
    [c]                     -- return items
xdfsWithStop getNodes getResult stopSearch nodes graph =
    DFS.xdfsWith getNodes' getResult nodes' graph
        where getNodes' ctx@(_, node, lnode, _) | stopSearch (node, lnode) = []
                                                | otherwise = getNodes ctx
              nodes' = map fst nodes

rdfsStop :: Graph gr => (LNode a -> Bool) -> [LNode a] -> gr a b -> [LNode a]
rdfsStop = xdfsWithStop pre' labNode'
