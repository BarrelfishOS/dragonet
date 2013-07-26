#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Embedding(
    testEmbeddingV3
    , getDepEdgesP
    , removeDroppedNodesP
    , topSort3
) where


import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MB
import qualified Data.Tuple as T

import qualified Debug.Trace as TR

import Operations

----------------------------------------------------------------
--  Node datatype manipulation
----------------------------------------------------------------

-- For given Node, find all its children
getChildrenP :: Node -> [(String,Node)]
getChildrenP root = case (getNodeEdges root) of
        (BinaryNode (as, bs))   -> L.nub ((pref "T" as) ++ (pref "F" bs))
        (NaryNode nList)        -> L.nub $ L.concatMap (uncurry pref) nList
    where
        pref i ls = map T.swap $ zip ls $ repeat i

-- Get outgoing edegs for all nodes underneath given Node
getOutEdgesP :: Node -> [(Node,String,Node)]
getOutEdgesP root = nextLevel ++ deeperLevels
    where
    children = getChildrenP root
    nextLevel = map (\(p,x) -> (root, p, x)) children
    deeperLevels = L.concatMap (getOutEdgesP . snd) children

-- Get outgoing edegs for all nodes underneath given Node
getDepEdgesP :: Node -> [(Node,String,Node)]
getDepEdgesP root = L.nub $ getOutEdgesP root


----------------------------------------------------------------
--      Generic list based functionalities
----------------------------------------------------------------

-- Topological sort on edge list representation
topSort3 :: Eq a => [(a,b,a)] -> [a]
topSort3 [] = []
topSort3 es =
    noIncoming ++ orphaned ++ (topSort3 newEdges)
    where
        fst3 (a,_,_) = a
        third3 (_,_,a) = a

        -- Is n a successor of another Node?
        notSucc n = MB.isNothing $ L.find (\x -> (third3 x) == n) es
        -- All nodes without incoming edges
        noIncoming = filter (notSucc) $ L.nub $ map fst3 es
        -- edges that don't start at noIncoming nodes
        newEdges = filter (\x -> notElem (fst3 x) noIncoming) es
        -- edges that start at noIncoming nodes
        dropped = filter (\x -> elem (fst3 x) noIncoming) es
        -- Sink nodes (without outgoing edges) that lost their incoming edges
        -- those also vanish from the edges list
        isOrphaned n = MB.isNothing $ L.find
                (\x -> ((third3 x) == n) || ((fst3 x) == n)) newEdges
        orphaned = L.nub $ filter isOrphaned $ map third3 dropped


----------------------------------------------------------------
-- Support functions which work on edges with labels
----------------------------------------------------------------

get3snd :: (a, b, c) -> b
get3snd (_, p, _) = p

getThird :: (a, b, c) -> c
getThird (_, _, z) = z


----------------------------------------------------------------
--      Embedding algorithm: Set based
--      Node based version
----------------------------------------------------------------


edgeStart :: (a,b,c) -> a
edgeStart (n,_,_) = n

edgeEnd :: (a,b,c) -> c
edgeEnd (_,_,n) = n

nodesSet :: ((Ord a),(Ord b)) => S.Set (a,b,a) -> S.Set a
nodesSet s = (S.map edgeStart s) `S.union` (S.map edgeEnd s)

inEdges n s = S.filter ((n ==) . edgeEnd) s
inNeighbours n s = S.map edgeStart $ inEdges n s

edgeNMap :: (a -> c) -> (a, b, a) -> (c, b, c)
edgeNMap f (a,b,c) = (f a, b, f c)




removeDroppedNodesP :: [(Node, String, Node)] -> [(Node, String, Node)]
removeDroppedNodesP edgeList = L.filter (\ (_,_,y) ->
    not ( (nCompPrgLpg y dnode) ||  (nCompPrgLpg y dnode) ) ) edgeList
    where
    dnode = getDropNode








--class EmbeddableNode a where
--    embIsLPG :: a -> Bool
--    embIsPRG :: a -> Bool

data ENode =
    ELPG String [String] String |
    EPRG String [String] String
    deriving (Show, Eq, Ord)

enLabel :: ENode -> String
enLabel (ELPG l _ _) = l
enLabel (EPRG l _ _) = l

enAttributes :: ENode -> [String]
enAttributes (ELPG _ a _) = a
enAttributes (EPRG _ a _) = a

enTag :: ENode -> String
enTag (ELPG _ _ t) = t
enTag (EPRG _ _ t) = t

isLPG :: ENode -> Bool
isLPG (ELPG _ _ _) = True
isLPG _ = False

isSoftware :: ENode -> Bool
isSoftware (ELPG _ _ _) = True
isSoftware (EPRG _ a _) = elem "software" a

isHardware :: ENode -> Bool
isHardware = not . isSoftware


-- Not very pretty :-/
enIsAnd :: ENode -> Bool
enIsAnd e = "AND:" `L.isPrefixOf` (enLabel e)

enIsOr :: ENode -> Bool
enIsOr e = "OR:" `L.isPrefixOf` (enLabel e)


--instance (EmbeddableNode ENode) where
--    embIsLPG (ELPG _) = True
--    embIsLPG _ = Fasle


matchLabel :: ENode -> ENode -> Bool
matchLabel a b = (enLabel a) == (enLabel b)


--embeddingV4Step :: ((Ord a), (Ord b)) => S.Set (a,b,a) -> (S.Set (a,b,a), S.Set a) -> (S.Set (a,b,a), S.Set a) -> S.Set (a,b,a)
embeddingV4 :: ((Ord a),(Show a)) => S.Set (ENode,a,ENode) -> S.Set (ENode,a,ENode) -> S.Set (ENode,a,ENode)
embeddingV4 prg lpg =
    embeddingV4Step (S.empty,S.empty) lpgNodes
    where
    prgNodes = nodesSet prg
    lpgNodes = nodesSet lpg
    memberL n s = not $ S.null $ S.filter (matchLabel n) s
    isSubsetOfL a b = all (flip memberL b) $ S.toList a
    nodeL n s = S.findMin $ S.filter (matchLabel n) s

    prgDeps n = inNeighbours n prg
    lpgDeps n = inNeighbours n lpg

    embeddingV4Step (sEn,sEe) sUn
        | S.null sUn = sEe
        | otherwise = embeddingV4Step (sEn',sEe') sUn'
        where
            (sEn',sEe') = (sEn `S.union` newNodes, sEe `S.union` newEdges)
            sUn' = S.delete v sUn

            -- Find a node whose dependencies are already embedded
            v = S.findMin $ S.filter (flip isSubsetOfL sEn . lpgDeps) sUn


            -- Find nodes and edges to add
            (newNodes,newEdges) = case prgEmb of
                Just a -> a        -- Take from PRG
                Nothing -> lpgEmb  -- Take from LPG

            prgEmb =
                if v `memberL` prgNodes then
                    pullPRGNode (nodeL v prgNodes) (Just (S.empty,S.empty))
                else
                    Nothing


            pullPRGNode _ Nothing = Nothing
            pullPRGNode n (Just (ns,es))
                | n `memberL` sEn = Just (ns,es) -- we arrived an an already embedded node
                | otherwise =
                    S.foldr pullPRGNode (Just (ns',es')) $ prgDeps n
                    where
                        ns' = S.insert n ns
                        es' = S.union es $ inEdges n prg
                    
            lpgNodeConvert n = nodeL n sEn
            lpgEdgeConvert (s,p,d) = ((lpgNodeConvert s),p,d)
            lpgEmb = (S.singleton v, S.map lpgEdgeConvert $ inEdges v lpg)
            

serializeV4 :: S.Set (ENode,String,ENode) -> S.Set (ENode,String,ENode)
serializeV4 graph =
    graph S.\\ crossingEdges `S.union` fixedEdges `S.union` queueEdges
    where
        crossingEdges = S.filter isCrossingEdge graph
        isCrossingEdge (s,_,e) = isHardware s && isSoftware e

        nodes = (S.map edgeStart graph) `S.union` (S.map edgeEnd graph)
        isQueue q = (isHardware q) && (L.isPrefixOf "Queue" $ enLabel q)
        queueEdges = S.map queueEdge $ S.filter isQueue $ nodes
        queueEdge  q = (q,"out",boundaryNode $ enLabel q)
        fixedEdges = S.fromList $ concatMap fixEdge $ S.toList $ crossingEdges
        fixEdge (_,_,e) = [(boundaryNode $ enTag e,boundaryNodePort,e)]
        boundaryNode l = ELPG "SoftwareEntry" [] l
        boundaryNodePort = "out"


convertV4 :: S.Set (ENode,String,ENode) -> [(Node,String,Node)]
convertV4 edges =
    map convertEdge $ S.toList edges
    where
        convertEdge (a,p,b) = ((convertNode a),p,(convertNode b))
        outMap = S.foldl addTo M.empty edges
        addTo m (x,y,z) =
            case M.lookup x m of
                Nothing -> M.insert x (S.singleton (x,y,z)) m
                Just l -> M.insert x (S.insert (x,y,z) l) m

        convertNode :: ENode -> Node
        convertNode n = nodeF l t e a
            where
                a = enAttributes n
                el = enLabel n
                l = case n of
                    (ELPG s _ _) -> "LPG:" ++ s
                    (EPRG s _ _) -> "PRG:" ++ s
                t = enTag n
                nodeF = if (take 3 el) == "OR:" || (take 4 el) == "AND:" then
                        getOperatorNode
                    else
                        getDecNode
                outedges = M.findWithDefault S.empty n outMap
                ports = S.map get3snd outedges
                edge p = (p, map (convertNode . getThird) $ S.toList $ S.filter ((== p) . get3snd) outedges)
                epl = map edge $ S.toList ports
                e = NaryNode epl

-- Track recursively backwards along incoming edges to specified node and
-- collect edges
prgRevReachable :: S.Set (ENode,String,ENode) -> ENode -> S.Set (ENode,String,ENode)
prgRevReachable g n
    | S.null edges = S.empty
    | otherwise = S.union edges $ S.fold S.union S.empty $ S.map (prgRevReachable g) nodes
    where
        edges = inEdges n g
        nodes = inNeighbours n g


-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingV4Wrapper ::  [(Node, String, Node)] -> [(Node, String, Node)] -> [(Node, String, Node)]
embeddingV4Wrapper prg lpg =
    convertV4 $ serialized
    where
    lpgEdges = S.fromList $ map (convertEdgeV4 ELPG) $ lpg
    prgEdges = S.fromList $ map (convertEdgeV4 EPRG) $ prg

    -- Replace 'Queue' node in LPG with specific queue label
    lpgQueue q = S.map replE lpgEdges
        where
            replE (a,b,c) = ((replN a),b,(replN c))
            replN (ELPG l a t)
                | l == "Queue" = ELPG q a t
                | otherwise = ELPG l a t

    -- Get prg for a particular queue (basically go recursively and get all
    -- nodes reachable from the queue node. This is necessary to avoid embedding
    -- functionality that is not available on a particular queue
    prgQueue q = prgRevReachable prgEdges n
        where n = S.findMin $ S.filter ((== q) . enLabel) $ nodesSet prgEdges

    -- Queue labels in PRG
    queues = map enLabel $ S.toList $ S.filter (L.isPrefixOf "Queue" . enLabel) $ nodesSet prgEdges

    -- Tag software node with queue label
    tagLPG q n
        | isHardware n = n
        | enIsAnd n = f l a q
        | enIsOr n = f l a q
        | otherwise = f l a q

        where
            f = if isLPG n then ELPG else EPRG
            l = enLabel n
            a = enAttributes n

    -- Calculate embedding for a particular queue
    embeddedSWQ q = S.map (edgeNMap $ tagLPG q) $ embeddingV4 (prgQueue q) (lpgQueue q)

    -- Get union of embeddings of each queue
    embedded = foldl S.union S.empty $ map embeddedSWQ $ queues

    serialized = serializeV4 embedded

convertEdgeV4 :: (String -> [String] -> String -> ENode) -> (Node, String, Node) -> (ENode, String, ENode)
convertEdgeV4 f (n1, p, n2) =
    ((node n1),p,(node n2))
    where
        node n = f (nLabel n) (nAttributes n) ""
        
----------------------------------------------------------------
--      Testing code
----------------------------------------------------------------

testEmbeddingV3 :: [(Node, String, Node)] -> [(Node, String, Node)] -> [(Node, String, Node)]
testEmbeddingV3 prg lpg = embeddingV4Wrapper prg lpg

----------------------------------------------------------------

