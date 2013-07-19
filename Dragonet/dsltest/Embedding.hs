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
-- To support conversion from Node to String and vice versa

convertEdgeP :: (Node, String, Node) -> (String, String, String)
convertEdgeP (n1, p, n2) = ((nodeDefinition n1), p, (nodeDefinition n2))



convertLabelToNode :: [Node] -> String -> Node
convertLabelToNode nlist label = L.head $
    filter (\ x -> (nodeDefinition x) == label) nlist


convertEdgeToNodeP :: [Node] -> (String, String, String) -> (Node, String, Node)
convertEdgeToNodeP nlist (a, p, b) = (a', p, b')
    where
    a' = convertLabelToNode nlist a
    b' = convertLabelToNode nlist b



-- Get string label for GNode
gLabelStr gn = (gLabel gn)

nodeDefinition :: Node -> String
nodeDefinition n =
    case n of
        (Des (Decision gn))           -> ((gLabelStr gn) )
        (Opr (Operator gn))           -> ((gLabelStr gn) )
        (Conf (Configuration gn))     -> ((gLabelStr gn) )


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

getSoftStartNode :: Node
getSoftStartNode = getDecNode "inSoftware" "" (NaryNode []) []


----------------------------------------------------------------
--      Generic list based functionalities
----------------------------------------------------------------

-- Topological sort on edge list representation
topSort :: Eq a => [(a,a)] -> [a]
topSort [] = []
topSort es =
    noIncoming ++ orphaned ++ (topSort newEdges)
    where
        -- Is n a successor of another Node?
        notSucc n = MB.isNothing $ L.find (\x -> (snd x) == n) es
        -- All nodes without incoming edges
        noIncoming = filter (notSucc) $ L.nub $ map fst es
        -- edges that don't start at noIncoming nodes
        newEdges = filter (\x -> notElem (fst x) noIncoming) es
        -- edges that start at noIncoming nodes
        dropped = filter (\x -> elem (fst x) noIncoming) es
        -- Sink nodes (without outgoing edges) that lost their incoming edges
        -- those also vanish from the edges list
        isOrphaned n = MB.isNothing $ L.find
                (\x -> ((snd x) == n) || ((fst x) == n)) newEdges
        orphaned = L.nub $ filter isOrphaned $ map snd dropped


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

getfst :: (a, b, c) -> a
getfst (x, _, _) = x

get3snd :: (a, b, c) -> b
get3snd (_, p, _) = p

getThird :: (a, b, c) -> c
getThird (_, _, z) = z


-- For given edges get all nodes which are at source
getSRCNodesP :: (Eq a) => [(a, b, a)] -> [a]
getSRCNodesP gedges = L.map (getfst) gedges

-- For given edges get all nodes which are at destination
getDESTNodesP :: (Eq a) => [(a, b, a)] -> [a]
getDESTNodesP gedges = L.map (getThird) gedges

-- For given edges get all nodes which appear anywhere
getALLNodesP :: (Eq a) => [(a, b, a)] -> [a]
getALLNodesP gedges = L.nub $ (srcNodes ++ destNodes)
    where
    srcNodes = getSRCNodesP gedges
    destNodes = getDESTNodesP gedges

-- Find all edges where given Node appear at source
locateAsSRCEdgeP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
locateAsSRCEdgeP gedges gn = L.filter (\ (x, _, _) -> gn == x ) gedges

-- Find all edges where given Node appear at destination (sink)
locateAsDESTEdgeP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
locateAsDESTEdgeP gedges gn = L.filter (\ (_, _, z) -> gn == z ) gedges




----------------------------------------------------------------
--      Embedding algorithm: Set based
--      Node based version
----------------------------------------------------------------


edgeStart :: (a,b,c) -> a
edgeStart (n,_,_) = n

edgeLabel :: (a,b,c) -> b
edgeLabel (_,l,_) = l

edgeEnd :: (a,b,c) -> c
edgeEnd (_,_,n) = n

nodesSet :: ((Ord a),(Ord b)) => S.Set (a,b,a) -> S.Set a
nodesSet s = (S.map edgeStart s) `S.union` (S.map edgeEnd s)

inEdges n s = S.filter ((n ==) . edgeEnd) s
outEdges n s = S.filter ((n ==) . edgeStart) s
inNeighbours n s = S.map edgeStart $ inEdges n s


--embeddingV3Step sP sE [] = sE
--embeddingV3Step :: ((Show a), (Ord a)) => S.Set (a,a) -> S.Set (a,a) -> S.Set (a,a) -> S.Set (a,a)
embeddingV3Step :: ((Ord a), (Ord b)) => S.Set (a,b,a) -> (S.Set (a,b,a), S.Set a) -> (S.Set (a,b,a), S.Set a) -> S.Set (a,b,a)
embeddingV3Step sP (sE,sEv) (sU,sUv)
    | S.null sU = sE
    | sEv == sEv' = error "E not changing"
    | otherwise = embeddingV3Step sP (sE',sEv') (sU',sUv')
    where
        sPv = nodesSet sP

        -- Find dependencies of specified node in LPG and PRG
        deps n = (inNeighbours n sE) `S.union` (inNeighbours n sP)

        -- Find a node whose dependencies are already embedded
        v = S.findMin $ S.filter (flip S.isSubsetOf sEv . deps) sUv

        -- v's dependencies from PRG
        sEprg = S.filter ((v ==) . edgeEnd) sP -- S.map (mkEdge v) $ outNeighbours v sP
        -- v's dependencies from LPG
        sElpg' = S.filter (flip S.notMember sPv . edgeStart) $ inEdges v sU
        sElpg = if v `S.notMember` sPv then sElpg' else S.empty

        sE' = sE `S.union` sEprg `S.union` sElpg
        -- remove edges starting from v in
        sU' = sU S.\\ (inEdges v sU)

        -- Move vertex from E to U
        sEv' = S.insert v sEv
        sUv' = S.delete v sUv


-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingV3Wrapper ::  Node -> Node -> [(Node, String, Node)]
embeddingV3Wrapper prg lpg =
    result ++ softImplEdge
    where
--    result = convert $ embeddingV3Step prgEdges (S.empty,S.empty) (lpgEdges, allNodesSet)
--    result = convert prgHWedges'
    result = convert $ embeddingV3Step prgHWedges' (S.empty,S.empty) (lpgEdges, allNodesSet)

--    allNodesSet = (nodesSet lpgEdges) `S.union` (nodesSet prgEdges)
    allNodesSet = (nodesSet lpgEdges) `S.union` (nodesSet prgHWedges')
    prgNodes = L.nub $ nTreeNodes prg

    prgEmulatedNodes = L.map (nodeDefinition) $
        L.filter (not . isNodeEmbebible) prgNodes


    nodeList = L.nub (nTreeNodes lpg  ++ prgNodes ++ [swStartNode])
    convert n = map (\ x -> convertEdgeToNodeP nodeList x) $ S.toList n
    swStartNode = getSoftStartNode

    lpgEdges = S.fromList $ L.map convertEdgeP $ removeDroppedNodesP
            $  getDepEdgesP lpg
--    prgEdges = S.fromList $ L.map convertEdgeP $ removeDroppedNodesP
--            $ getDepEdgesP prg
    prgEdges = L.map convertEdgeP $ removeDroppedNodesP $ getDepEdgesP prg

    prgHWedges' = S.fromList $ prgHWedges

--    prgHWedges = prgEdges

    (prgHWedges, prgSWnodes) =  removeSWnodePRGP (L.nub prgEdges)
                                (isPRGNodeEmulated prgEmulatedNodes)

    defaultQueue = getDecNode "toDefaultQueue" ""
        (BinaryNode ([], [])) []
    softImplEdge =  [((defaultQueue), "", (swStartNode))]



removeDroppedNodesP :: [(Node, String, Node)] -> [(Node, String, Node)]
removeDroppedNodesP edgeList = L.filter (\ (_,_,y) ->
    not ( (nCompPrgLpg y dnode) ||  (nCompPrgLpg y dnode) ) ) edgeList
    where
    dnode = getDropNode








--class EmbeddableNode a where
--    embIsLPG :: a -> Bool
--    embIsPRG :: a -> Bool

data ENode =
    ELPG String [String] |
    EPRG String [String]
    deriving (Show, Eq, Ord)

enLabel :: ENode -> String
enLabel (ELPG l _) = l
enLabel (EPRG l _) = l

enAttributes :: ENode -> [String]
enAttributes (ELPG _ a) = a
enAttributes (EPRG _ a) = a

isLPG :: ENode -> Bool
isLPG (ELPG _ _) = True
isLPG _ = False

isPRG :: ENode -> Bool
isPRG = not . isLPG

isSoftware :: ENode -> Bool
isSoftware (ELPG _ _) = True
isSoftware (EPRG _ a) = elem "software" a

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
    setMinusL a b = S.filter (not . flip memberL b) a

    prgDeps n = inNeighbours n prg
    lpgDeps n = inNeighbours n lpg

    embeddingV4Step (sEn,sEe) sUn
        | S.null sUn = sEe
        {-| not $ sEn `S.isProperSubsetOf` sEn' = error "E not growing"-}
        | otherwise = embeddingV4Step (sEn',sEe') sUn'
        where
            (sEn',sEe') = (sEn `S.union` newNodes, sEe `S.union` newEdges)
            --sUn' = sUn `setMinusL` newNodes
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
        queueEdge  q = (q,"out",boundaryNode)
        fixedEdges = S.fromList $ concatMap fixEdge $ S.toList $ crossingEdges
        --fixEdge (s,p,e) = [(s,p,boundaryNode), (boundaryNode,boundaryNodePort,e)]
        fixEdge (s,p,e) = [(boundaryNode,boundaryNodePort,e)]
        boundaryNode = ELPG "SoftwareEntry" []
        boundaryNodePort = "out"


convertV4 :: S.Set (ENode,String,ENode) -> [(Node,String,Node)]
convertV4 edges =
    map convertEdge $ S.toList edges
    where
        convertEdge (a,p,b) = ((convertNode a),p,(convertNode b))
        
        convertNode :: ENode -> Node
        convertNode n = nodeF l "" e a
            where
                a = enAttributes n
                el = enLabel n
                l = case n of
                    (ELPG s _) -> "LPG:" ++ s
                    (EPRG s _) -> "PRG:" ++ s
                nodeF = if (take 3 el) == "OR:" || (take 4 el) == "AND:" then
                        getOperatorNode
                    else
                        getDecNode
                outedges = S.filter ((== n) . getfst) edges
                ports = S.map get3snd outedges
                edge p = (p, map (convertNode . getThird) $ S.toList $ S.filter ((== p) . get3snd) outedges)
                epl = map edge $ S.toList ports
                e = NaryNode epl

getConstraints :: S.Set (ENode,String,ENode) -> M.Map ENode (S.Set (ENode,String))
getConstraints g =
    gcStep nodes M.empty
    where
        nodes = (S.map edgeStart g) `S.union` (S.map edgeEnd g)
        gcStep ns a
            | S.null ns = a
            | otherwise = gcStep ns' a'
            where
                directConstraints = S.map (\(a,b,_) -> (a,b)) $ inEdges n g
                indirectConstraints = S.foldr S.union S.empty $ S.map (\x -> MB.fromJust $ M.lookup x a) $ inNeighbours n g
                constraints = directConstraints `S.union` indirectConstraints
                a' = M.insert n constraints a

                ns' = n `S.delete` ns
                n = S.findMin $ S.filter isSatisfied ns
                isSatisfied m = S.null $ inNeighbours m g `S.intersection` ns

data Ternary = TTrue | TFalse | TZ
    deriving (Show, Eq, Ord)

tcombine TTrue TTrue = TTrue
tcombine TTrue TZ = TTrue
tcombine TZ TTrue = TTrue
tcombine TFalse TFalse = TFalse
tcombine TZ TFalse = TFalse
tcombine TFalse TZ = TFalse
tcombine TZ TZ = TZ
tcombine TTrue TFalse = error "Contradiction"
tcombine TFalse TTrue = error "Contradiction"

tLabel TTrue = "T"
tLabel TFalse = "F"

fixedValue :: ENode -> S.Set (ENode,String) -> Ternary
fixedValue n c
    | isHardware n = TZ
    | (prefix `L.isPrefixOf` lbl) = check
    | otherwise = TZ
    where
        lbl = enLabel n
        prefix = "IsUDPDest"
        check = S.foldl tcombine TZ $ S.map val $ S.filter (L.isPrefixOf ("HW" ++ prefix) . enLabel . fst) c
        val (en,l)  =
            if el == "HW" ++ lbl then
                if l == "T" then
                    TTrue
                else if l == "F" then
                    TFalse
                else
                    TZ
            else if l == "T" then
                TFalse
            else
                TZ
            where
                el = enLabel en

applyConstraints :: S.Set (ENode,String,ENode) -> S.Set (ENode,String,ENode)
applyConstraints g =
    S.foldl step g nodes
    where
        nodes :: S.Set ENode
        nodes = (S.map edgeStart g) `S.union` (S.map edgeEnd g)
        constraints = getConstraints g
        step :: S.Set (ENode,String,ENode) -> ENode -> S.Set (ENode,String,ENode)
        step g' n
            | fv == TZ = g'
            | otherwise = TR.trace("fixed node " ++ (show n) ++ " to " ++ (show fv)) (g'')
            where
                c = MB.fromJust $ M.lookup n constraints
                fv = fixedValue n c
                l = tLabel fv
                g'' = (g' S.\\ dropEdges) --`S.union` addEdges
                dropEdges = S.filter (\e -> (edgeStart e == n) && (edgeLabel e /= l)) g'
                --addEdges = S.foldl S.union S.empty $ S.map edge $ inEdges n g'
                edge (a,b,_) = S.map (\e -> (a,b,e)) $ S.map edgeEnd $ S.filter ((== l) . edgeLabel) $ outEdges n g'


-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingV4Wrapper ::  [(Node, String, Node)] -> [(Node, String, Node)] -> [(Node, String, Node)]
embeddingV4Wrapper prg lpg =
    convertV4 $ constrained
    where
    lpgEdges = S.fromList $ map (convertEdgeV4 ELPG) $ lpg
    prgEdges = S.fromList $ map (convertEdgeV4 EPRG) $ prg

    embedded = embeddingV4 prgEdges lpgEdges
    serialized = serializeV4 embedded
    constrained = applyConstraints serialized

convertEdgeV4 :: (String -> [String] -> ENode) -> (Node, String, Node) -> (ENode, String, ENode)
convertEdgeV4 f (n1, p, n2) =
    ((node n1),p,(node n2))
    where
        node n = f (nLabel n) (nAttributes n)
        
----------------------------------------------------------------
--  A version that works on the String type
----------------------------------------------------------------


isNodeEmbebible :: Node -> Bool
isNodeEmbebible n
    | L.elem "inSoft" attrs = False
    | otherwise               = True
    where
    attrs = getNodeAttributes n


------------------------------------------------------------------------
-- Graph trimming helper functions for graphs with edge label
------------------------------------------------------------------------

{-
 - FIXME: I need to clearly understand, what exactly it means to drop the
 -      node from PRG.  Specially, in presence of True/False ports,
 -      it becomes much more complicated.
 -}
removeNodeP :: (Eq a) => (Show a) => (Show b) => [(a, b, a)] -> a -> [(a, b, a)]
removeNodeP edgesG v =
    --TR.trace ("Request to remove " ++ show v ++ " from graph " ++ show edgesG)
    edgesG''
    where
    -- Note: assumption: there is only one node in graph with given name

    sEdges n = L.map (\ (x', l) -> (x', l, n)) $ L.map
        (\ (x, y, _) -> (x, y)) $ locateAsDESTEdgeP edgesG v
    edgesG' = L.concatMap (\ (x, l, y) -> if x == v then (sEdges y)
                else [(x, l, y)]) edgesG

    cNodes = getDESTNodesP $ locateAsSRCEdgeP edgesG v
    dEdges n l = L.map (\ x' -> (n, l, x')) cNodes
    edgesG'' = L.concatMap (\ (x, l, y) -> if y == v then (dEdges x l)
                else [(x, l, y)]) edgesG'


--
-- remove all the nodes which are marked for sw execution
--      prgFn is function which tells if given node a is software node or not
removeSWnodePRGP :: (Eq a) => (Show a) => (Show b) => [(a, b, a)] -> (a -> Bool)
    -> ([(a, b, a)], [a])
removeSWnodePRGP prgEdges prgFn = (prgEdges', trueV)
    where

    -- Find all the nodes
    allV = getALLNodesP prgEdges

    -- Find set of nodes which should go in SW
    trueV = L.filter (prgFn) allV -- goes in software

    -- Delete these nodes from PRG
    prgEdges' =
        L.foldl (\ acc x -> removeNodeP acc x) prgEdges trueV


------------------------------------------------------------------
--
------------------------------------------------------------------


isPRGNodeEmulated :: (Eq a) => [a] -> a -> Bool
isPRGNodeEmulated vList v = L.elem v vList



----------------------------------------------------------------
--      Testing code
----------------------------------------------------------------

testEmbeddingV3 :: [(Node, String, Node)] -> [(Node, String, Node)] -> [(Node, String, Node)]
testEmbeddingV3 prg lpg = embeddingV4Wrapper prg lpg

----------------------------------------------------------------

