#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Embedding(
    testEmbeddingList
    , testPRGrearrangement
    , testEmbeddingSTR
    , testEmbeddingSet
    , getDepEdgesP
    , getDepEdges
    , removeDroppedNodesP
    , removeDroppedNodes
) where


import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as MB
import qualified Data.List as DL
import qualified Data.Tuple as T

import qualified Debug.Trace as TR

import qualified NetBasics as NB
import Operations

----------------------------------------------------------------
-- To support conversion from Node to String and vice versa
showEdgeGen :: (Show a) => (a, a) -> String
showEdgeGen (n1, n2) = ((show n1) ++ " --> " ++ (show n2)
            ++ "\n")


convertEdge :: (Node, Node) -> (String, String)
convertEdge (n1, n2) = ((nodeDefinition n1),  (nodeDefinition n2))

convertEdgeP :: (Node, String, Node) -> (String, String, String)
convertEdgeP (n1, p, n2) = ((nodeDefinition n1), p, (nodeDefinition n2))


convertLabelToNode :: [Node] -> String -> Node
convertLabelToNode nlist label = DL.head $
    filter (\ x -> (nodeDefinition x) == label) nlist


convertEdgeToNode :: [Node] -> (String, String) -> (Node, Node)
convertEdgeToNode nlist (a, b) = (a', b')
    where
    a' = convertLabelToNode nlist a
    b' = convertLabelToNode nlist b

convertEdgeToNodeP :: [Node] -> (String, String, String) -> (Node, String, Node)
convertEdgeToNodeP nlist (a, p, b) = (a', p, b')
    where
    a' = convertLabelToNode nlist a
    b' = convertLabelToNode nlist b



-- Get string label for GNode
gLabelStr gn = NB.graphLabelStr (gLabel gn)

nodeDefinition :: Node -> String
nodeDefinition n =
    case n of
        (Des (Decision gn))           -> ((gLabelStr gn) )
        (Opr (Operator gn))           -> ((gLabelStr gn) )
        (Conf (Configuration gn))     -> ((gLabelStr gn) )


showEdge :: (Node, Node) -> String
showEdge  (n1, n2) = ((nodeDefinition n1) ++ " --> " ++ (nodeDefinition n2)
            ++ "\n")


----------------------------------------------------------------
--  Node datatype manipulation
----------------------------------------------------------------

getChildren :: Node -> [Node]
getChildren root = case (getNodeEdges root) of
        (BinaryNode (as, bs))   -> DL.nub (as ++ bs)
        (NaryNode nList)        -> DL.nub $ DL.concatMap (\ (x, y) -> y) nList

-- For given Node, find all its children
getChildrenP :: Node -> [(String,Node)]
getChildrenP root = case (getNodeEdges root) of
        (BinaryNode (as, bs))   -> DL.nub ((pref "T" as) ++ (pref "F" bs))
        (NaryNode nList)        -> DL.nub $ DL.concatMap (uncurry pref) nList
    where
        pref i ls = map T.swap $ zip ls $ repeat i

getOutEdges :: Node -> [(Node,Node)]
getOutEdges root = nextLevel ++ deeperLevels
    where
    children = getChildren root
    nextLevel = DL.concatMap (\ x -> [(root, x)]) children
    deeperLevels = DL.concatMap (getOutEdges) children


getDepEdges :: Node -> [(Node,Node)]
getDepEdges root =  DL.map (\ (x, y) -> (y, x)) $ getOutEdges root

-- Get outgoing edegs for all nodes underneath given Node
getOutEdgesP :: Node -> [(Node,String,Node)]
getOutEdgesP root = nextLevel ++ deeperLevels
    where
    children = getChildrenP root
    nextLevel = map (\(p,x) -> (root, p, x)) children
    deeperLevels = DL.concatMap (getOutEdgesP . snd) children

-- Get outgoing edegs for all nodes underneath given Node
getDepEdgesP :: Node -> [(Node,String,Node)]
--getDepEdges root =  DL.map (\ (x, p, y) -> (y, p, x)) $ getOutEdges root
getDepEdgesP root = getOutEdgesP root

getSoftStartNode :: Node
getSoftStartNode = getDecNode NB.InSoftware "" (NaryNode []) []

getSoftStartNode1 :: String
getSoftStartNode1 = nodeDefinition getSoftStartNode


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



isElemBy :: (Eq a) => (a -> a -> Bool) -> [a] -> a -> Bool
isElemBy fn list e
    | matched == [] = False
    | otherwise = True
    where
    matched = DL.filter (fn e) list



-- Get all nodes which don't have any dependencies
noDepNodes1 :: (Eq a) =>[(a, a)] -> [a]
noDepNodes1 depList = nonDep
    where
    srcNodes = DL.nub $ DL.map (fst) depList
    dstNodes = DL.nub $ DL.map (snd) depList
    nonDep = DL.filter (\ x -> x `DL.notElem` srcNodes ) dstNodes


-- For given edges get all nodes which are at source
getSRCNodes :: (Eq a) => [(a, a)] -> [a]
getSRCNodes gedges = DL.map (fst) gedges

-- For given edges get all nodes which are at destination
getDESTNodes :: (Eq a) => [(a, a)] -> [a]
getDESTNodes gedges = DL.map (snd) gedges

-- For given edges get all nodes which appear anywhere
getALLNodes :: (Eq a) => [(a, a)] -> [a]
getALLNodes gedges = DL.nub $ (srcNodes ++ destNodes)
    where
    srcNodes = getSRCNodes gedges
    destNodes = getDESTNodes gedges

-- Find all edges where given Node appear at source
locateAsSRCEdge :: (Eq a) => [(a, a)] -> a -> [(a, a)]
locateAsSRCEdge gedges gn = DL.filter (\ (x, y) -> gn == x ) gedges

-- Find all edges where given Node appear at destination (sink)
locateAsDESTEdge :: (Eq a) => [(a, a)] -> a -> [(a, a)]
locateAsDESTEdge gedges gn = DL.filter (\ (x, y) -> gn == y ) gedges

-- Find all edges where given Node appear anywhere in edge
locateAsANYEdge :: (Eq a) => [(a, a)] -> a -> [(a, a)]
locateAsANYEdge gedges gn = DL.filter (\ (x, y) -> gn == x || gn == y) gedges

-- Find all direct and indirect of given Node till there are no deps left
getAllDeps :: (Eq a) => [(a, a)] -> a -> [(a, a)]
getAllDeps graphEdges gn = deps ++ indirectDeps
    where
    deps = locateAsSRCEdge graphEdges gn
    indirectDeps = DL.concatMap (\ (x, y) -> getAllDeps graphEdges y) deps


----------------------------------------------------------------
-- Support functions which work on edges with labels
----------------------------------------------------------------

getfst :: (a, b, c) -> a
getfst (x, y, z) = x

getThird :: (a, b, c) -> c
getThird (x, y, z) = z


-- Get all nodes which don't have any dependencies
noDepNodes1P :: (Eq a) =>[(a, b, a)] -> [a]
noDepNodes1P depList = nonDep
    where
    srcNodes = DL.nub $ DL.map (getfst) depList
    dstNodes = DL.nub $ DL.map (getThird) depList
    nonDep = DL.filter (\ x -> x `DL.notElem` srcNodes ) dstNodes


-- For given edges get all nodes which are at source
getSRCNodesP :: (Eq a) => [(a, b, a)] -> [a]
getSRCNodesP gedges = DL.map (getfst) gedges

-- For given edges get all nodes which are at destination
getDESTNodesP :: (Eq a) => [(a, b, a)] -> [a]
getDESTNodesP gedges = DL.map (getThird) gedges

-- For given edges get all nodes which appear anywhere
getALLNodesP :: (Eq a) => [(a, b, a)] -> [a]
getALLNodesP gedges = DL.nub $ (srcNodes ++ destNodes)
    where
    srcNodes = getSRCNodesP gedges
    destNodes = getDESTNodesP gedges

-- Find all edges where given Node appear at source
locateAsSRCEdgeP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
locateAsSRCEdgeP gedges gn = DL.filter (\ (x, _, _) -> gn == x ) gedges

-- Find all edges where given Node appear at destination (sink)
locateAsDESTEdgeP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
locateAsDESTEdgeP gedges gn = DL.filter (\ (_, _, z) -> gn == z ) gedges

-- Find all edges where given Node appear anywhere in edge
locateAsANYEdgeP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
locateAsANYEdgeP gedges gn = DL.filter (\ (x, _, z) -> gn == x || gn == z) gedges

-- Find all direct and indirect of given Node till there are no deps left
getAllDepsP :: (Eq a) => [(a, b, a)] -> a -> [(a, b, a)]
getAllDepsP graphEdges gn = deps ++ indirectDeps
    where
    deps = locateAsSRCEdgeP graphEdges gn
    indirectDeps = DL.concatMap (\ (_, _, z) -> getAllDepsP graphEdges z) deps




----------------------------------------------------------------
--      Embedding algorithm: Set based
--      Node based version
----------------------------------------------------------------



edgeStart (n,_,_) = n -- get nodes which are at SRC of some edge
edgeEnd (_,_,n) = n -- get nodes which are at DEST of some edge

-- Get all nodes from edge set
nodesSet s = (S.map edgeStart s) `S.union` (S.map edgeEnd s)

-- set of all edges which are incoming to node n
inEdges n s = S.filter ((n ==) . edgeEnd) s

-- set of all edges which are outgoing from node n
outEdges n s = S.filter ((n ==) . edgeStart) s

-- set of all nodes which appear immediately before node n
--      ie: they are parent of node n
--      ie: n is dependent on them
inNeighbours n s = S.map edgeStart $ inEdges n s



--embeddingSetStep sP sE [] = sE
--embeddingSetStep :: ((Show a), (Ord a)) => S.Set (a,a) -> S.Set (a,a) -> S.Set (a,a) -> S.Set (a,a)
embeddingSetStep :: (Ord a) => S.Set (a,a,a) -> (S.Set (a,a,a), S.Set a) -> (S.Set (a,a,a), S.Set a) -> S.Set (a,a,a)
embeddingSetStep sP (sE,sEv) (sU,sUv)
    | S.null sU = sE
    | sEv == sEv' = error "E not changing"
    | otherwise = embeddingSetStep sP (sE',sEv') (sU',sUv')
    where
        sPv = nodesSet sP -- All the vertices from PRG

        -- Find dependencies of specified node in LPG and PRG
        --      PS: shouldn't this be dependencies in embedded and unembedded LPG???
        deps n = (inNeighbours n sE) `S.union` (inNeighbours n sP)

        -- Find a node whose dependencies are already embedded
        v = S.findMin $ S.filter (flip S.isSubsetOf sEv . deps) sUv
        -- PS: v = any {x | x elem sU ^ forAll (x, y) -> y elem sE }

        -- v's dependencies from PRG
        --sEprg = S.filter ((v ==) . edgeEnd) sP
        sEprg = inEdges v sP  -- PS: this is more concise

        -- v's dependencies from LPG (after removing those which are already in PRG)
        sElpg' = S.filter (flip S.notMember sPv . edgeStart) $ inEdges v sU

        -- if node v does not go in hardware then take LPG deps, otherwise don't
        sElpg = if v `S.notMember` sPv then sElpg' else S.empty

        -- Finally, the edges associated with v are those from prg and lpg
        --          after filtering lpg edges
        -- So, Lets move the selected edges in embedded
        sE' = sE `S.union` sEprg `S.union` sElpg

        -- we will remove selected edges from unembedded part
        sU' = sU S.\\ (inEdges v sU)

        -- Move vertex from E to U
        sEv' = S.insert v sEv
        sUv' = S.delete v sUv


embeddingSetStepV2 :: (Ord a) => (a -> a -> Bool) -> S.Set (a,a,a)
        -> (S.Set (a,a,a), S.Set a) -> (S.Set (a,a,a), S.Set a) -> S.Set (a,a,a)
embeddingSetStepV2 cmpFn sP (sE,sEv) (sU,sUv)
    | S.null sU = sE
    | sEv == sEv' = error "E not changing"
    | otherwise = embeddingSetStepV2 cmpFn sP (sE',sEv') (sU',sUv')
    where
        sPv = nodesSet sP -- All the vertices from PRG

        -- Find dependencies of specified node in LPG and PRG
        --      PS: shouldn't this be dependencies in embedded and unembedded LPG???
--        deps n = (inNeighbours n sE) `S.union` (inNeighbours n sP)
        deps n = (inNeighbours n sE) `S.union` (inNeighbours n sU)

        -- Find a node whose dependencies are already embedded
        v = S.findMin $ S.filter (flip S.isSubsetOf sEv . deps) sUv
        -- PS: v = any {x | x elem sU ^ forAll (x, y) -> y elem sE }

        -- v's dependencies from PRG
        --sEprg = S.filter ((v ==) . edgeEnd) sP
        sEprg = inEdges v sP  -- PS: this is more concise

        -- v's dependencies from LPG (after removing those which are already in PRG)
        sElpg' = S.filter (flip S.notMember sPv . edgeStart) $ inEdges v sU

        -- if node v does not go in hardware then take LPG deps, otherwise don't
        sElpg = if v `S.notMember` sPv then sElpg' else S.empty

        -- Finally, the edges associated with v are those from prg and lpg
        --          after filtering lpg edges
        -- So, Lets move the selected edges in embedded
        sE' = sE `S.union` sEprg `S.union` sElpg

        -- we will remove selected edges from unembedded part
        sU' = sU S.\\ (inEdges v sU)

        -- Move vertex from E to U
        sEv' = S.insert v sEv
        sUv' = S.delete v sUv




-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingSet ::  Node -> Node -> [(Node, String, Node)]
embeddingSet prg lpg =
    result -- ++ softImplEdge
    where
--    result = convert $ embeddingSetStep prgEdges (S.empty,S.empty) (lpgEdges, allNodesSet)
--    result = convert prgHWedges'
--    result = convert $ embeddingSetStep prgHWedges' (S.empty,S.empty) (lpgEdges, allNodesSet)
    result = convert $ embeddingSetStepV2 (==) prgHWedges' (S.empty,S.empty) (lpgEdges, allNodesSet)

--    allNodesSet = (nodesSet lpgEdges) `S.union` (nodesSet prgEdges)
    allNodesSet = (nodesSet lpgEdges) `S.union` (nodesSet prgHWedges')
    prgNodes = DL.nub $ nTreeNodes prg

    prgEmulatedNodes = DL.map (nodeDefinition) $
        DL.filter (not . isNodeEmbebible) prgNodes

    nodeList = DL.nub (nTreeNodes lpg  ++ prgNodes ++ [swStartNode])
    convert n = map (\ x -> convertEdgeToNodeP nodeList x) $ S.toList n
    swStartNode = getSoftStartNode
    swStartNode1 = nodeDefinition swStartNode

    lpgEdges = S.fromList $ DL.map convertEdgeP $ removeDroppedNodesP
            $  getDepEdgesP lpg

--    prgEdges = S.fromList $ DL.map convertEdgeP $ removeDroppedNodesP
--            $ getDepEdgesP prg
    prgEdges = DL.map convertEdgeP $ removeDroppedNodesP $ getDepEdgesP prg

    prgHWedges' = S.fromList $ prgHWedges

    prgHWedges = prgEdges

--    (prgHWedges, prgSWnodes) =  removeSWnodePRGP (DL.nub prgEdges)
--                                (isPRGNodeEmulated prgEmulatedNodes)

    defaultQueue = getDecNode (NB.ToQueue NB.getDefaultQueue) ""
        (BinaryNode ([], [])) []
    softImplEdge =  [((defaultQueue), "", (swStartNode))]



removeDroppedNodesP :: [(Node, String, Node)] -> [(Node, String, Node)]
removeDroppedNodesP edgeList = edgeList
{-
removeDroppedNodesP edgeList = DL.filter (\ (x,_,y) ->
    not ( (nCompPrgLpg y dnode) ||  (nCompPrgLpg y dnode) ) ) edgeList
    where
    dnode = getDropNode
-}


----------------------------------------------------------------
--  A version that works on the String type
----------------------------------------------------------------


{-
 - The basic algo:
 -  If Node in H/W ---> copy PRG deps
 -  If Node in S/W --->
 -      no child in H/W  ---> copy LPG deps
 -      some children in H/W ---> (boundary case) copy PRG deps for nodes in HW
 -                                  copy LPG deps for nodes in SW
-}
embeddingListStep :: (Show a) => (Eq a) => a -> [(a, a)] -> (a -> Bool) ->
    (([(a, a)], [(a, a)]), [(a, a)]) ->  (([(a, a)], [(a, a)]), [(a, a)])
embeddingListStep swStartNode prgEdges prgFn ((embedHW, embedSW), lpgUnembedded)
    | lpgUnembedded == []   = ((embedHW, embedSW), [])
    | offendingDeps /= []   = error ("few deps of next Node to embedd [ " ++
            (show vNext) ++ " ]  are not embedded ")
    | otherwise             = embeddingListStep swStartNode prgEdges prgFn
                    ((embedHW', embedSW'), lpgUnembedded')
    where
    allLPGEdges = lpgUnembedded ++ (embedHW ++ embedSW)
    vNext = fst $ DL.head lpgUnembedded

    -- get list of all dependencies
    depEdges = locateAsSRCEdge allLPGEdges vNext
    depNodes = getDESTNodes depEdges

    -- sanitcy check: find all deps which are already not in lpgEmbedded
    embeddedNodes =  (noDepNodes1 allLPGEdges) ++
            (getALLNodes  (embedHW ++ embedSW))
    offendingDeps = DL.filter (\ x -> DL.notElem x embeddedNodes) depNodes

    -- find deps which can be embedded in hardware
    dInHW = DL.filter (\ x -> locateAsANYEdge prgEdges x /= []) depNodes

    -- find deps which are are not in HW and hence should be embedded in SW
    dOnlyInSW = DL.filter (\ x -> DL.notElem x dInHW) depNodes

    -- Find dep edges which should go in HW
    --      All direct and indirect deps of the Node.
    edgesInHW = getAllDeps prgEdges vNext

    (prgSW, prgHW) = classifyDeps prgEdges prgFn

    -- edgesInHW = DL.concatMap (\ x -> locateAsANYEdge prgEdges x) dInHW

    -- Find dep edges which should go in SW
    --     that is: edges in LPG with vNext as src and all the children nodes
    --                  which are not in HW

    -- Selection if you want all edges, or only those edges which are going
    --  to node "InSoftware"
    edgesInSW = DL.filter (\ (x, y) -> x == vNext && DL.notElem y dInHW) depEdges
    -- edgesInSW = DL.filter (\ (x, y) -> x == vNext) depEdges

    -- Explicit dep on InSoft
    explicitSWDep = [(vNext, swStartNode)]

    (newEdgesHW, newEdgesSW)
        | locateAsANYEdge prgEdges vNext /= []  =
            TR.trace ("Vertex "
            ++ show vNext ++ " is completely in HW \n" ++
            ((DL.concatMap showEdgeGen edgesInHW)))
                (edgesInHW, [])   -- completely in HW
        | dInHW == [] =
            TR.trace ("Vertex " ++ show vNext ++
            " is completely from SW with edges \n" ++
            ((DL.concatMap showEdgeGen edgesInSW)))
                ([], edgesInSW)   -- completely in SW
        | otherwise =
            TR.trace ("Vertex " ++ show vNext ++ " is on the boundary, {{{\n"
            ++ "\n\n\nInHW\n" ++ ((DL.concatMap showEdgeGen edgesInHW))
            ++ "\n\n\nInSW\n" ++ ((DL.concatMap showEdgeGen edgesInSW))
            ++ "\n\n\nExPL\n" ++ ((DL.concatMap showEdgeGen explicitSWDep))
            ++ "\n}}}\n")
            (edgesInHW, (edgesInSW ++ explicitSWDep)) -- on the boundary
--            (edgesInHW ++ edgesInSW ++ explicitSWDep)
    -- Move the Node from unembedded to embedded.
    -- This implies that we move all the edges associated with it.

    -- TODO: Find all the HW edges that are getting newly added into embedded graph
    -- TODO: out of those, find ones which should go in software

    lpgUnembedded' = DL.filter (\ (x, y) -> x /= vNext) lpgUnembedded
    (embedHW', embedSW') = ((embedHW ++ newEdgesHW), (embedSW ++ newEdgesSW))

removeDroppedNodes :: [(Node, Node)] -> [(Node, Node)]
removeDroppedNodes edgeList = DL.filter (\ (x,y) ->
    not ( (nCompPrgLpg x dnode) ||  (nCompPrgLpg x dnode) ) ) edgeList
    where
    dnode = getDropNode


isNodeEmbebible :: Node -> Bool
isNodeEmbebible n
    | DL.elem (NB.InSoft True) attrs = False
    | otherwise               = True
    where
    attrs = getNodeAttributes n


------------------------------------------------------------------
--      Code to rearrange PRG
------------------------------------------------------------------

--
-- rearrange PRG to move software nodes at end
--      prgFn is function which tells if given node a is software node or not
--      swNode tell the node on which all nodes who are supposed to go in
--              software should depend
rearrangePRG :: (Eq a) => (Show a) => [(a, a)] -> (a -> Bool) -> a
    -> [(a, a)]
rearrangePRG prgEdges prgFn swNode = prgEdges' ++ swDepEdges
    where

    -- Find all the nodes
    allV = getALLNodes prgEdges

    -- Find set of nodes which should go in SW
    trueV = DL.filter (prgFn) allV -- goes in software

    -- Delete these nodes from PRG
    prgEdges' =
        TR.trace ("nodes in HW " ++ show trueV)
        DL.foldl (\ acc x -> removeNode acc x) prgEdges trueV

    -- Add these nodes as dep on software boundary node
    swDepEdges = DL.map (\ x -> (x, swNode)) trueV


removeNode :: (Eq a) => (Show a) => [(a, a)] -> a -> [(a, a)]
removeNode edgesG v = edgesG''
    where
    -- Note: assumption: there is only one node in graph with given name

    pNodes = getSRCNodes $ locateAsDESTEdge edgesG v

    sEdges n = DL.map (\ x' -> (x', n)) pNodes
    edgesG' = DL.concatMap (\ (x, y) -> if x == v then (sEdges y)
                else [(x, y)]) edgesG

    cNodes = getDESTNodes $ locateAsSRCEdge edgesG v
    dEdges n = DL.map (\ x' -> (n, x')) cNodes
    edgesG'' = DL.concatMap (\ (x, y) -> if y == v then (dEdges x)
                else [(x, y)]) edgesG'



--
-- remove all the nodes which are marked for sw execution
--      prgFn is function which tells if given node a is software node or not
removeSWnodePRG :: (Eq a) => (Show a) => [(a, a)] -> (a -> Bool)
    -> ([(a, a)], [a])
removeSWnodePRG prgEdges prgFn = (prgEdges', trueV)
    where

    -- Find all the nodes
    allV = getALLNodes prgEdges

    -- Find set of nodes which should go in SW
    trueV = DL.filter (prgFn) allV -- goes in software

    -- Delete these nodes from PRG
    prgEdges' =
        TR.trace ("nodes in HW " ++ show trueV)
        DL.foldl (\ acc x -> removeNode acc x) prgEdges trueV

-- classify given edges based on some condition specified by function fn
classifyDeps :: (Eq a) => [(a, a)] -> (a -> Bool) -> ([(a, a)], [(a, a)])
classifyDeps graphEdges fn = (trueEdges, falseEdges)
    where
    trueEdges = DL.filter (\ (x, y) -> fn y) graphEdges
    falseEdges = DL.filter (\ (x, y) -> not $ fn y) graphEdges


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

    sEdges n = DL.map (\ (x', l) -> (x', l, n)) $ DL.map
        (\ (x, y, _) -> (x, y)) $ locateAsDESTEdgeP edgesG v
    edgesG' = DL.concatMap (\ (x, l, y) -> if x == v then (sEdges y)
                else [(x, l, y)]) edgesG

    cNodes = getDESTNodesP $ locateAsSRCEdgeP edgesG v
    dEdges n l = DL.map (\ x' -> (n, l, x')) cNodes
    edgesG'' = DL.concatMap (\ (x, l, y) -> if y == v then (dEdges x l)
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
    trueV = DL.filter (prgFn) allV -- goes in software

    -- Delete these nodes from PRG
    prgEdges' =
        DL.foldl (\ acc x -> removeNodeP acc x) prgEdges trueV


-- classify given edges based on some condition specified by function fn
classifyDepsP :: (Eq a) => [(a, b, a)] -> (a -> Bool)
        -> ([(a, b, a)], [(a, b, a)])
classifyDepsP graphEdges fn = (trueEdges, falseEdges)
    where
    trueEdges = DL.filter (\ (x, y, z) -> fn z) graphEdges
    falseEdges = DL.filter (\ (x, y, z) -> not $ fn z) graphEdges


------------------------------------------------------------------
--
------------------------------------------------------------------


isPRGNodeEmulated :: (Eq a) => [a] -> a -> Bool
isPRGNodeEmulated vList v = DL.elem v vList



-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingList ::  Node -> Node -> [(Node, Node)]
embeddingList prg lpg
    | unemb == [] = TR.trace ("\n\n\nDone with embedding "
            ++ "\nIn HW {{{\n" ++ ((DL.concatMap showEdgeGen (DL.nub embHW)))
            ++ "}}}\n\nIn SW {{{\n" ++ ((DL.concatMap showEdgeGen (DL.nub embSW)))
            ++ "}}}\n\n\n"
        ) emb''
    | otherwise = error "not all nodes embedded"
    where

    prgNodes = DL.nub $ nTreeNodes prg
    prgEmulatedNodes = DL.map (nodeDefinition) $
        DL.filter (not . isNodeEmbebible) prgNodes





    nodeList = DL.nub (nTreeNodes lpg  ++ prgNodes ++ [swStartNode])
    emb'' = embHW' ++ embSW' ++ softImplEdge
    embHW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub embHW
--    embHW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub prgEdges --embHW
    embSW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub embSW
    swStartNode = getSoftStartNode
    swStartNode1 = nodeDefinition swStartNode
    ((embHW, embSW), unemb) = embeddingListStep swStartNode1 prgHWedges -- prgEdges
        (isPRGNodeEmulated prgEmulatedNodes)  (([], []), lpgEdges)
    --emb = DL.nub (embHW ++ embSW)
    lpgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes
            $ DL.reverse $ topoSortEdges $ getDepEdges lpg
    prgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes
            $ DL.reverse $ topoSortEdges $ getDepEdges prg

    (prgHWedges, prgSWnodes) = removeSWnodePRG prgEdges
                                (isPRGNodeEmulated prgEmulatedNodes)

    defaultQueue = getDecNode (NB.ToQueue NB.getDefaultQueue) ""
        (BinaryNode ([], [])) []
    softImplEdge =  [((swStartNode), (defaultQueue))]


findEdgesForV :: [(Node, Node)] -> Node -> [(Node, Node)]
findEdgesForV edges toMatch = DL.filter (\ x -> (fst x) == toMatch) edges


topoSortEdges :: [(Node, Node)] -> [(Node, Node)]
topoSortEdges unSorted = sortedE
    where
    sortedE = DL.concatMap (findEdgesForV unSorted) sortedV
    sortedV = topSort unSorted


----------------------------------------------------------------
--      Testing code
----------------------------------------------------------------

testEmbeddingList :: Node -> Node -> [(Node, Node)]
--testEmbeddingList prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges lpg
--testEmbeddingList prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges prg
testEmbeddingList prg lpg = embeddingList prg lpg

testEmbeddingSet :: Node -> Node -> [(Node, String, Node)]
testEmbeddingSet prg lpg = embeddingSet prg lpg



testEmbeddingSTR :: Node -> String
testEmbeddingSTR graph = text
    where

    prgNodes = DL.nub $ nTreeNodes graph
    prgEmulatedNodes = DL.map (nodeDefinition) $
        DL.filter (not . isNodeEmbebible) prgNodes

    swStartNode = getSoftStartNode
    nodeList = DL.nub (prgNodes ++ [swStartNode])

    prgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes $
                DL.reverse $ topoSortEdges $ getDepEdges graph

    (lastNode, _) = DL.last prgEdges
--    edgesSTR = rearrangePRG prgEdges (isPRGNodeEmulated prgEmulatedNodes) lastNode
    (edgesSTR, swNodes) = removeSWnodePRG prgEdges (isPRGNodeEmulated prgEmulatedNodes)

    finalEdges = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub edgesSTR

    text = DL.concatMap showEdgeGen edgesSTR


testPRGrearrangement :: Node -> [(Node, Node)]
testPRGrearrangement prg = finalEdges
    where

    prgNodes = DL.nub $ nTreeNodes prg
    prgEmulatedNodes = DL.map (nodeDefinition) $
        DL.filter (not . isNodeEmbebible) prgNodes

    swStartNode = getSoftStartNode
    nodeList = DL.nub (prgNodes ++ [swStartNode])

    prgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes $
                DL.reverse $ topoSortEdges $ getDepEdges prg

    defaultQueue = getDecNode (NB.ToQueue NB.getDefaultQueue) ""
        (BinaryNode ([], [])) []

    lastNode = nodeDefinition defaultQueue

--    edgesSTR = rearrangePRG prgEdges (isPRGNodeEmulated prgEmulatedNodes) lastNode
    (edgesSTR, swNodes) = removeSWnodePRG prgEdges (isPRGNodeEmulated prgEmulatedNodes)
    finalEdges = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub edgesSTR


----------------------------------------------------------------

