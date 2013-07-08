#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Embedding(
    testEmbeddingV2
    , testEmbeddingSTR
    , getDepEdges
    , removeDroppedNodes
) where


import qualified Data.List as L
import qualified Data.Maybe as MB
import qualified Data.List as DL

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


convertLabelToNode :: [Node] -> String -> Node
convertLabelToNode nlist label = DL.head $
    filter (\ x -> (nodeDefinition x) == label) nlist


convertEdgeToNode :: [Node] -> (String, String) -> (Node, Node)
convertEdgeToNode nlist (a, b) = (a', b')
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

-- For given Node, find all its children
getChildren :: Node -> [Node]
getChildren root = case (getNodeEdges root) of
        (BinaryNode (as, bs))   -> DL.nub (as ++ bs)
        (NaryNode nList)        -> DL.nub $ DL.concatMap (\ (x, y) -> y) nList

-- Get outgoing edegs for all nodes underneath given Node
getOutEdges :: Node -> [(Node,Node)]
getOutEdges root = nextLevel ++ deeperLevels
    where
    children = getChildren root
    nextLevel = DL.concatMap (\ x -> [(root, x)]) children
    deeperLevels = DL.concatMap (getOutEdges) children

-- Get outgoing edegs for all nodes underneath given Node
getDepEdges :: Node -> [(Node,Node)]
getDepEdges root =  DL.map (\ (x, y) -> (y, x)) $ getOutEdges root

getSoftStartNode :: Node
getSoftStartNode = getDecNode NB.InSoftware "" (BinaryNode ([], [])) []

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


-- classify given edges based on some condition specified by function fn
classifyDeps :: (Eq a) => [(a, a)] -> (a -> Bool) -> ([(a, a)], [(a, a)])
classifyDeps graphEdges fn = (trueEdges, falseEdges)
    where
    trueEdges = DL.filter (\ (x, y) -> fn y) graphEdges
    falseEdges = DL.filter (\ (x, y) -> not $ fn y) graphEdges



----------------------------------------------------------------
--      Embedding algorithm: Set based
--      Node based version
----------------------------------------------------------------

--embeddingV3Step sP sE [] = sE
embeddingV3Step sP sE sU
    | sU == [] = sE
    | sU == sU' = error "U not shrinking"
    | otherwise = embeddingV3Step sP sE' sU'
    where
        eq = nCompPrgLpgV2
        elem' = isElemBy eq
        edgeEq (a,b) (c,d) = a `eq` c && b `eq` d
        union' = L.unionBy edgeEq

        dep n s = [x | (y,x) <- s, y `eq` n]
        sNodes s = L.nub $ map fst s
        nodes s = L.nub $ (map fst s ++ map snd s)
        isSubset :: [Node] -> [Node] -> Bool
        isSubset sA sB = all (elem' sB) sA

        sL = sE `union'` sU
        sEv = (nodes sE) ++ noDepNodes1 sL
        sUv = sNodes sU
        sPv = nodes sP

        v = head [v' | v' <- sUv, (dep v' sL) `isSubset` sEv]
        sE' = sE `union'` [(v,y) | y <- dep v sP] `union'`
                [(x,y) | (x,y) <- sU, x `eq` v && (not $ sPv `elem'` y)]
        sU' = [(x,y) | (x,y) <- sU, not $ x `eq` v]


embeddingV2Wrapper ::  Node -> Node -> [(Node, Node)]
embeddingV2Wrapper prg lpg =
    embeddingV3Step prgEdges [] lpgEdges
    where
    lpgEdges = removeDroppedNodes $ getDepEdges lpg
    prgEdges = removeDroppedNodes $ getDepEdges prg
    defaultQueue = getDecNode (NB.ToQueue NB.getDefaultQueue) ""
        (BinaryNode ([], [])) []
    softImplEdge =  [((getSoftStartNode), (defaultQueue))]

removeDroppedNodes :: [(Node, Node)] -> [(Node, Node)]
removeDroppedNodes edgeList = DL.filter (\ (x,y) ->
    not ( (nCompPrgLpg x dnode) ||  (nCompPrgLpg x dnode) ) ) edgeList
    where
    dnode = getDropNode



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
embeddingV2Step :: (Show a) => (Eq a) => a -> [(a, a)] -> (a -> Bool) ->
    (([(a, a)], [(a, a)]), [(a, a)]) ->  (([(a, a)], [(a, a)]), [(a, a)])
embeddingV2Step swStartNode prgEdges prgFn ((embedHW, embedSW), lpgUnembedded)
    | lpgUnembedded == []   = ((embedHW, embedSW), [])
    | offendingDeps /= []   = error ("few deps of next Node to embedd [ " ++
            (show vNext) ++ " ]  are not embedded ")
    | otherwise             = embeddingV2Step swStartNode prgEdges prgFn
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
--    edgesInSW = DL.filter (\ (x, y) -> x == vNext && DL.notElem y dInHW) depEdges
    edgesInSW = DL.filter (\ (x, y) -> x == vNext) depEdges

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

isPRGNodeEmulated :: (Eq a) => [a] -> a -> Bool
isPRGNodeEmulated vList v = DL.elem v vList

isNodeEmbebible :: Node -> Bool
isNodeEmbebible n
    | DL.elem (NB.InSoft True) attrs = False
    | otherwise               = True
    where
    attrs = getNodeAttributes n



-- Takes Nodes, converts them into String, applies the embedding algorithm
--      convert back the results into Node format
embeddingV3Wrapper ::  Node -> Node -> [(Node, Node)]
embeddingV3Wrapper prg lpg
    | unemb == [] = TR.trace ("\n\n\nDone with embedding "
            ++ "\nIn HW {{{\n" ++ ((DL.concatMap showEdgeGen (DL.nub embHW)))
            ++ "}}}\n\nIn SW {{{\n" ++ ((DL.concatMap showEdgeGen (DL.nub embSW)))
            ++ "}}}\n\n\n"
        ) emb''
    | otherwise = error "not all nodes embedded"
    where
    prgNodes = DL.nub $ nTreeNodes prg
    prgEmulatedNodes = DL.map (nodeDefinition) $
        DL.filter (isNodeEmbebible) prgNodes



    nodeList = DL.nub (nTreeNodes lpg  ++ prgNodes ++ [swStartNode])
    emb'' = embHW' ++ embSW' ++ softImplEdge
    embHW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub embHW
--    embHW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub prgEdges --embHW
    embSW' = DL.map (\ x -> convertEdgeToNode nodeList x) $ DL.nub embSW
    swStartNode = getSoftStartNode
    swStartNode1 = nodeDefinition swStartNode
    ((embHW, embSW), unemb) = embeddingV2Step swStartNode1 prgEdges
        (isPRGNodeEmulated prgEmulatedNodes)  (([], []), lpgEdges)
    --emb = DL.nub (embHW ++ embSW)
    lpgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes
            $ DL.reverse $ topoSortEdges $ getDepEdges lpg
    prgEdges = DL.nub $ DL.map convertEdge $ removeDroppedNodes
            $ DL.reverse $ topoSortEdges $ getDepEdges prg
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

testEmbeddingV2 :: Node -> Node -> [(Node, Node)]
--testEmbeddingV2 prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges lpg
--testEmbeddingV2 prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges prg
testEmbeddingV2 prg lpg = embeddingV3Wrapper prg lpg


testEmbeddingSTR :: Node -> String
testEmbeddingSTR graph = text
    where
    text = DL.concatMap showEdgeGen $ DL.nub $ DL.map convertEdge
        $ removeDroppedNodes $ DL.reverse $ topoSortEdges $ getDepEdges graph

----------------------------------------------------------------

