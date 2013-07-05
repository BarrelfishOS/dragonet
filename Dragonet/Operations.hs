#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Operations(
    main
    , testOperation
    , DesFunction
    , Implementation(..)
    , Decision(..)
    , Configuration(..)
    , Operator(..)
    , GNode(..)
    , Node(..)
    , NodeEdges(..)
    , NodeCompare(..)
    , getDecNode
    , getOperatorNode
    , getConfNode
    , getDropNode
    , getNodeEdges
    , setNodeEdges
    , appendToTrue
    , appendToFalse
    , nTreeNodes
    , TagType
    , applyConfigWrapper
    , applyConfig
    , updateNodeList
    , updateNodeEdges
    , embeddGraphs
    , applyConfigWrapperList
    , ConfWrapperType
    , testGenConf
    , testEmbeddingV2
) where

import qualified NetBasics as NB
import qualified Data.List as L
import qualified Data.Maybe as MB

import qualified Data.List as DL
--import qualified Data.Set as Set

--import qualified Debug.Trace as TR

type TagType = String

type Packet = String -- FIXME: change this
type ConfSpace = String  -- FIXME: change this



data Implementation t f  = Implementation {
    iTag        :: t -- name for the implementation
    , iFn       :: f
}

instance Show t => Show (Implementation t f)  where
    show (Implementation tag _) = show tag

instance Eq t => Eq (Implementation t f) where
    (Implementation tag1 _) ==  (Implementation tag2 _) = tag1 == tag2


-- Decision function:  based on packet, decides which outgoing edges to choose
type DesFunction = (Decision -> Packet -> (Packet, [Node]))

-- Conf function: Based on the configuration, decides which outgoing edges to choose
type ConfFunction = (Configuration -> ConfSpace -> [Node])

-- opearator function: Based on the result of incoming edges,
--  decides which outgoing edges to choose
type OpFunction = (Operator -> [Node] -> [Node])

type PortLabel = String

--                          (trueP,  falseP)
data NodeEdges = BinaryNode ([Node], [Node])
        | NaryNode [(PortLabel, [Node])]
        deriving (Show, Eq)

data GNode l a f = GNode {
    gLabel              :: l
    , gTag              :: TagType
    , gAttributes       :: [a]
    , gEdges            :: NodeEdges
    , gImplementation   :: [Implementation TagType f]
} deriving (Show, Eq)


data Decision =  Decision (GNode NB.DesLabel NB.DesAttribute DesFunction)
    deriving (Show, Eq)
data Configuration = Configuration (GNode NB.ConfLabel NB.ConfAttribute ConfFunction)
    deriving (Show, Eq)
data Operator = Operator (GNode NB.OpLabel NB.OpAttribute OpFunction)
    deriving (Show, Eq)


class NodeCompare a where
    nCompPrgLpg :: a -> a -> Bool
    nCompPrgLpgV2 :: a -> a -> Bool
    nCompFullTag :: a -> a -> Bool



data Node = Des Decision
    | Conf Configuration -- (GNode NB.ConfLabel ConfFunction) --
    | Opr Operator -- (GNode NB.OpLabel OpFunction) --
    deriving (Show, Eq)


instance NodeCompare Node where
    nCompPrgLpg (Des (Decision n1)) (Des (Decision n2)) =
        NB.embedCompare (gLabel n1) (gLabel n2)
    nCompPrgLpg _ _ = False

    nCompPrgLpgV2 (Des (Decision n1)) (Des (Decision n2)) =
        NB.embedCompare (gLabel n1) (gLabel n2)
    nCompPrgLpgV2 (Opr (Operator n1)) (Opr (Operator n2)) =
        (gLabel n1) == (gLabel n2)
    nCompPrgLpgV2 (Conf (Configuration n1)) (Conf (Configuration n2)) =
        error ("nCompPrgLpgV2: comparing two conf node for embedding...!! \n"
            ++ "  but you are not supposed to have conf nodes in LPG!!!" )
    nCompPrgLpgV2 _ _ = False


    nCompFullTag (Des (Decision n1)) (Des (Decision n2)) =
        (gLabel n1) == (gLabel n2) &&
        (gTag n1) == (gTag n2)
    nCompFullTag (Conf (Configuration n1)) (Conf (Configuration n2)) =
        (gLabel n1) == (gLabel n2) &&
        (gTag n1) == (gTag n2)
    nCompFullTag (Opr (Operator n1)) (Opr (Operator n2)) =
        (gLabel n1) == (gLabel n2) &&
        (gTag n1) == (gTag n2)
    nCompFullTag x1 x2 = False
        -- error ("nCompFullTag: error in matching " ++ show x1 ++ " and " ++ show x2)


instance NB.EmbedCompare Node where
    embedCompare (Des (Decision n1)) (Des (Decision n2)) =
        NB.embedCompare (gLabel n1) (gLabel n2) && (gTag n1) == (gTag n2)
    embedCompare n1 n2 = error ("embedCompare: non Decision nodes are used "
            ++ "inside the embedding comparision")



getLabels :: [Node] -> ([NB.DesLabel], [NB.ConfLabel], [NB.OpLabel])
getLabels [] = ([], [], [])
getLabels (x:xs) = newTuple
    where
    xsTuples = getLabels xs
    xTuples = case x of
        Des (Decision (GNode label t _ _ _)) -> ([label], [], [])
        Conf (Configuration (GNode label t _ _ _)) -> ([], [label], [])
        Opr (Operator (GNode label t _ _ _)) -> ([], [], [label])
    (ax, bx, cx) = xTuples
    (axs, bxs, cxs) = xsTuples
    newTuple = ((ax ++ axs), (bx ++ bxs), (cx ++ cxs))

{-
 - Find all nodes which are config nodes
 - For every config node, apply the config, and get the graph
 - check the diff in the graph.
 - for all the nodes which are diff:
 -      find out which version (conf true/false) of the graph they are present
 -      mark that version as dependency
 -}


-- Test function to check the working of configuration generation
testGenConf :: Node -> String
testGenConf prg = show $ getConfLabels prg

-- get labels of all the configuration nodes
getConfLabels :: Node -> [NB.ConfLabel]
getConfLabels prg = prunedCL
    where
    prunedCL = DL.nub cl
    (_, cl, _) = getLabels $ getConfNodes prg

-- Return all the configuration nodes from given graphnode (mostly PRG)
getConfNodes :: Node -> [Node]
getConfNodes tree = DL.filter (isConfNode) $ nTreeNodes tree

-- Tells if givn node is configuration node or not
isConfNode :: Node -> Bool
isConfNode n = case n of
    Conf x  -> True
    _       -> False


-- Simple embedding algorithm, which does embedding just by looking at names
-- of the node and it does not care about dependencies.
embeddGraphs :: Node -> Node -> Node -- Node
embeddGraphs prg lpgOrig = lpg'
    where
    lpg =  getDecNode NB.InSoftware "" (BinaryNode ([lpgOrig], [])) []
    lpgForest = embeddGraphStep prg lpg
    lpg'
        | DL.length lpgForest == 1 = DL.head lpgForest
        | otherwise = error "embeddedGraphs: first node got embedded!!"


embeddGraphStep :: Node -> Node -> [Node]
embeddGraphStep prg lpg = nlist
    where
    nl = removeDesNode (isNodeEmbeddible prg) lpg
    nlist = DL.map (updateNodeEdges (embeddGraphStep prg)) nl
-- apply on all the outgoing edges of lpg




-- Decides if the given node is embeddible in given tree
isNodeEmbeddible :: Node -> Node -> Bool
isNodeEmbeddible tree node
    | isDesNodePresent tree node /= True = False
    | (NB.InSoft True) `DL.elem` desAttrs = False
--    | (NB.NeedAdaptor True) `DL.elem` desAttrs = False
--    | (NB.ResultSaved False) `DL.elem` desAttrs = False
--    | (NB.InHardware False) `DL.elem` desAttrs = False
    | otherwise = True
    where
    -- desAttrs = getDesOnlyAttributes $ prgNode
    prgNode = DL.head $ DL.filter (nCompPrgLpg node) (nTreeNodes tree)
        -- FIXME: figure out a way to find an element which matches exactly
        --      maybe you can use tags for it.  Right now, I am just taking the head
    desAttrs = getDesOnlyAttributes prgNode


isElemBy :: (Eq a) => (a -> a -> Bool) -> [a] -> a -> Bool
isElemBy fn list e
    | matched == [] = False
    | otherwise = True
    where
    matched = DL.filter (fn e) list


-- Decides if the given node is present in the given tree
isNodePresent :: Node -> Node -> Bool
isNodePresent tree node =
    isElemBy (nCompPrgLpg) (nTreeNodes tree) node


isDesNodePresent :: Node -> Node -> Bool
isDesNodePresent tree node =
    case node of
        Des (Decision d) -> isNodePresent tree node
        _ -> False



-- Removes a given node from recursive
removeDesNode :: (Node -> Bool) -> Node -> [Node]
removeDesNode checkFn tree = tree'
    where
    tree' = if checkFn tree then getNodeEdgesSide True tree
        else [updateNodeEdges (removeDesNode checkFn) tree]

{-
removeDesNodeList :: Node -> [Node] -> Node
removeDesNodeList tree nodes =
    DL.foldl (\ acc x -> removeDesNode acc x) tree nodes
-}

-- FIXME: add a dummy node before LPG to avoid returning forest instead of tree

-- apply given function on every element of the list and return resultant list
updateNodeList :: (Node -> [Node]) -> [Node] -> [Node]
updateNodeList fn nlist = DL.concatMap fn nlist

-- replace all followup nodes of given node with lists produced by given
-- function
updateNodeEdges :: (Node -> [Node]) -> Node -> Node
updateNodeEdges fn tree = tree'
    where
    tree' = case (getNodeEdges tree) of
        BinaryNode (tl, fl) -> setNodeEdges tree (BinaryNode
            ((updateNodeList fn tl),  (updateNodeList fn fl)))
        NaryNode plist ->  setNodeEdges tree (NaryNode
            (DL.map mapPort plist))
    mapPort (l,ns) = (l, (updateNodeList fn ns))

type ConfWrapperType = (NB.NetOperation, TagType, Bool)
applyConfigWrapperList :: Node -> [ConfWrapperType]  -> Node
applyConfigWrapperList tree config =
    DL.foldl (\ acc (a, b, c) -> applyConfigWrapper a b c acc) tree config

-- Wrapper around applyConfig to take care of extream condition that
-- first node itself is Configuration, and it matched with current
-- configuration change requested
applyConfigWrapper :: NB.NetOperation -> TagType -> Bool -> Node -> Node
applyConfigWrapper confOp tag whichSide tree
    | DL.length expanded == 1 = DL.head expanded
    | expanded == [] = error ("Something went terribaly wrong somewhere,"
            ++ " causing expanded list to be empty")
    | otherwise  = error ("First node itself matched with given config."
            ++ " Add a dummy node before it to simply your life")
    where
    expanded = applyConfig confOp tag whichSide tree


-- Finds and replace a Decision node following a configuration node
-- which has same NetOperation label as configuration node
-- and replaces it with new NetOperation label which we got as part
-- of the configuration
findAndReplaceWithinDes :: NB.NetOperation -> [Node] -> [Node]
findAndReplaceWithinDes _ [] = []
findAndReplaceWithinDes confOp (x:xs)  = case x of
    Des (Decision (GNode (NB.DesLabel no) t alist nextNodes imp)) ->
        if (NB.confCompare
            (NB.ConfLabel (MB.Just no))
            (NB.ConfLabel (MB.Just confOp))
           ) then  [(Des (Decision (GNode (NB.DesLabel confOp) t alist nextNodes imp)))]
           ++ findAndReplaceWithinDes confOp xs
        else
            [x] ++ findAndReplaceWithinDes confOp xs
    _ ->  [x] ++ findAndReplaceWithinDes confOp  xs


{-
 - Apply given configuration and get the new tree where the
 - configuration node does not exist anymore.
 -}
applyConfig :: NB.NetOperation -> TagType -> Bool -> Node -> [Node]
applyConfig confOp tag whichSide tree  = tree'
    where
    tree'' = updateNodeEdges (applyConfig confOp tag whichSide) tree

    tree' = case tree of
        Conf (Configuration (GNode confl  t alist nextNodes imp)) ->
            if ( NB.confCompare confl (NB.ConfLabel (MB.Just confOp)) )
                && (tag == t)  then
                -- (NB.ConfLabel (MB.Just netop))
                -- We found the configuration node, lets replace it
                case nextNodes of
                    BinaryNode (tlist, flist)   -> if whichSide then
                                        findAndReplaceWithinDes confOp tlist
                                                    else flist
                    _ -> error "non binary Config node"
            else  [tree'']
        _ -> [tree'']

-- Get list containing all nodes reachable from the specified start node.
-- Note that nodes with multiple incoming edges might be contained more than
-- once in the resulting list.
nTreeNodes :: Node -> [Node]
nTreeNodes n =
    n:children
    where
        ep =
            case (getNodeEdges n) of
                (BinaryNode (as, bs)) -> L.nub (as ++ bs)
                (NaryNode as) -> L.nub (concat $ map snd as)
        children = concat (map nTreeNodes ep)


getConfNode :: MB.Maybe NB.NetOperation -> TagType -> NodeEdges -> Node
getConfNode op tag edges = Conf $ Configuration GNode {
        gLabel = (NB.ConfLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }





getDecNode :: NB.NetOperation -> TagType -> NodeEdges -> [NB.DesAttribute]
        -> Node
getDecNode op tag edges attrs = Des $ Decision GNode {
        gLabel = (NB.DesLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = attrs
        , gImplementation = []
    }

getOperatorNode :: NB.NetOperator -> String -> TagType -> NodeEdges -> Node
getOperatorNode op label tag edges = Opr $ Operator GNode {
        gLabel = (NB.OpLabel op label)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }


getNodeEdgesSide :: Bool -> Node -> [Node]
getNodeEdgesSide side node = case getNodeEdges node of
    (BinaryNode (tSide, lSide)) -> if side then tSide else lSide
    _ -> error ("getNodeEdgesSide: requesting side on Nary node"
            ++ show node)


toBaseAttrDes :: NB.DesAttribute -> NB.Attribute
toBaseAttrDes (NB.DesAttribute x) = x

toBaseAttrConf :: NB.ConfAttribute -> NB.Attribute
toBaseAttrConf (NB.ConfAttribute x) = x

toBaseAttrOpr :: NB.OpAttribute -> NB.Attribute
toBaseAttrOpr (NB.OpAttribute x) = x


getNodeAttributes :: Node -> [NB.Attribute]
getNodeAttributes node = case node of
    Des (Decision (GNode _ _ a _  _)) -> DL.map (toBaseAttrDes) a
--    Des (Decision (GNode _ _ a _  _)) -> DL.map (NB.DesAttribute) a
    Conf (Configuration (GNode _ _ a _  _)) -> DL.map (toBaseAttrConf) a
    Opr (Operator (GNode _ _ a _  _)) -> DL.map (toBaseAttrOpr) a

getDesOnlyAttributes :: Node -> [NB.Attribute]
getDesOnlyAttributes node = case node of
    Des (Decision (GNode _ _ a _  _)) -> DL.map (toBaseAttrDes) a
    _                                                   -> error
        "non Decision node was given to get decision attributes"


getNodeEdges :: Node -> NodeEdges
getNodeEdges node = case node of
    Des (Decision a)          -> gEdges a
    Conf (Configuration a)    -> gEdges a
    Opr (Operator a)          -> gEdges a

setNodeEdges :: Node -> NodeEdges -> Node
setNodeEdges node newEdges = case node of
   Des (Decision a)           -> Des (Decision a { gEdges = newEdges })
   Conf (Configuration a)     -> Conf (Configuration a { gEdges = newEdges })
   Opr (Operator a)           -> Opr (Operator a { gEdges = newEdges })

appendToTrue :: Node -> Node -> Node
appendToTrue orig toAdd = case getNodeEdges orig of
    BinaryNode (tl, fl) -> setNodeEdges orig $ BinaryNode (tl ++ [toAdd], fl)
    NaryNode _          -> error "assumption about node being Binary is wrong."

appendToFalse :: Node -> Node -> Node
appendToFalse orig toAdd = case getNodeEdges orig of
    BinaryNode (tl, fl) -> setNodeEdges orig $ BinaryNode (tl, fl ++ [toAdd] )
    NaryNode _          -> error "assumption about node being Binary is wrong."

{-
compareNodes :: Node -> Node -> Bool
compareNodes Des (Decision (l1 _ _ _ _)) Des (Decision (l2 _ _ _ _))  = l1 == l2

insertAfterBN :: Node -> Node -> Node -> Node
insertNodeInBinaryNode big parent toAdd = big'
    where
    -- FIXME: complete this implementation
    big = case compareNodes big parent
-}

testGetOperatorOp :: Node
testGetOperatorOp = getOperatorNode NB.AND "testOp" "+" (NaryNode [])

testGetDecElem :: Node
testGetDecElem = getDecNode NB.ClassifiedL2Ethernet "test" (NaryNode []) []

-- Returns the node which drops the packet
getDropNode :: Node
getDropNode = getDecNode NB.PacketDrop "Drop" (BinaryNode ([],[])) []


testGetConfElem :: Node
testGetConfElem = getConfNode (MB.Just NB.L2EtherValidCRC) "checkIt" (BinaryNode ([], []))

testOperation :: [Node]
testOperation = [a, b, c]
    where
    a = testGetConfElem
    b = testGetDecElem
    c = testGetOperatorOp

main :: IO()
main = putStrLn "Hello world"


-- ################## copied from dsltest/implementation.hs

{-
-- intermediate function to help getPredDS
getPredDSStep :: Node -> Node -> Node -> [Node]
getPredDSStep parent currNode toMatch
    | nCompFullTag currNode toMatch     = [parent] ++ nextLevel
    | otherwise                         = nextLevel
    where
    children = case (getNodeEdges currNode) of
        (BinaryNode (as, bs))   -> DL.nub (as ++ bs)
        (NaryNode as)           -> DL.nub (concat as)
    nextLevel = DL.concatMap (\ x -> getPredDSStep currNode x toMatch) children


-- Get all predesessor nodes for given node
getPredDS :: Node -> Node -> [Node]
getPredDS currNode toMatch
    | nCompFullTag currNode toMatch     = []
    | otherwise                         = nextLevel
    where
    children = case (getNodeEdges currNode) of
        (BinaryNode (as, bs))   -> DL.nub (as ++ bs)
        (NaryNode as)           -> DL.nub (concat as)
    nextLevel = DL.concatMap (\ x -> getPredDSStep currNode x toMatch) children
-}
-- For given node, find all its children
getChildren :: Node -> [Node]
getChildren root = case (getNodeEdges root) of
        (BinaryNode (as, bs))   -> DL.nub (as ++ bs)
        (NaryNode nList)        -> DL.nub $ DL.concatMap (\ (x, y) -> y) nList

-- Get outgoing edegs for all nodes underneath given node
getOutEdges :: Node -> [(Node,Node)]
getOutEdges root = nextLevel ++ deeperLevels
    where
    children = getChildren root
    nextLevel = DL.concatMap (\ x -> [(root, x)]) children
    deeperLevels = DL.concatMap (getOutEdges) children

-- Get outgoing edegs for all nodes underneath given node
getDepEdges :: Node -> [(Node,Node)]
getDepEdges root =  DL.map (\ (x, y) -> (y, x)) $ getOutEdges root



-- Find predecessor for specfied node in edge list
getPredecessors :: Eq a => a -> [(a,a)] -> [a]
getPredecessors n e =
    L.nub $ map fst $ filter (\(_,x) -> x == n) e


-- Topological sort on edge list representation
topSort :: Eq a => [(a,a)] -> [a]
topSort [] = []
topSort es =
    noIncoming ++ orphaned ++ (topSort newEdges)
    where
        -- Is n a successor of another node?
        notSucc n = MB.isNothing $ L.find (\x -> (snd x) == n) es
        -- All nodes without incoming edges
        noIncoming = filter (notSucc) $ L.nub $ map fst es
        -- edges that don't start at noIncoming nodes
        newEdges = filter (\x -> notElem (fst x) noIncoming) es
        -- edges that start at noIncoming nodes
        dropped = filter (\x -> elem (fst x) noIncoming) es
        -- Sink nodes (without outgoing edges) that lost their incoming edges
        -- those also vanish from the edges list
        isOrphaned n = MB.isNothing $ L.find (\x -> ((snd x) == n) || ((fst x) == n)) newEdges
        orphaned = L.nub $ filter isOrphaned $ map snd dropped


testEmbeddingV2 :: Node -> Node -> [(Node, Node)]
--testEmbeddingV2 prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges lpg
--testEmbeddingV2 prg lpg = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges prg
testEmbeddingV2 prg lpg = embeddingV2Wrapper prg lpg

-- few deps of next node to embedd [ L2EtherValidType ]  are not embedded "ClassifiedL2Ethernet"

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


-- Find all nodes which are sucessor of given node for given list of edges
-- I am using to find all previous dependencies for given node
getAllSucc :: [(Node, Node)] -> Node -> [Node]
getAllSucc depList node = DL.concatMap (\ (x, y) -> if x == node then [y] else [] ) depList

-- Get all nodes which don't have any dependencies
noDepNodes :: [(Node, Node)] -> [Node]
noDepNodes depList = nonDep
    where
    srcNodes = DL.nub $ DL.map (fst) depList
    dstNodes = DL.nub $ DL.map (snd) depList
    nonDep = DL.filter (\ x -> x `DL.notElem` srcNodes ) dstNodes


-- find out all the parent nodes for given node
getParentNodes :: [(Node, Node)] -> Node -> [Node]
getParentNodes edgeList v = DL.concatMap (\ (x, y) -> if v == y then [x] else [] ) edgeList


-- try and find given node into the PRG
-- Note: Remember that the node is from LPG whereas we are searching it in PRG
locateInPRG ::  [(Node, Node)] -> Node -> [Node]
locateInPRG prgEdges gn = matchingPRGnodes
    where
    matchingPRGnodes = DL.map (fst) $ locateEdgesInPRG prgEdges gn
--    prgNodes = DL.nub $ DL.map (\ (x, y) -> x) prgEdges
--    matchingPRGnodes = DL.filter (\ x -> nCompPrgLpgV2 x gn) prgNodes

locateEdgesInPRG ::  [(Node, Node)] -> Node -> [(Node, Node)]
locateEdgesInPRG prgEdges gn = matchingPRGEdges
    where
    matchingPRGEdges = DL.filter (\ (x, y) -> nCompPrgLpgV2 x gn) prgEdges

locateEdgesInLPG ::  [(Node, Node)] -> Node -> [(Node, Node)]
locateEdgesInLPG lpgEdges gn = matchingLPGEdges
    where
    matchingLPGEdges = DL.filter (\ (x, y) -> nCompFullTag x gn) lpgEdges


getSoftStartNode :: Node
getSoftStartNode = getDecNode NB.InSoftware "" (BinaryNode ([], [])) []


-- This node can ideally go in hardware, so try to do so
addHWnode :: [(Node, Node)] -> ([(Node, Node)], [(Node, Node)]) -> Node
    -> [(Node, Node)]
addHWnode prgEdges (lpgEmbedded, lpgUnembedded) gn = gnEdges
    where
    allLPGEdges = lpgEmbedded ++ lpgUnembedded
    embeddedNodes = DL.map (fst) lpgEmbedded
    parentsInLPG = getParentNodes allLPGEdges gn

    -- find parents which are embedded in hardware
    parentsInHW = DL.filter (\ x -> locateInPRG prgEdges x /= []) parentsInLPG
    parentsInSW = DL.filter (\ x -> locateInPRG prgEdges x == []) parentsInLPG
    -- All parents in HW : it goes in H/W
    gnEdges
        | parentsInSW == [] = hwDepEdges
        | parentsInHW == [] = swDepEdges -- All parents in SW
        | otherwise = hwDepEdges ++ swDepEdges ++ explicitSWDep
            -- keep HW deps, SW deps, and add explicit dep marking it in SW
            -- Also, this implies that this is an operator node
        where
        hwDepEdges = locateEdgesInPRG prgEdges gn
        --hwDepEdges = []
        swDepEdges = locateEdgesInLPG allLPGEdges gn
        explicitSWDep = [(gn, getSoftStartNode)]


    -- If all the parents in hardware, then this goes in hardware


embeddingV2Step :: [(Node, Node)] -> ([(Node, Node)], [(Node, Node)]) ->
    ([(Node, Node)], [(Node, Node)])
embeddingV2Step prgEdges (lpgEmbedded, lpgUnembedded)
    | lpgUnembedded == []   = (lpgEmbedded, [])
    | offendingSucc /= []   = error ("few deps of next node to embedd [ " ++
            (nodeDefinition nextV)
            ++ " ]  are not embedded "
            ++ (show $ (DL.concatMap (nodeDefinition) offendingSucc)))
    | otherwise             = embeddingV2Step prgEdges (lpgEmbedded', lpgUnembedded')
    where
    allLPGEdges = lpgUnembedded ++ lpgEmbedded
    nextV = fst $ DL.head lpgUnembedded
    -- make sure that all deps of nextV are in the embedded part
    embeddedV = (noDepNodes allLPGEdges) ++ (DL.map (fst) lpgEmbedded)

    offendingSucc = DL.filter (\ x -> x `DL.notElem` embeddedV) $
            getAllSucc allLPGEdges nextV

    -- check if this node is in hardware
    matchingPRGnodes = locateInPRG prgEdges nextV

    newEdges = addHWnode prgEdges (lpgEmbedded, lpgUnembedded) nextV

    {-
    newEdges = if matchingPRGnodes == [] then [] -- in software
        else addHWnode prgEdges (lpgEmbedded, lpgUnembedded) nextV  -- in hardware
    -}

    -- Move the node from unembedded to embedded.
    -- This implies that we move all the edges associated with it.
    lpgUnembedded' = DL.filter (\ (x, y) -> x /= nextV) lpgUnembedded
    lpgEmbedded' =  lpgEmbedded ++ newEdges


embeddingV2Wrapper ::  Node -> Node -> [(Node, Node)]
embeddingV2Wrapper prg lpg
    | unEmbedded == []  = embedded ++ softImplEdge
    | otherwise         = error ("Could not embedd all the nodes")
    where
    lpgEdges = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges lpg
    prgEdges = DL.reverse $ topoSortEdges $ DL.nub $ getDepEdges prg
    defaultQueue = getDecNode (NB.ToQueue NB.getDefaultQueue) ""
        (BinaryNode ([], [])) []
    softImplEdge =  [((getSoftStartNode), (defaultQueue))]
    (embedded, unEmbedded) = embeddingV2Step prgEdges ([], (lpgEdges))


findEdgesForV :: [(Node, Node)] -> Node -> [(Node, Node)]
findEdgesForV edges toMatch = DL.filter (\ x -> (fst x) == toMatch) edges


topoSortEdges :: [(Node, Node)] -> [(Node, Node)]
topoSortEdges unSorted = sortedE
    where
    sortedE = DL.concatMap (findEdgesForV unSorted) sortedV
    sortedV = topSort unSorted



