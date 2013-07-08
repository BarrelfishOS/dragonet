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
    , applyConfigWrapperList
    , ConfWrapperType
    , getDesOnlyAttributes
    , getNodeAttributes

) where

import qualified NetBasics as NB
import qualified Data.List as L
import qualified Data.Maybe as MB

import qualified Data.List as DL
--import qualified Data.Set as Set

import qualified Debug.Trace as TR

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

getOperatorNode :: NB.NetOperator -> NB.NetOperation -> TagType
    -> NodeEdges -> Node
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


testGetOperatorOp :: Node
testGetOperatorOp = getOperatorNode NB.AND NB.ClassifiedL2Ethernet "+" (BinaryNode ([], []))

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



