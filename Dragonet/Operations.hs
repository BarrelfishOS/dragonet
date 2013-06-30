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

data NodeEdges = BinaryNode ([Node], [Node])
        | NaryNode [[Node]]
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


data Node = Des Decision
    | Conf Configuration -- (GNode NB.ConfLabel ConfFunction) --
    | Opr Operator -- (GNode NB.OpLabel OpFunction) --
    deriving (Show, Eq)


{-
replaceNodeFromNodeList :: [Node] -> Node -> [Node] -> [Node]
replaceNodeFromNodeList inList toRemove toAdd = updatedList
    where
    updatedList = DL.mapConcat (\ x -> if x == toRemove then toAdd else [x]) inList

-- Replaces all the instances of Node with NodeList within given tree
replaceNodeFromNodeEdges :: Node -> Node -> [Node] -> Node
replaceNodeFromNodeEdges tree toRemove toAdd = tree'
    where
    tree' = case (getNodeEdges tree) of
        BinaryNode (tlist, flist) -> setNodeEdges tree (BinaryNode
            (replaceNodeFromNodeList tlist,  replaceNodeFromNodeList llist))
        NaryNode nlist -> DL.map (\ x -> replaceNodeFromNodeList x toRemove toAdd) nlist
-}


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
        NaryNode nlist ->  setNodeEdges tree (NaryNode
            (DL.map (updateNodeList fn) nlist))


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

{-
 - Apply given configuration and get the new tree where the
 - configuration node does not exist anymore.
 -}
applyConfig :: NB.NetOperation -> TagType -> Bool -> Node -> [Node]
applyConfig confOp tag whichSide tree  = tree'
    where
    tree'' = updateNodeEdges (applyConfig confOp tag whichSide) tree

    tree' = case tree of
        Conf (Configuration (GNode (NB.ConfLabel (MB.Just netop)) t alist nextNodes imp)) ->
            if ( netop == confOp ) && (tag == t)  then
                -- We found the configuration node, lets replace it
                case nextNodes of
                    BinaryNode (tlist, flist)   -> if whichSide then tlist
                                                    else flist
                    otherwise                   -> error "non binary Config node"
            else  [tree'']
        otherwise -> [tree'']

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
                (NaryNode as) -> L.nub (concat as)
        children = concat (map nTreeNodes ep)


getConfNode :: MB.Maybe NB.NetOperation -> TagType -> NodeEdges -> Node
getConfNode op tag edges = Conf $ Configuration GNode {
        gLabel = (NB.ConfLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }





getDecNode :: NB.NetOperation -> TagType -> NodeEdges -> Node
getDecNode op tag edges = Des $ Decision GNode {
        gLabel = (NB.DesLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }

getOperatorNode :: NB.NetOperator ->  TagType -> NodeEdges -> Node
getOperatorNode op tag edges = Opr $ Operator GNode {
        gLabel = (NB.OpLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }


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
testGetOperatorOp = getOperatorNode (NB.AND "testOp" ) "+" (NaryNode [])

testGetDecElem :: Node
testGetDecElem = getDecNode NB.ClassifiedL2Ethernet "test" (NaryNode [])

-- Returns the node which drops the packet
getDropNode :: Node
getDropNode = getDecNode NB.PacketDrop "Drop" (NaryNode [])


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

