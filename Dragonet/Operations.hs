#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Operations(
    main
    , testOperation
    , DesFunction
    , Implementation(..)
    , Decision(..)
    , GNode(..)
    , Node(..)
    , NodeEdges(..)
    , getDecNode
    , getOperatorNode
    , getDropNode
) where

import qualified NetBasics as NB


--import qualified Data.List as DL
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

getConfNode :: String -> TagType -> NodeEdges -> Node
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

getOperatorNode :: NB.NetOperator -> TagType -> NodeEdges -> Node
getOperatorNode op tag edges = Opr $ Operator GNode {
        gLabel = (NB.OpLabel op)
        , gTag = tag
        , gEdges = edges
        , gAttributes = []
        , gImplementation = []
    }

testGetOperatorOp :: Node
testGetOperatorOp = getOperatorNode NB.AND "+" (NaryNode [])

testGetDecElem :: Node
testGetDecElem = getDecNode NB.ClassifiedL2Ethernet "test" (NaryNode [])

-- Returns the node which drops the packet
getDropNode :: Node
getDropNode = getDecNode NB.PacketDrop "Drop" (NaryNode [])


testGetConfElem :: Node
testGetConfElem = getConfNode "checksumConf" "checkIt" (BinaryNode ([], []))

testOperation :: [Node]
testOperation = [a, b, c]
    where
    a = testGetConfElem
    b = testGetDecElem
    c = testGetOperatorOp

main :: IO()
main = putStrLn "Hello world"

