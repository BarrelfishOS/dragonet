#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Operations(
    main
    , testOperation
    , DesFunction
    , Attribute(..)
    , Implementation(..)
    , Decision(..)
    , Node(..)
) where

import qualified NetBasics as NB

import qualified Data.Data as DD

--import qualified Data.List as DL
--import qualified Data.Set as Set

--import qualified Debug.Trace as TR

type TagType = String

type Packet = String -- FIXME: change this
type ConfSpace = String  -- FIXME: change this



data Attribute = Security Bool
    | InHardware Bool
    deriving (Show, Eq, Ord, DD.Typeable, DD.Data)

data Implementation t f  = Implementation {
    iTag        :: t -- name for the implementation
    , iFn       :: f
}

instance Show t => Show (Implementation t f)  where
    show (Implementation tag _) = show tag

instance Eq t => Eq (Implementation t f) where
    (Implementation tag1 _) ==  (Implementation tag2 _) = tag1 == tag2


-- Decision function:  based on packet, decides which outgoing edges to choose
type DesFunction = (Decision -- GNode NB.DesLabel DesFunction
        -> Packet -> (Packet, [Node]))

-- Conf function: Based on the configuration, decides which outgoing edges to choose
type ConfFunction = (Configuration --  GNode NB.ConfLabel ConfFunction
        -> ConfSpace -> [Node])

-- opearator function: Based on the result of incoming edges,
--  decides which outgoing edges to choose
type OpFunction = (Operator -- GNode NB.OpLabel OpFunction
        -> [Node] -> [Node]) -- Need better names

data GNode l f = GNode {
    gLabel              :: l
    , gTag               :: TagType
    , gEdges            :: [Node]
    , gImplementation   :: [Implementation TagType f]
} deriving (Show, Eq)


data Decision =  Decision (GNode NB.DesLabel DesFunction)
    deriving (Show, Eq)
data Configuration = Configuration (GNode NB.ConfLabel ConfFunction)
    deriving (Show, Eq)
data Operator = Operator (GNode NB.OpLabel OpFunction)
    deriving (Show, Eq)


data Node = Des Decision
    | Conf Configuration -- (GNode NB.ConfLabel ConfFunction) --
    | Opr Operator -- (GNode NB.OpLabel OpFunction) --
    deriving (Show, Eq)

testGetConfElem :: Node
testGetConfElem = Conf $ Configuration GNode {
        gLabel = (NB.ConfLabel "checksumConf")
        , gTag = "tt"
        , gEdges = []
        , gImplementation = []
    }

testGetDecElem :: Node
testGetDecElem = Des $ Decision GNode {
        gLabel = (NB.DesLabel "checksum")
        , gTag = "tt"
        , gEdges = []
        , gImplementation = []
    }

testGetOperatorOp :: Node
testGetOperatorOp = Opr $ Operator GNode {
        gLabel = (NB.OpLabel "AND")
        , gTag = "++"
        , gEdges = []
        , gImplementation = []
    }

testOperation :: [Node]
testOperation = [a, b, c]
    where
    a = testGetConfElem
    b = testGetDecElem
    c = testGetOperatorOp

main :: IO()
main = putStrLn "Hello world"

