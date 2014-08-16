module Dragonet.ProtocolGraph (
    Node(..),
    Edge(..),

    PGraph(..),
    PGNode,
    PGEdge,
    PGContext,

    NLabel,
    NTag,
    NAttribute(..),
    NPort,
    NImplementation(..),
    NOperator(..),
    NSpawnHandle,

    ESAttribute(..),

    ConfMonad,
    ConfFunction,
    ConfType(..),
    ConfValue(..),

    baseFNode,
    baseONode,
    baseCNode,

    nAttrAdd,
    nAttrsAdd,
    nAttrElem
) where

import Dragonet.Semantics (PortSemantics)

import qualified Data.Graph.Inductive as DGI
import qualified Control.Monad.State as ST

import Text.Show.Functions -- show instance for functions, so that ConfFunction
                           -- gets Show and we can derive Show for Node


-------------------------------------------------------------------------------
-- Basic Types

-- Graph node label
data Node =
    FNode {
        nLabel          :: NLabel,
        nTag            :: NTag,
        nAttributes     :: [NAttribute],
        nPorts          :: [NPort],
        nImplementation :: NImplementation,
        nSemantics      :: [(NPort,PortSemantics)],
        nPredicates     :: [(NPort, String)],
        nOrigin         :: String  -- LPG, PRG, etc (mostly for debugging)
        } |
    ONode {
        nLabel          :: NLabel,
        nTag            :: NTag,
        nAttributes     :: [NAttribute],
        nPorts          :: [NPort],
        nOperator       :: NOperator
        } |
    CNode {
        nLabel          :: NLabel,
        nTag            :: NTag,
        nAttributes     :: [NAttribute],
        nPorts          :: [NPort],
        nConfType       :: ConfType,
        nConfFunction   :: ConfFunction
        }
    deriving (Show)

-- Graph edge label
data Edge =
    Edge {
        ePort :: NPort
        } |
    ESpawn {
        eIdentifier :: NSpawnHandle,
        eAttributes :: [ESAttribute]
        }
    deriving (Show)

instance Ord Edge where
    (Edge p1) `compare` (Edge p2)         = p1 `compare` p2
    (ESpawn x1 _) `compare` (ESpawn x2 _) = x1 `compare` x2

instance Eq Edge where
    (Edge p1) == (Edge p2)         = p1 == p2
    (ESpawn x1 _) == (ESpawn x2 _) = x1 == x2
    (ESpawn _ _)  == (Edge _)      = False
    (Edge _)      == (ESpawn _ _)  = False

-- Graph representation
type PGraph = DGI.Gr Node Edge
type PGNode = DGI.LNode Node
type PGEdge = DGI.LEdge Edge
type PGContext = DGI.Context Node Edge


-------------------------------------------------------------------------------
-- Helper Types

type NLabel = String
type NTag   = String
type NPort  = String

data NAttribute =
    NAttrSoftware |
    NAttrCustom String
    deriving (Eq,Ord,Show)

-- spawn edge attribute
data ESAttribute =
    ESAttrPredicate String -- predicate for incoming edge
    deriving (Show)


data NImplementation =
    NImplFunction String
    deriving (Eq,Ord,Show)

data NOperator =
    NOpAnd |
    NOpOr |
    NOpNAnd |
    NOpNOr
    deriving (Eq,Ord,Show)

type NSpawnHandle = String


-------------------------------------------------------------------------------
-- Configuration Types

type ConfMonad a = ST.State (Int,[PGNode]) a

type ConfFunction =
    Node ->
    [(PGNode, Edge)] ->
    [(PGNode, Edge)] ->
    ConfValue ->
    ConfMonad [PGEdge]

data ConfType =
    CTInteger {
        ctMin :: Integer,
        ctMax :: Integer
      } |
    CTBool {
      } |
    CTMaybe {
        ctElement :: ConfType
      } |
    CTList {
        ctElement :: ConfType,
        ctOrdered :: Bool,
        ctLenMin  :: Maybe Integer,
        ctLenMax  :: Maybe Integer
      } |
    CTTuple {
        ctElements :: [(String, ConfType)]
      } |
    CTEnum {
        ctEnumerators :: [String]
      } |
    CTSum {
        ctElements :: [(String, ConfType)]
      }
    deriving (Eq, Ord, Show)

data ConfValue =
    CVInt Integer |
    CVBool Bool |
    CVMaybe (Maybe ConfValue) |
    CVList [ConfValue] |
    CVTuple [ConfValue] |
    CVEnum Int |
    CVTag Int ConfValue
    deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- Helper Functions

baseFNode :: NLabel -> [NPort] -> Node
baseFNode label ports =
    FNode {
        nLabel          = label,
        nTag            = "",
        nAttributes     = [],
        nPorts          = ports,
        nImplementation = NImplFunction label,
        nSemantics      = [],
        nPredicates     = [],
        nOrigin         = ""
        }

baseONode :: NLabel -> [NPort] -> NOperator -> Node
baseONode label ports operator =
    ONode {
        nLabel          = label,
        nTag            = "",
        nAttributes     = [],
        nPorts          = ports,
        nOperator       = operator
        }

baseCNode :: NLabel -> [NPort] -> ConfType -> ConfFunction -> Node
baseCNode label ports cType cFun =
    CNode {
        nLabel          = label,
        nTag            = "",
        nAttributes     = [],
        nPorts          = ports,
        nConfType       = cType,
        nConfFunction   = cFun
        }

nAttrAdd :: NAttribute -> Node -> Node
nAttrAdd a n = n { nAttributes = nAttributes n ++ [a] }

nAttrsAdd :: [NAttribute] -> Node -> Node
nAttrsAdd a n = n { nAttributes = nAttributes n ++ a }

nAttrElem :: NAttribute -> Node -> Bool
nAttrElem a n = elem a $ nAttributes n

