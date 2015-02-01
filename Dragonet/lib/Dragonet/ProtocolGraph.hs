module Dragonet.ProtocolGraph (
    Node(..),
    Edge(..),

    PGraph(..),
    PGNode,
    PGEdge,
    PGContext,
    PGAdj,
    PGDecomp,
    PGGDecomp,

    NLabel,
    NTag,
    NAttribute(..),
    NPort,
    NImplementation(..),
    NOperator(..),
    NSpawnHandle,

    ESAttribute(..),

    ConfMonad,
    ConfState(..),
    initConfState,
    ConfFunction,
    ConfType(..),
    ConfValue(..),
    csFold,

    baseFNode,
    baseONode,
    baseCNode,

    nAttrAdd,
    nAttrsAdd,
    nAttrElem,

    opShortCircuitInOut,
    opSingleVal,
) where

import Dragonet.Semantics (PortSemantics)
import Dragonet.Predicate.Definitions (PredExpr(..))

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
        nSemantics      :: [(NPort, PortSemantics)],
        nPredicates     :: [(NPort, PredExpr)],
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
        nLabel            :: NLabel,
        nTag              :: NTag,
        nAttributes       :: [NAttribute],
        nPorts            :: [NPort],
        nConfType         :: ConfType,
        nConfFunction     :: ConfFunction,
        nIncrConfFunction :: ConfFunction, -- incremental configuration function
        nIncrCounter      :: Int -- for removing incrementally configured C-nodes
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
type PGContext  = DGI.Context Node Edge
type PGMContext = DGI.MContext Node Edge
type PGAdj      = DGI.Adj Edge
type PGDecomp   = (PGMContext, PGraph)
type PGGDecomp  = (PGContext, PGraph)


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

-- TODO: should we add delete here?
-- NB: new edges are returned and not carried in the state, but we might want to
-- put them here
data ConfState = ConfState {
       csLastNid :: DGI.Node
     -- nodes/edges added
     , csNewNodes :: [PGNode]
}

initConfState maxNid = ConfState {
      csLastNid = maxNid
    , csNewNodes = []
}

-- this is stupid, TODO: use foldM
csFold :: ConfState -> ConfState -> ConfState
csFold cs1 cs2 = ConfState {
      csLastNid  = csLastNid cs2
    , csNewNodes = (csNewNodes cs2) ++ (csNewNodes cs1)
}

-- next id and set of new nodes
type ConfMonad a = ST.State ConfState a

-- NB: The graph argument in the configuration function should *not* be
-- generally used. Instead, functions should only consider the in/out edges.
-- Note that normal configuration functions only add nodes/edges (never delete
-- them).
-- The graph argument is used in some special cases (queue configuration
-- functions).
type ConfFunction =
    PGraph ->               -- graph
    PGNode ->               -- configuration node
    [(PGNode, Edge)] ->     -- configuration node in edges
    [(PGNode, Edge)] ->     -- configuration node out edges
    ConfValue ->            -- configuration value
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
        nConfFunction   = cFun,
        nIncrConfFunction = error $ "Incremental conf function Not Implemented! for node:" ++ label,
        nIncrCounter = error $ "Incremental conf counter Not Implemented! for node:" ++ label
        }

nAttrAdd :: NAttribute -> Node -> Node
nAttrAdd a n = n { nAttributes = nAttributes n ++ [a] }

nAttrsAdd :: [NAttribute] -> Node -> Node
nAttrsAdd a n = n { nAttributes = nAttributes n ++ a }

nAttrElem :: NAttribute -> Node -> Bool
nAttrElem a n = elem a $ nAttributes n


-- Operator nodes can be short-circuted -> Just (in,out)
-- in:  value that short-circuts the operator
-- out: result when the short-circuted value is applied
-- If all inputs are (NOT in) then the output is (NOT out)
opShortCircuitInOut :: NOperator -> Maybe (Bool, Bool)
opShortCircuitInOut NOpAnd   = Just (False, False)
opShortCircuitInOut NOpOr    = Just (True, True)
opShortCircuitInOut NOpNAnd  = Just (False, True)
opShortCircuitInOut NOpNOr   = Just (True, False)

opSingleVal :: NOperator -> Bool -> Bool
opSingleVal op val = if val == inV then outV else (not outV)
    where Just (inV, outV) = opShortCircuitInOut op
