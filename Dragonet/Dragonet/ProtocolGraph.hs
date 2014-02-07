module Dragonet.ProtocolGraph(
    Label,
    Tag,
    Attribute,
    Port,

    ConfFunction,
    ConfMonad,
    ConfSpace,

    Operator(..),
    Personality(..),
    GraphType(..),
    Node(..),

    nIsFNode,
    nIsONode,

    nIsCNode,
    nConfFun,

    nIsSoftware,

    opToString,

    PGraph,
    PGNode,
    PGEdge,
    PGContext,

    pgSetType,

    baseFNode,
    baseONode,
    baseCNode
) where

import qualified Data.Graph.Inductive as DGI
import qualified Control.Monad.State as ST

import Dragonet.Implementation (Implementation)

type Label = String
type Tag = String
type Attribute = String
type Port = String

data Operator = OpAnd | OpOr | OpNAnd | OpNOr
    deriving (Show, Eq)

data Personality = 
    CNode ConfFunction |
    ONode Operator | 
    FNode

instance Eq Personality where
    CNode _ == CNode _ = True
    ONode oa == ONode ob = oa == ob
    FNode == FNode = True
    _ == _ = False

instance Show Personality where
    show (CNode _) = "CNode"
    show (ONode op) = "ONode " ++ show op
    show (FNode) = "FNode"

data GraphType = GTUnknown | GTPrg | GTLpg
    deriving (Show, Eq)

data Node = Node {
    nLabel          :: Label,
    nTag            :: Tag,
    nPersonality    :: Personality,
    nGraphType      :: GraphType,
    nAttributes     :: [Attribute],
    nPorts          :: [Port],
    nImplementation :: Maybe Implementation
}-- deriving (Show)

instance Show (Node) where
    show n = pref ++ nLabel n ++ "[" ++ nTag n ++ "]"
        where
            pref = case nGraphType n of
                GTLpg -> "L:"
                GTPrg -> "P:"
                _ -> ""

instance Eq (Node) where
    a == b =
        (nLabel a == nLabel b)
        && (nTag a == nTag b)
        && (nGraphType a == nGraphType b)
        && (nAttributes a == nAttributes b)
        && (nPorts a == nPorts b)


type PGraph = DGI.Gr Node Port
type PGNode = DGI.LNode Node
type PGEdge = DGI.LEdge Port
type PGContext = DGI.Context Node Port




-------------------------------------------------------------------------------
-- Configuration
type ConfSpace = String
type ConfMonad a = ST.State (Int,[PGNode]) a 
type ConfFunction =
    Node ->
    [(DGI.LNode (Node), Port)] ->
    [(DGI.LNode (Node), Port)] ->
    ConfSpace ->
    ConfMonad [DGI.LEdge Port]


-- Get configuration function from node (assumes node is CNode)
nConfFun :: Node -> ConfFunction
nConfFun n = fun
    where (CNode fun) = nPersonality n



-------------------------------------------------------------------------------
-- Node functions

baseFNode :: Label -> [Attribute] -> [Port] -> Maybe Implementation -> Node
baseFNode label attr ports impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = FNode,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }

baseONode :: Label -> [Attribute] -> [Port] -> Operator -> Maybe Implementation -> Node
baseONode label attr ports op impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = ONode op,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }

baseCNode :: Label -> [Attribute] -> [Port] -> ConfFunction -> Maybe Implementation
                -> Node
baseCNode label attr ports cnf impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = CNode cnf,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }


nIsCNode :: Node -> Bool
nIsCNode n = persIsCNode $ nPersonality n

nIsFNode :: Node -> Bool
nIsFNode n = persIsFNode $ nPersonality n

nIsONode :: Node -> Bool
nIsONode n = persIsONode $ nPersonality n

-- Returns true iff node is a LPG node, or a PRG node with the software
-- attribute
nIsSoftware :: Node -> Bool
nIsSoftware n
    | elem "software" $ nAttributes n = True
    | otherwise = nGraphType n == GTLpg



   
-------------------------------------------------------------------------------
-- Protocol graph functions

pgSetType :: GraphType -> PGraph -> PGraph
pgSetType t = DGI.nmap (\n -> n { nGraphType = t })


-------------------------------------------------------------------------------
-- Misc functions


persIsCNode :: Personality -> Bool
persIsCNode (CNode _) = True
persIsCNode _ = False

persIsONode :: Personality -> Bool
persIsONode (ONode _) = True
persIsONode _ = False

persIsFNode :: Personality -> Bool
persIsFNode (FNode) = True
persIsFNode _ = False


opToString :: Operator -> String
opToString OpAnd = "AND"
opToString OpNAnd = "NAND"
opToString OpOr = "OR"
opToString OpNOr = "NOR"


