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

    Node(..),

    nIsFNode,
    nIsONode,

    nIsCNode,
    nConfFun,

    opToString,

    PGraph,
    PGNode,
    PGEdge,
    PGContext,

    baseFNode,
    baseONode,
    baseCNode
) where

import qualified Data.Graph.Inductive as DGI
import qualified Control.Monad.State as ST

type Label = String
type Tag = String
type Attribute = String
type Port = String


data Operator = OpAnd | OpOr
    deriving (Show, Eq)

data Personality i = 
    CNode (ConfFunction i) |
    ONode Operator | 
    FNode

instance Show (Personality i) where
    show (CNode _) = "CNode"
    show (ONode op) = "ONode " ++ (show op)
    show (FNode) = "FNode"

data GraphType = GTUnknown | GTPrg | GTLpg
    deriving (Show, Eq)

data Node i = Node {
    nLabel          :: Label,
    nTag            :: Tag,
    nPersonality    :: Personality i,
    nGraphType      :: GraphType,
    nAttributes     :: [Attribute],
    nPorts          :: [Port],
    nImplementation :: i
}-- deriving (Show)

instance Show (Node i) where
    show n = nLabel n


type PGraph i = DGI.Gr (Node i) Port
type PGNode i = DGI.LNode (Node i)
type PGEdge = DGI.LEdge Port
type PGContext i = DGI.Context (Node i) Port




-------------------------------------------------------------------------------
-- Configuration
type ConfSpace = String
type ConfMonad i a = ST.State (Int,[PGNode i]) a 
type ConfFunction i =
    Node i ->
    [(DGI.LNode (Node i), Port)] ->
    [(DGI.LNode (Node i), Port)] ->
    ConfSpace ->
    ConfMonad i [DGI.LEdge Port]


-- Get configuration function from node (assumes node is CNode)
nConfFun :: Node i -> ConfFunction i
nConfFun n = fun
    where (CNode fun) = nPersonality n



-------------------------------------------------------------------------------
-- Node functions

baseFNode label attr ports = Node {
        nLabel = label,
        nTag = "",
        nPersonality = FNode,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = () }

baseONode label attr ports op = Node {
        nLabel = label,
        nTag = "",
        nPersonality = ONode op,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = () }

baseCNode label attr ports cnf = Node {
        nLabel = label,
        nTag = "",
        nPersonality = CNode cnf,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = () }


nIsCNode :: Node i -> Bool
nIsCNode n = persIsCNode $ nPersonality n

nIsFNode :: Node i -> Bool
nIsFNode n = persIsFNode $ nPersonality n

nIsONode :: Node i -> Bool
nIsONode n = persIsONode $ nPersonality n
   


-------------------------------------------------------------------------------
-- Misc functions


persIsCNode :: Personality i -> Bool
persIsCNode (CNode _) = True
persIsCNode _ = False

persIsONode :: Personality i -> Bool
persIsONode (ONode _) = True
persIsONode _ = False

persIsFNode :: Personality i -> Bool
persIsFNode (FNode) = True
persIsFNode _ = False


opToString :: Operator -> String
opToString OpAnd = "AND"
opToString OpOr = "OR"


