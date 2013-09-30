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

data Node i = Node {
    nLabel          :: Label,
    nTag            :: Tag,
    nPersonality    :: Personality,
    nGraphType      :: GraphType,
    nAttributes     :: [Attribute],
    nPorts          :: [Port],
    nImplementation :: Maybe i
}-- deriving (Show)

instance Show (Node i) where
    show n = pref ++ nLabel n ++ "[" ++ nTag n ++ "]"
        where
            pref = case nGraphType n of
                GTLpg -> "L:"
                GTPrg -> "P:"
                _ -> ""

instance Eq (Node i) where
    a == b =
        (nLabel a == nLabel b)
        && (nTag a == nTag b)
        && (nGraphType a == nGraphType b)
        && (nAttributes a == nAttributes b)
        && (nPorts a == nPorts b)


type PGraph i = DGI.Gr (Node i) Port
type PGNode i = DGI.LNode (Node i)
type PGEdge = DGI.LEdge Port
type PGContext i = DGI.Context (Node i) Port




-------------------------------------------------------------------------------
-- Configuration
type ConfSpace = String
type ConfMonad i a = ST.State (Int,[PGNode i]) a 
type ConfFunction =
    forall i.
    Node i ->
    [(DGI.LNode (Node i), Port)] ->
    [(DGI.LNode (Node i), Port)] ->
    ConfSpace ->
    ConfMonad i [DGI.LEdge Port]


-- Get configuration function from node (assumes node is CNode)
nConfFun :: Node i -> ConfFunction
nConfFun n = fun
    where (CNode fun) = nPersonality n



-------------------------------------------------------------------------------
-- Node functions

baseFNode :: Label -> [Attribute] -> [Port] -> Maybe i -> Node i
baseFNode label attr ports impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = FNode,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }

baseONode :: Label -> [Attribute] -> [Port] -> Operator -> Maybe i -> Node i
baseONode label attr ports op impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = ONode op,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }

baseCNode :: Label -> [Attribute] -> [Port] -> ConfFunction -> Maybe i
                -> Node i
baseCNode label attr ports cnf impl = Node {
        nLabel = label,
        nTag = "",
        nPersonality = CNode cnf,
        nGraphType = GTUnknown,
        nAttributes = attr,
        nPorts = ports,
        nImplementation = impl }


nIsCNode :: Node i -> Bool
nIsCNode n = persIsCNode $ nPersonality n

nIsFNode :: Node i -> Bool
nIsFNode n = persIsFNode $ nPersonality n

nIsONode :: Node i -> Bool
nIsONode n = persIsONode $ nPersonality n

-- Returns true iff node is a LPG node, or a PRG node with the software
-- attribute
nIsSoftware :: Node i -> Bool
nIsSoftware n
    | elem "software" $ nAttributes n = True
    | otherwise = nGraphType n == GTLpg



   
-------------------------------------------------------------------------------
-- Protocol graph functions

pgSetType :: GraphType -> PGraph i -> PGraph i
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


