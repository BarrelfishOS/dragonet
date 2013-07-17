#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}



module Operations(
      DesFunction
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
    , getNodeEdges
    , setNodeEdges
    , appendToTrue
    , appendToFalse
    , nTreeNodes
    , getDropNode
    , getNodeAttributes
    , nLabel
    , nConfImpl
    , nAttributes
    , ConfFunction
    , applyConfig
) where

import qualified Data.List as L
import Data.Maybe


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

type ConfFunction = ([(Node,String)] -> [(String,Node)] -> String -> [(Node,String,Node)])

-- opearator function: Based on the result of incoming edges,
--  decides which outgoing edges to choose
type OpFunction = (Operator -> [Node] -> [Node])

data NodeEdges = BinaryNode ([Node], [Node])
        | NaryNode [(String, [Node])]
        deriving (Show, Eq)

data GNode l a f = GNode {
    gLabel              :: l
    , gTag              :: TagType
    , gAttributes       :: [a]
    , gEdges            :: NodeEdges
    , gImplementation   :: [Implementation TagType f]
} deriving (Show, Eq)


data Decision =  Decision (GNode String String DesFunction)
    deriving (Show, Eq)
data Configuration = Configuration (GNode String String ConfFunction)
    deriving (Show, Eq)
data Operator = Operator (GNode String String OpFunction)
    deriving (Show, Eq)

class NodeCompare a where
    nCompPrgLpg :: a -> a -> Bool
    nCompPrgLpgV2 :: a -> a -> Bool


data Node = Des Decision
    | Conf Configuration -- (GNode NB.ConfLabel ConfFunction) --
    | Opr Operator -- (GNode NB.OpLabel OpFunction) --
    deriving (Show, Eq)

instance NodeCompare Node where
    nCompPrgLpg (Des (Decision n1)) (Des (Decision n2)) =
        -- todo: was embedCompare
        (gLabel n1) == (gLabel n2)
    nCompPrgLpg _ _ = False

    nCompPrgLpgV2 (Des (Decision n1)) (Des (Decision n2)) =
        (gLabel n1) == (gLabel n2)
    nCompPrgLpgV2 (Opr (Operator n1)) (Opr (Operator n2)) =
        (gLabel n1) == (gLabel n2)
    nCompPrgLpgV2 (Conf (Configuration n1)) (Conf (Configuration n2)) =
        (gLabel n1) == (gLabel n2)
    nCompPrgLpgV2 _ _ = False
 


nLabel :: Node -> String
nLabel (Des (Decision gn)) = gLabel gn
nLabel (Conf (Configuration gn)) = gLabel gn
nLabel (Opr (Operator gn)) = gLabel gn

nConfImpl :: Node -> ConfFunction
nConfImpl (Conf (Configuration gn)) = f
    where (Implementation _ f) = head $ gImplementation gn


nAttributes :: Node -> [String]
nAttributes (Des (Decision gn)) = gAttributes gn
nAttributes (Conf (Configuration gn)) = gAttributes gn
nAttributes (Opr (Operator gn)) = gAttributes gn


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
                (NaryNode as) -> L.nub $ concat $ map snd as
        children = concat (map nTreeNodes ep)


getConfNode :: String -> TagType -> NodeEdges -> [String] -> ConfFunction -> Node
getConfNode op tag edges attrs fun = Conf $ Configuration GNode {
        gLabel = op
        , gTag = tag
        , gEdges = edges
        , gAttributes = attrs
        , gImplementation = [(Implementation "" fun)]
    }



getDecNode :: String -> TagType -> NodeEdges -> [String] -> Node
getDecNode op tag edges attrs = Des $ Decision GNode {
        gLabel = op
        , gTag = tag
        , gEdges = edges
        , gAttributes = attrs
        , gImplementation = []
    }

getOperatorNode :: String -> TagType -> NodeEdges -> [String] -> Node
getOperatorNode op tag edges attrs = Opr $ Operator GNode {
        gLabel = op
        , gTag = tag
        , gEdges = edges
        , gAttributes = attrs
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


getDropNode :: Node
getDropNode = getDecNode "packetDrop" "Drop" (BinaryNode ([],[])) []

getNodeAttributes :: Node -> [String]
getNodeAttributes node = case node of
    Des (Decision (GNode _ _ a _  _)) -> a
    Conf (Configuration (GNode _ _ a _  _)) -> a
    Opr (Operator (GNode _ _ a _  _)) -> a


applyConfig :: [(String,String)] -> [(Node,String,Node)] -> [(Node,String,Node)]
applyConfig cfg g =
    rmNewSources $ foldl handleCNode g configNodes
    where
        configNodes = L.nub $ filter isConfN $ (map fst3 g) ++ (map third3 g)

        isConfN :: Node -> Bool
        isConfN (Conf _) = True
        isConfN _ = False

        fst3 (a,_,_) = a
        third3 (_,_,a) = a

        sminus a b = filter (not . (flip elem b)) a

        handleCNode g' cn = g'' ++ (cF inE outE c)
            where
                inE' = filter ((== cn) . third3) g'
                outE' = filter ((== cn) . fst3) g'
                g'' = g' `sminus` (inE' ++ outE')
                inE = map (\(a,b,c) -> (a,b)) inE'
                outE = map (\(a,b,c) -> (b,c)) outE'
                c = fromJust $ lookup (nLabel cn) cfg 
                cF = nConfImpl cn

        sources g' = (map fst3 g') `sminus` (map third3 g')

        rmNode g' n = filter (\a -> fst3 a /= n && third3 a /= n) g'

        rmNewSources g'
            | (null newSources) = g'
            | otherwise = rmNewSources g''
            where
                newSources = (sources g') `sminus` (sources g)
                n = head newSources
                g'' = rmNode g' n



