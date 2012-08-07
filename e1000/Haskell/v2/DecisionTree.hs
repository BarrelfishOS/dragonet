
module DecisionTree (
    Packet(..)
    , Classifier(..)
    , CResult(..)
    , Action(..)
    , Decision(..)
    , decide
    , convertDecision
    , printAbstractTree
) where


--module Main (main) where
import qualified Data.List as DL
import qualified Data.ByteString as BS
import qualified NICState as NS

-- packet with tags based on how it is classified
data Packet = RawPacket {
                    bytes :: BS.ByteString
                }
            | L2Packet Packet
            | IPv4Packet Packet
            | IPv6Packet Packet
            | L3Packet Packet
            | TCPPacket Packet
            | UDPPacket Packet
            | InvalidPacket {
                contents :: Packet
                , reason :: String
             }
            deriving (Show, Eq)

type QueueID = Integer  -- ID for the hardware queue

data CResult = InvalidState String
        | ValidAction Integer
        deriving (Show, Eq)


-- Function prototype for selecting proper action
--type Classifier = (Packet -> CResult)
data Classifier = Classifier {
            funPtr :: (NS.NICState -> Packet -> CResult)
            , funName :: String
        }

-- To simplify the printing of data-structures like Decision and Action
instance Show Classifier where
    show (Classifier fpr fun_name) = show fun_name

instance Eq Classifier where
    (Classifier fpr1 fun_name1) == (Classifier fpr2 fun_name2) =
                (fun_name1 == fun_name2)


data Decision = Decision {
                selector :: Classifier
                , possibleActions :: [Action]
              }
              deriving (Show, Eq)



-- action specifiying what action each step can take
data Action = Error String
            | Dropped
            | InQueue {
                queueID :: QueueID
                }
            | ToDecide Decision
            deriving (Show, Eq)

-- #################### Code for traversing/printing Decision ################
-- Code for traversing the data-structure
type Name = String -- the name of elements (printable)
type Position = Integer -- position of action with respect to siblings

data RootNode = RootNode {
                    rootNodeName ::  Name
                   -- , relatedAction :: Action
               }
               deriving (Show, Eq)

data Declaration = Declaration {
                    instanceName :: Name
                    , instanceType :: Name
                   -- , instanceAction :: Action
                }
               deriving (Show, Eq)

data Relation = Relation {
                    parent :: Name
                    , childPos :: Position
                    , child :: Name
                   -- , parentAction :: Action
                   -- , childAction :: Action
                }
               deriving (Show, Eq)

data AbstractTree = AbstractTree {
                rootNode :: RootNode
                , declarations :: [Declaration]
                , relations :: [Relation]
                }
               deriving (Show, Eq)

printRelations :: [Relation] -> String
printRelations [] = []
printRelations (x:[]) = (parent x) ++ "[" ++ (show (childPos x)) ++ "]"
                        ++ " -> " ++ (child x) ++ "\n"
printRelations (x:xs) = (printRelations [x]) ++ (printRelations xs)


printDeclarations :: [Declaration] -> String
printDeclarations [] = []
printDeclarations (x:[]) = (instanceName x) ++ " :: " ++ (instanceType x)
                                        ++ "()" ++ "\n"
printDeclarations (x:xs) = (printDeclarations [x]) ++ (printDeclarations xs)

printAbstractTree :: AbstractTree -> String
printAbstractTree tree = decls ++ rels
        where
            decls = printDeclarations $ DL.nub (declarations tree)
            rels = printRelations $ relations tree

convertAction :: RootNode -> Position -> Action -> AbstractTree
convertAction root pos (Error msg) = let
                    eleName = "ERROR"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (Dropped) = let
                    eleName = "DROPPED"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (InQueue qid) = let
                    eleName = "RXQueue"
                    inst = eleName ++ (show qid)
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (ToDecide des) = let
                    res = convertDecision des
                    inst = rootNodeName (rootNode res)
                    rName = RootNode inst
                    decls = [] ++ (declarations res)
                    rels = [(Relation (rootNodeName root) pos inst)] ++
                            (relations res)
                in
                    AbstractTree rName decls rels

-- Processes list of action and returns their combined results
myMapper :: RootNode -> Position -> [Action] -> AbstractTree
myMapper root pos [] = error "Error: list of possible actions is empty!"
myMapper root pos (x:[]) = convertAction root pos x
myMapper root pos (x:xs) = AbstractTree rName decls rels
        where
            res1 = convertAction root pos x
            resRest = myMapper root (pos + 1) xs
            rName = rootNode res1
            decls = (declarations res1) ++ (declarations resRest)
            rels = (relations res1) ++ (relations resRest)

-- Convert decision data-strucutre into graph elements
convertDecision :: Decision -> AbstractTree
convertDecision decision = AbstractTree rName decls rels
        where
            eleName = rootNodeName $ RootNode (funName (selector decision))
            inst = eleName ++ "Fun"
            rName = RootNode inst
            results = myMapper rName 0 (possibleActions decision)
            decls = [(Declaration inst eleName)] ++ (declarations results)
            rels =  relations results

-- #################### Decision function implementation ####################

-- applyDecision finds the action based on the classifier.
-- It also handles the Error case properly.
applyDecision :: Decision -> NS.NICState -> Packet -> Action
applyDecision (Decision classifier actionList) nicstate pkt =
    case ((funPtr classifier) nicstate pkt) of
        (InvalidState cause) -> Error cause
        (ValidAction idx) -> actionList !! (fromIntegral idx)

-- Decision function implementation
decide :: Decision -> NS.NICState -> Packet -> Action
decide (Decision classifier actionList) nicstate pkt =
    case nextAction of
        ToDecide toDecide -> decide toDecide nicstate pkt
        InQueue q -> InQueue q
        Dropped -> Dropped
        Error info -> Error info
    where
        nextAction = applyDecision (Decision classifier actionList) nicstate pkt

-- ################################## EOF ###################################

