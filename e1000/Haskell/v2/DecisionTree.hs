
module DecisionTree (
    Packet
    , Classifier
    , CResult
    , Action
    , Decision
    , decide
    , convertDecision
    , selector
    , possibleActions
) where


--module Main (main) where

import qualified List as List
import qualified Data.Word as W
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
            decls = printDeclarations $ List.nub (declarations tree)
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

-- #################### Classifier placeholders ####################

-- Validate L2 packet
checksumCRC :: NS.NICState -> Packet -> CResult
checksumCRC nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
checksumCRC nicstate _ = InvalidState "invalid type packet passed to checksumCRC"

-- Validate L2 length
isValidLength :: NS.NICState -> Packet -> CResult
isValidLength nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
isValidLength nicstate _ = InvalidState "invalid type packet passed to isValidLength"

-- classify L2 packet
classifyL2 :: NS.NICState -> Packet -> CResult
classifyL2 nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
classifyL2 nicstate _ = InvalidState "invalid type packet passed to classifyL2"


-- Validate isValidUnicast packet
isValidUnicast :: NS.NICState -> Packet -> CResult
isValidUnicast nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
isValidUnicast nicstate _ = InvalidState "invalid type packet passed to isValidUnicast"

-- Validate isValidMulticast packet
isValidMulticast :: NS.NICState -> Packet -> CResult
isValidMulticast nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
isValidMulticast nicstate _ = InvalidState "invalid type packet passed to isValidMulticast"

-- Validate isValidBroadcast packet
isValidBroadcast :: NS.NICState -> Packet -> CResult
isValidBroadcast nicstate (RawPacket pktContents) = ValidAction 1
                -- FIXME: Assuming valid packet
isValidBroadcast nicstate _ = InvalidState "invalid type packet passed to isValidBroadcast"



-- Check if it is valid IPv4 packet
checksumIPv4 :: NS.NICState -> Packet -> CResult
checksumIPv4 nicstate (L2Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
checksumIPv4 nicstate _ = InvalidState "invalid type packet passed to isValidv4"

-- Check if it is valid IPv6 packet
checksumIPv6 :: NS.NICState -> Packet -> CResult
checksumIPv6 nicstate (L2Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
checksumIPv6 nicstate _ = InvalidState "invalid type packet passed to isValidv6"

-- Check if it is valid L3 packet
classifyL3 :: NS.NICState -> Packet -> CResult
classifyL3 nicstate (IPv4Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
classifyL3 nicstate (IPv6Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
classifyL3 nicstate _ = InvalidState "invalid type packet passed to classifyL3"

-- Check if it is valid TCP packet
isValidTCP :: NS.NICState -> Packet -> CResult
isValidTCP nicstate (L3Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidTCP nicstate _ = InvalidState "invalid type packet passed to isValidTCP"

-- Check if it is valid UDP packet
isValidUDP :: NS.NICState -> Packet -> CResult
isValidUDP nicstate (L3Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidUDP nicstate _ = InvalidState "invalid type packet passed to isValidUDP"

-- Selects queue after applying filters based on packet type
applyFilter :: NS.NICState -> Packet -> CResult
applyFilter nicstate (TCPPacket pkt) = ValidAction 1 -- FIXME: get hash and select queue
applyFilter nicstate (UDPPacket pkt) = ValidAction 1 -- FIXME: get hash and select queue
applyFilter nicstate _ = ValidAction 0 -- Default queue (when no other filter matches)

{-
packetHandlingDes :: Decision
packetHandlingDes = des
            where
                actionQSelect = ToDecide Decision {
                    selector = (Classifier applyFilter "applyFilter")
                    , possibleActions = [(InQueue 0), (InQueue 1)]
                }
                actionUDP = ToDecide Decision {
                     selector = (Classifier isValidUDP "isValidUDP")
                     , possibleActions = [ Dropped , actionQSelect]
                }
                actionL4 = ToDecide Decision {
                    selector = (Classifier isValidTCP "isValidTCP")
                    , possibleActions = [actionUDP , actionQSelect]
                }
                actionIPv4 = ToDecide Decision {
                    selector = (Classifier checksumIPv4 "checksumIPv4")
                    , possibleActions = [Dropped, actionL4]
                }
                actionIPv6 = ToDecide Decision {
                    selector = (Classifier checksumIPv6 "checksumIPv6")
                    , possibleActions = [Dropped, actionL4]
                }
                actionL3 = ToDecide Decision {
                    selector = (Classifier classifyL3 "classifyL3")
                    , possibleActions = [Dropped, actionIPv4, actionIPv6]
                }
                actionMulticast = ToDecide Decision {
                    selector = (Classifier isValidMulticast "isValidMulticast")
                    , possibleActions = [Dropped, actionL3]
                }
                actionBroadcast = ToDecide Decision {
                    selector = (Classifier isValidBroadcast "isValidBroadcast")
                    , possibleActions = [Dropped, actionL3]
                }
                actionUnicast = ToDecide Decision {
                    selector = (Classifier isValidUnicast "isValidUnicast")
                    , possibleActions = [Dropped, actionL3]
                }

                actionClassifyL2 = ToDecide Decision {
                    selector = (Classifier classifyL2 "classifyL2")
                    , possibleActions = [Dropped, actionUnicast,
                            actionMulticast, actionBroadcast]
                }

                actionValidateLength = ToDecide Decision {
                    selector = (Classifier isValidLength "isValidLength")
                    , possibleActions = [Dropped, actionClassifyL2]
                }
                des = Decision {
                    selector = (Classifier checksumCRC "checksumCRC")
                    , possibleActions = [Dropped, actionValidateLength]
                }


-- Takes raw packet and returns associated action
classifyPacket :: NS.NICState -> Packet -> Action
classifyPacket nicstate pkt = decide packetHandlingDes nicstate pkt

-- #################### Main module ####################

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: Packet
getNextPacket = L3Packet $ RawPacket $ BS.pack ([50..120] :: [W.Word8])

-- main function
main = do
--        putStrLn out1
--        putStrLn out2
--        putStrLn out3
        putStrLn out4
    where
        nicState = NS.updateQueueElement NS.initNICState 6 1 1
--        out1 = show $ classifyPacket nicState getNextPacket
        out2 = show $ packetHandlingDes
        myTree = convertDecision packetHandlingDes
        out3 = "\n\n\n\n\n"
        out4 = printAbstractTree myTree
-}

-- ################################## EOF ###################################

