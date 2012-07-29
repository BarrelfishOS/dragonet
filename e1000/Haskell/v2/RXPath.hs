
module Main (main) where

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
            funPtr :: (Packet -> CResult)
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
type Name = String
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
                    , child :: Name
                   -- , parentAction :: Action
                   -- , childAction :: Action
                }
               deriving (Show, Eq)

data ResultSet = ResultSet {
                rootNode :: RootNode
                , declarations :: [Declaration]
                , relations :: [Relation]
                }
               deriving (Show, Eq)


printAction :: RootNode -> Action -> ResultSet
printAction root (Error msg) = let
                    eleName = "ERROR"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) inst)]
                in
                    ResultSet rName decls rels
printAction root (Dropped) = let
                    eleName = "DROPPED"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) inst)]
                in
                    ResultSet rName decls rels
printAction root (InQueue qid) = let
                    eleName = "RXQueue"
                    inst = eleName ++ (show qid)
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) inst)]
                in
                    ResultSet rName decls rels
printAction root (ToDecide des) = let
                    res = printDecision des
                    inst = rootNodeName (rootNode res)
                    rName = RootNode inst
                    decls = [] ++ (declarations res)
                    rels = [(Relation (rootNodeName root) inst)] ++
                            (relations res)
                in
                    ResultSet rName decls rels

-- Processes list of action and returns their combined results
myMapper :: RootNode -> [Action] -> ResultSet
myMapper root [] = error "Error: list of possible actions is empty!"
myMapper root (x:[]) = printAction root x
myMapper root (x:xs) = ResultSet rName decls rels
        where
            res1 = printAction root x
            resRest = myMapper root xs
            rName = rootNode res1
            decls = (declarations res1) ++ (declarations resRest)
            rels = (relations res1) ++ (relations resRest)

-- Convert decision data-strucutre into graph elements
printDecision :: Decision -> ResultSet
printDecision decision = ResultSet rName decls rels
        where
            eleName = rootNodeName $ RootNode (funName (selector decision))
            inst = eleName ++ "Fun"
            rName = RootNode inst
            results = myMapper rName (possibleActions decision)
            decls = [(Declaration inst eleName)] ++ (declarations results)
            rels =  relations results

-- #################### Decision function implementation ####################

-- findAction finds the action based on the classifier.
-- It also handles the Error case properly.
findAction :: Decision -> Packet -> Action
findAction (Decision classifier actionList) pkt =
    case ((funPtr classifier) pkt) of
        (InvalidState cause) -> Error cause
        (ValidAction idx) -> actionList !! (fromIntegral idx)

-- Decision function implementation
decide :: Decision -> Packet -> Action
decide (Decision classifier actionList) pkt =
    case nextAction of
        Error info -> Error info
        Dropped -> Dropped
        InQueue q -> InQueue q
        ToDecide toDecide -> decide toDecide pkt
    where
        nextAction = findAction (Decision classifier actionList) pkt

-- #################### Few classifiers ####################

-- Validate L2 packet
isValidL2 :: Packet -> CResult
isValidL2 (RawPacket pktContents) = ValidAction 1 -- FIXME: Assuming valid packet
isValidL2 _ = InvalidState "invalid type packet passed to isValidL2"

-- Check if it is valid IPv4 packet
isValidIPv4 :: Packet -> CResult
isValidIPv4 (L2Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidIPv4 _ = InvalidState "invalid type packet passed to isValidv4"

-- Check if it is valid IPv6 packet
isValidIPv6 :: Packet -> CResult
isValidIPv6 (L2Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidIPv6 _ = InvalidState "invalid type packet passed to isValidv6"

-- Check if it is valid L3 packet
isValidL3 :: Packet -> CResult
isValidL3 (IPv4Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidL3 (IPv6Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidL3 _ = InvalidState "invalid type packet passed to isValidL3"

-- Check if it is valid TCP packet
isValidTCP :: Packet -> CResult
isValidTCP (L3Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidTCP _ = InvalidState "invalid type packet passed to isValidTCP"

-- Check if it is valid UDP packet
isValidUDP :: Packet -> CResult
isValidUDP (L3Packet pkt) = ValidAction 1 -- FIXME: Assuming valid packet
isValidUDP _ = InvalidState "invalid type packet passed to isValidUDP"

-- Selects queue after applying filters based on packet type
selectQueue :: Packet -> CResult
selectQueue (TCPPacket pkt) = ValidAction 1 -- FIXME: get hash and select queue
selectQueue (UDPPacket pkt) = ValidAction 1 -- FIXME: get hash and select queue
selectQueue _ = ValidAction 0 -- Default queue (when no other filter matches)


theBigDecision :: Decision
theBigDecision =
            Decision {
                    selector = (Classifier isValidTCP "isValidTCP")
                    , possibleActions = [
                        ToDecide Decision {
                            selector = (Classifier isValidUDP "isValidUDP")
                            , possibleActions = [
                                Dropped
                                , qAction
                                ]
                        }
                        , qAction
                    ]
                }
            where
                qAction = ToDecide Decision {
                    selector = (Classifier selectQueue "selectQueue")
                    , possibleActions = [(InQueue 0), (InQueue 1)]
                }



-- Takes raw packet and returns associated action
classifyPacket :: Packet -> Action
classifyPacket pkt = decide theBigDecision pkt

-- #################### Main module ####################

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: Packet
getNextPacket = L3Packet $ RawPacket $ BS.pack ([50..120] :: [W.Word8])

-- main function
main = do
        putStrLn out1
        putStrLn out2
        putStrLn out3
    where
        nicState = NS.updateQueueElement NS.initNICState 6 1 1
        out1 = show $ classifyPacket getNextPacket
        out2 = show $ theBigDecision
        out3 = show $ printDecision theBigDecision

-- ################################## EOF ###################################

