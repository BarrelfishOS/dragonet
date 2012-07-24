
module Main (main) where

import qualified Data.ByteString as BS

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
        }
       -- deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier

data Decision = Decision {
                selector :: Classifier
                , possibleActions :: [Action]
              }
           --  deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier

-- action specifiying what action each step can take
data Action = Error String
            | Dropped
            | InQueue {
                queueID :: QueueID
                }
            | ToDecide Decision
           --  deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier


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

-- Takes raw packet and returns associated action
classifyPacket :: Packet -> Action
classifyPacket pkt = decide Decision {
            selector = (Classifier isValidTCP)
               , possibleActions = [
                   ToDecide Decision {
                        selector = (Classifier isValidUDP)
                        , possibleActions = [
                            Dropped
                            , qDecision
                          ]
                   }
                   , qDecision
                ]
            } pkt
            where
                qDecision = ToDecide Decision {
                    selector = (Classifier selectQueue)
                    , possibleActions = [(InQueue 0), (InQueue 1)]
                }


selectProperQueue :: Packet -> Action
selectProperQueue pkt = decide (Decision (Classifier selectQueue) acList) pkt
                    where
                        acList = [(InQueue 0), (InQueue 1)]

-- #################### Main module ####################
-- main function
main = print "hello world"

-- ################################## EOF ###################################

