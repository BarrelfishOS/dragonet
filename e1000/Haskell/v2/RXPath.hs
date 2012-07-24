
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

-- Function prototype for selecting proper action
type Classifier = (Packet -> Integer)

data Decision = Decision {
                selector :: Classifier
                , results :: [Action]
              }
           --  deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier

-- action specifiying what action each step can take
data Action = Dropped
            | InQueue {
                queueID :: QueueID
                }
            | ToDecide Decision
           --  deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier


-- Decision function implementation
decide :: Decision -> Packet -> Action
decide (Decision classifier actionList) pkt =
            case nextAction of
                Dropped -> Dropped
                InQueue q -> InQueue q
              --  ToDecide (Decision fnPtr actionList) -> decide2
                ToDecide toDecide -> decide toDecide pkt
                           -- (Decision fnPtr actionList) pkt
            where
                nextAction = actionList !! (fromIntegral $ classifier pkt)


-- #################### Few classifiers ####################

-- Validate L2 packet
isValidL2 :: Packet -> Integer
isValidL2 (RawPacket pktContents) = 1 -- FIXME: Assuming valid packet
isValidL2 _ = error "invalid type packet passed to isValidL2"

-- Check if it is valid IPv4 packet
isValidIPv4 :: Packet -> Integer
isValidIPv4 (L2Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidIPv4 _ = error "invalid type packet passed to isValidv4"

-- Check if it is valid IPv6 packet
isValidIPv6 :: Packet -> Integer
isValidIPv6 (L2Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidIPv6 _ = error "invalid type packet passed to isValidv6"

-- Check if it is valid L3 packet
isValidL3 :: Packet -> Integer
isValidL3 (IPv4Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidL3 (IPv6Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidL3 _ = error "invalid type packet passed to isValidL3"

-- Check if it is valid TCP packet
isValidTCP :: Packet -> Integer
isValidTCP (L3Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidTCP _ = error "invalid type packet passed to isValidTCP"

-- Check if it is valid UDP packet
isValidUDP :: Packet -> Integer
isValidUDP (L3Packet pkt) = 1 -- FIXME: Assuming valid packet
isValidUDP _ = error "invalid type packet passed to isValidUDP"

-- Selects queue after applying filters based on packet type
selectQueue :: Packet -> Integer
selectQueue (TCPPacket pkt) = 1 -- FIXME: get hash and select queue
selectQueue (UDPPacket pkt) = 1 -- FIXME: get hash and select queue
selectQueue _ = 0 -- Default queue (when no other filter matches)

-- Takes raw packet and returns associated action
classifyPacket :: Packet -> Action
classifyPacket pkt = decide Decision {
            selector = isValidTCP
               , results = [
                   ToDecide Decision {
                        selector = isValidUDP
                        , results = [
                            Dropped
                            , qDecision
                          ]
                   }
                   , qDecision
                ]
            } pkt
            where
                qDecision = ToDecide Decision {
                    selector = selectQueue
                    , results = [(InQueue 0), (InQueue 1)]
                }


selectProperQueue :: Packet -> Action
selectProperQueue pkt = decide (Decision selectQueue acList) pkt
                    where
                        acList = [(InQueue 0), (InQueue 1)]

-- #################### Main module ####################
-- main function
main = print "hello world"

-- ################################## EOF ###################################

