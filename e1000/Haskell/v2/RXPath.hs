
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
            deriving (Show, Eq)



type QueueID = Integer  -- ID for the hardware queue

-- Function prototype for selecting proper action
type Classifier = (Packet -> Integer)

-- action specifiying what action each step can take
data Action = Dropped
            | InQueue {
                queueID :: QueueID
                }
            | Decide {  clf :: Classifier
                        , alist :: [Action]
                     }
--            | Decide (Classifier -> [Action] -> Packet -> Action)
--            deriving (Show, Eq) -- FIXME: not working due to function ptr



processSelected :: Action -> Packet -> Action
processSelected (Dropped) pkt = Dropped
processSelected (InQueue q) pkt = InQueue q
processSelected (Decide fnPtr actionList) pkt = decide fnPtr actionList pkt


-- Decision function implementation
decide :: Classifier -> [Action] -> Packet -> Action
decide classifier actionList pkt = let
                    idx = fromIntegral $ classifier pkt
                    selectedAction = actionList !! idx
                in
                    processSelected selectedAction pkt


-- #################### Main module ####################
-- main function
main = print "hello world"

-- ################################## EOF ###################################

