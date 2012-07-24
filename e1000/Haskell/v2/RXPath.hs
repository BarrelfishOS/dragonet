
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
type CoreID = Integer  -- ID for the CPU which can accept notification

-- Function prototype for selecting proper action
type Classifier = (Packet -> Integer)

data Result = Dropped
            | InQueue {
                queueID :: QueueID
                }

-- action specifiying what action each step can take
data Action = DropPacket Packet
            | DMAPacket QueueID Packet
            | Decide (Classifier -> [Action] -> Packet -> Action)
--            deriving (Show, Eq) -- FIXME: not working due to function ptr


-- Decision function implementation
decide :: Classifier -> [Action] -> Packet -> Action
decide classifier actionList pkt =
                selectedAction
                where
                    idx = fromIntegral $ classifier pkt
                    selectedAction = actionList !! idx


-- #################### Main module ####################
{-
decideQueue :: Packet -> QueueID

chooseQueue :: hash -> queueid
-}

-- main function which
main = print "hello world"

-- ################################## EOF ###################################
