
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
--          deriving (Show, Eq) -- FIXME: not working due to fun ptr Classifier


-- Decision function implementation
decide :: Classifier -> [Action] -> Packet -> Action
decide classifier actionList pkt =
            case nextAction of
                Dropped -> Dropped
                InQueue q -> InQueue q
                Decide fnPtr actionList -> decide fnPtr actionList pkt
            where
                nextAction = actionList !! (fromIntegral $ classifier pkt)


-- #################### Main module ####################
-- main function
main = print "hello world"

-- ################################## EOF ###################################

