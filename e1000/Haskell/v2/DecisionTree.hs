
module DecisionTree (
    Packet(..)
    , Classifier(..)
    , CResult(..)
    , Action(..)
    , Decision(..)
    , decide
) where


--module Main (main) where
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
{-
data RDecision = RDecision {
                selector :: Classifier
                , possibleActions :: [RDecision]
              }
              | RDropped
              | RProcessed
              deriving (Show, Eq)
-}


-- action specifiying what action each step can take
data Action = Error String
            | Dropped
            | Processed
            | InQueue {
                queueID :: QueueID
                }
            | ToDecide Decision
            deriving (Show, Eq)

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
        Processed -> Processed
        Error info -> Error info
    where
        nextAction = applyDecision (Decision classifier actionList) nicstate pkt

-- ################################## EOF ###################################

