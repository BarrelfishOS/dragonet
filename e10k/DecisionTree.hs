
module DecisionTree (
    Computation(..)
    , Decision(..)
) where

import qualified Data.ByteString as BS
import qualified NICState as NS

type QueueID = Integer  -- ID for the hardware queue

--module Main (main) where

-- packet: capturing incoming packet
-- Currently it is a placeholder and not actually used
data Packet = Packet {
                    bytes :: BS.ByteString
              }
              deriving (Show, Eq)

-- Computation: Name of the computation unit
-- Note: can also add a function which will actually perform the computation
--      using packet as input
data Computation = Computation {
        name :: String
    }
    deriving (Show, Eq)

-- Recursive decision tree which captures the decision tree
data Decision = Decision {
                compute :: Computation
                , possibleActions :: [Decision]
              }
              | Error {
                msg :: String
              }
              | Dropped
              | Processed
              | InQueue {
                queueID :: QueueID
              }
              deriving (Show, Eq)

-- #################### Decision function implementation ####################


-- ################################## EOF ###################################

