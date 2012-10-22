#!/usr/bin/env runhaskell
{-
module DecisionTree (
    Computation(..)
    , Decision(..)
) where
-}

module Main (main) where

import qualified Data.ByteString as BS
-- import qualified NICState as NS

type QueueID = Integer  -- ID for the hardware queue

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

data Action = Dropped
            | Processed
            | InQueue {
                queueID :: QueueID
            }
            deriving (Show, Eq)

-- Recursive decision tree which captures the decision tree
data Decision = Decision {
                compute :: Computation
                , next :: [Decision]
              }
              | Action
              deriving (Show, Eq)


data Step = Step {
                pre :: Condition
                , post :: Condition
                , des :: Decision
            }
            deriving (Show, Eq)
-- #################### Decision function implementation ####################


-- #################### Main module ####################

-- main function
main = do
        putStrLn out1
    where
        des = Dropped
        out1 = show des

-- ################################## EOF ###################################

