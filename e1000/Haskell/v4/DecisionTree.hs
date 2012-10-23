#!/usr/bin/env runhaskell
module DecisionTree (
    Computation(..)
    , SDecision(..)
    , Action(..)
    , Condition(..)
    , Step(..)
) where

--module Main (main) where

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
              | Act {
                    saction :: Action
              }
              deriving (Show, Eq)

-- Stupid decision.  It is placeholder for decision and should
-- eventually replace the decision
data SDecision = SDecision {
                decision :: String
              }
              | SAct {
                    action :: Action
              }
              deriving (Show, Eq)

-- Defination of condition
data Condition = Condition {
                    tag :: String
               }
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Empty
               | Error
               deriving (Show, Eq)

data Step = Step {
                pre :: Condition
                , post :: Condition
                , des :: SDecision
            }
            deriving (Show, Eq)

data Node = Node {
                elem :: Step
                , edges :: [Node]
            }
            deriving (Show, Eq)

-- #################### Decision function implementation ####################


-- #################### Main module ####################

-- main function
tmain = do
        putStrLn out1
    where
        des = Dropped
        pre = Condition "Packet"
        post = Or (Condition "IPv4") (Condition "IPv6")
        step1 = Step pre post (SDecision "test")
        out1 = show step1

-- ################################## EOF ###################################

