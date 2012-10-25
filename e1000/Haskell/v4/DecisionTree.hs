#!/usr/bin/env runhaskell

module DecisionTree (
    Action(..)
    , Terminal(..)
    , NonTerminal(..)
    , PreCondition(..)
    , PostCondition(..)
    , Module(..)
    , Node(..)
    , defaultPostcondition
    , finalPostcondition
    , initPrecondition
) where

-- module Main (main) where

type QueueID = Integer  -- ID for the hardware queue

type Tag = String -- tag for condition

data Terminal = Dropped
            | Processed
            | InQueue {
                queueID :: QueueID
            }
            deriving (Show, Eq)

data NonTerminal = NIC
            | Ethernet
            | IPv4
            | IPv6
            | ICMP
            | UDP
            | TCP
            | RTP
            deriving (Show, Eq)

data Action = T Terminal
            | NT NonTerminal
            deriving (Show, Eq)


data PreCondition = PreCondition {
            preCond :: (Module -> [Node] -> Bool)
        }

data PostCondition = PostCondition {
            postCond :: (Module -> [Node] -> [Node])
        }

data Module = Module {
                pre :: PreCondition
                , post :: PostCondition
                , action :: Action
            }

-- To support printing of module
instance Show Module where
    show (Module pre post action) = show action


instance Eq Module where
    (Module pre1 post1 action1) == (Module pre2 post2 action2) =
        (action1 == action2)


data Node = Node {
                elem :: Module
                , edges :: [Node]
            }
            deriving (Show, Eq)

-- ################ Decision function implementation #################


-- #################### Main module ####################

-- Precondition for the first module
-- If the graph is empty then it will return true, otherwise false
initPrecondition :: Module -> [Node] -> Bool
initPrecondition _ [] = True
initPrecondition _ _ = False

-- Postcondition for last node
-- It will return the same module without any modification
finalPostcondition :: Module -> [Node] -> [Node]
finalPostcondition _ n = n

-- Postcondition for most of modules
-- It will just add given module at end of nodes list
defaultPostcondition :: Module -> [Node] -> [Node]
defaultPostcondition m n = n ++ [newNode]
        where
            newNode = (Node m [])

{-
allActionsPerformed :: [Node]
-- check if given action is performed by atleast one module in nodes list.
isActionPerformed :: [Node] -> Action -> Bool
isActionPerformed nlist action

-- function which can be passed to precondition checking
-- This precondition function makes sure that list of action is performed
-- as part of precondition
preConditionCheck :: [Action] ->  Module -> [Node] -> Bool
preConditionCheck alist mod nlist =

-}

-- main function
main = do
        putStrLn out1
    where
        act = T Dropped
        precond = PreCondition initPrecondition
        postcond = PostCondition defaultPostcondition
        m = Module precond postcond act
        out1 = show m

-- ################################ EOF #################################

