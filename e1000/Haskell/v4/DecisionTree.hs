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
    , applyPrecondition
    , applyPostcondition
    , getActLstNode
    , getModLstNode
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
                , dependency :: [[Action]]
            }

-- To support printing of module
instance Show Module where
    show (Module pre post act dep) =
                "\nModule: { " ++ show act ++ show dep ++ "}"


instance Eq Module where
    (Module pre1 post1 act1 dep1) == (Module pre2 post2 act2 dep2) =
        (act1 == act2) && (dep1 == dep2)

data Node = Node {
                element :: Module
                , edges :: [Node]
            }
            deriving (Show, Eq)

getModLstNode :: [Node] -> [Module]
getModLstNode nlist = map (element) nlist

getActLstNode :: [Node] -> [Action]
getActLstNode nlist = map (action) $ getModLstNode nlist

-- ########### Helper functions for condition test ##############

-- Applies a precondition associated with given module
-- and returns the result
applyPrecondition :: [Node] -> Module -> Bool
applyPrecondition nl m = (preCond (pre m)) m nl

-- Applies the postcondition associated iwth given module
-- and returns the result
applyPostcondition :: [Node] -> Module -> [Node]
applyPostcondition nl m = (postCond (post m)) m nl

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

-- #################### Main module ####################

-- main function
main = do
        putStrLn out1
    where
        act = T Dropped
        precond = PreCondition initPrecondition
        postcond = PostCondition defaultPostcondition
        m = Module precond postcond act [[]]
        out1 = show m

-- ################################ EOF #################################

