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
    , getActLstMod
    , getModLstNode
    , main
) where

-- module Main (main) where

type QueueID = Integer  -- ID for the hardware queue

-- set of terminal actions, which mark that there is no further execution
data Terminal = Dropped
            | Processed
            | InQueue {
                queueID :: QueueID
            }
            deriving (Show, Eq, Ord)

-- set of non-terminal action, which means that these can be further extended
data NonTerminal = NIC
            | Ethernet
            | IPv4
            | IPv6
            | ICMP
            | UDP
            | TCP
            | RTP
            deriving (Show, Eq, Ord)

-- Actions which can be taken from a module
data Action = T Terminal
            | NT NonTerminal
            deriving (Show, Eq, Ord)


data PreCondition = PreCondition {
            preCond :: (Module -> [Node] -> Bool)
        }

data PostCondition = PostCondition {
            postCond :: (Module -> [Node] -> [Node])
        }

-- Module marks a computation which should be performed on every node
-- I should rename it to Computation
data Module = Module {
                pre :: PreCondition
                , post :: PostCondition
                , action :: Action
                , dependency :: [[Action]]
            }

-- To support printing of module
instance Show Module where
    show (Module _ _ act1 dep1) =
                "Module: { " ++ show act1 ++ show dep1 ++ "}\n"


instance Eq Module where
    (Module _ _ act1 dep1) == (Module _ _ act2 dep2) =
        (act1 == act2) && (dep1 == dep2)

-- It is node in dataflow graph where vertex is a module/computation
-- and edges mark which other computations can be performed after this computation
data Node = Node {
                element :: Module
                , edges :: [Node]
            }
            deriving (Eq)

instance Show Node where
    show (Node elem1 _) =
                show elem1
                --  "\nNode: { element " ++ show elem1 ++ "}"



getModLstNode :: [Node] -> [Module]
getModLstNode nlist = map (element) nlist


getActLstMod :: [Module] -> [Action]
getActLstMod mlist = map (action) mlist

getActLstNode :: [Node] -> [Action]
getActLstNode nlist = getActLstMod $ getModLstNode nlist

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
main :: IO()
main = do
        putStrLn out1
    where
        act = T Dropped
        precond = PreCondition initPrecondition
        postcond = PostCondition defaultPostcondition
        m = Module precond postcond act [[]]
        out1 = show m

-- ################################ EOF #################################

