module FitnessHelpers (
    findNodes
    , stdDev
    , getStdDev
    , getFlowsG
    , getGoldFlowsG
    , getFlowsS
--  , getGoldFlowsS
    , findFlowQueues
    , getRXQueueCount
    , findPipelines
    , findNodesPerPL
    , findRXQueuesPerPL
    , findGoldRXQueuesPerPL
    , toPriorityClass
) where


import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Data.Graph.Inductive as DGI
import qualified Dragonet.ProtocolGraph as PG

import qualified Data.List as L
import qualified Data.Map as M

import Dragonet.Conventions (rxQPref, txQPref, qTag)

import Stack as SS

isQueueNode :: PG.Node -> Bool
isQueueNode (PG.ONode _ _ _ _ _)   = False
isQueueNode (PG.CNode _ _ _ _ _ _) = False
isQueueNode n = rxQPref `L.isPrefixOf` l || txQPref `L.isPrefixOf` l
    where l = PG.nLabel n


isRXQueueNode :: PG.Node -> Bool
isRXQueueNode (PG.ONode _ _ _ _ _)   = False
isRXQueueNode (PG.CNode _ _ _ _ _ _) = False
isRXQueueNode n = rxQPref `L.isPrefixOf` l
    where l = PG.nLabel n


isTXQueueNode :: PG.Node -> Bool
isTXQueueNode (PG.ONode _ _ _ _ _)   = False
isTXQueueNode (PG.CNode _ _ _ _ _ _) = False
isTXQueueNode n = txQPref `L.isPrefixOf` l
    where l = PG.nLabel n

isFlowNode :: PG.Node -> Bool
isFlowNode (PG.ONode _ _ _ _ _)   = False
isFlowNode (PG.CNode _ _ _ _ _ _) = False
isFlowNode n =  "RxL4UDP(" `L.isPrefixOf` l
    where l = PG.nLabel n


-- FIXME: TODO: Implement this function, currently it is only a placeholder
toPriorityClass :: PG.Node -> Int
toPriorityClass n = 1

-- FIXME: implement this function correctly
isGoldFlow :: PG.Node -> Bool
isGoldFlow (PG.ONode _ _ _ _ _)   = False
isGoldFlow (PG.CNode _ _ _ _ _ _) = False
isGoldFlow (PG.FNode label tag attr ports semantics impl origin) = True


findNodesPGraph :: (PG.Node -> Bool) -> PG.PGraph -> [PG.Node]
findNodesPGraph selector pg = toReturn
    where
        ele = DGI.labNodes pg
        onlyRX = filter (selector . snd ) ele
        toReturn = map snd onlyRX


-- findNodes with debug information
findNodes :: (PG.Node -> Bool) -> PL.PLGraph -> [(Int, String)]
findNodes selector plg = nodesPerPipeline -- (dbgMsg, count)
    where
        ele = DGI.labNodes plg
        nodesPerPipeline = map
                (\(_, x) -> (
                        (length $ findNodesPGraph selector (PL.plGraph  x)),
                        ("\n{{{" ++ show x ++ "} ==> " ++  -- pipeline name (or whole pipeline)
                            (show $ findNodesPGraph selector (PL.plGraph  x))  -- nodenames of RXqueue
                            ++ "}}} ==> " ++  ( show $ length $ findNodesPGraph selector (PL.plGraph  x)) ++ " \n"
                        )
                            )
                ) -- end lambda function
                ele

--        count = sum $ map fst queuesPerPipeline
--        dbgMsg = concat $ map snd queuesPerPipeline

average xs = realToFrac (sum xs) / L.genericLength xs

stdDev :: [Int] -> Float
stdDev vals = sd
    where
    sd :: Float
    sd = realToFrac (sqrt $ summedElements) / realToFrac lengthX
    variance :: Float
    variance = sd ^ 2
    length' = fromIntegral . length
    lengthX = length' vals
    myAvg = realToFrac (sum vals) / realToFrac lengthX
    summedElements = sum (map (\x -> ( (fromIntegral x) - myAvg) ^ 2) vals)


getStdDev :: [[a]] -> Float
getStdDev llist = stdDev $ map length llist


findRxTXQueues  = findNodes isQueueNode
findRxQueues    = findNodes isRXQueueNode
findTxQueues    = findNodes isTXQueueNode
findFlowQueues  = findNodes isFlowNode


getFlowsS :: SS.StackState -> Int
getFlowsS ss = endpoints
    where
        endpoints = length $ M.elems  -- converts endpoint map into list
                    $ SS.ssEndpoints ss -- get all endpoints from stack-state



-- FIXME: Complete the implementation
getGoldFlowsS :: SS.StackState -> Int
getGoldFlowsS ss = (getFlowsS ss) `div` 2


getFlowsG :: PL.PLGraph -> Int
getFlowsG plg =  sum flows
    where
        flowsPerQueue = findFlowQueues plg
        flows = map fst flowsPerQueue


-- FIXME: Complete the implementation
getGoldFlowsG :: PL.PLGraph -> Int
getGoldFlowsG plg =  (getFlowsG plg) `div` 2
--    where
--        flowsPerQueue = findFlowQueues plg
--        flows = map fst flowsPerQueue


getRXQueueCount :: PL.PLGraph -> Int
getRXQueueCount plg = sum qs
    where
        queuesPerPipeline = findRxQueues plg
        qs = map fst queuesPerPipeline


-- Find all pipeline graphs which are in PLGraph
findPipelines :: PL.PLGraph -> [PG.PGraph]
findPipelines plg = map (PL.plGraph . snd) $ DGI.labNodes plg

-- Get all flows in each pipeline, grouped by pipeline
findNodesPerPL :: (PG.Node -> Bool) -> PL.PLGraph -> [[PG.Node]]
findNodesPerPL selector plg = nodesPerPipeline
    where
        pls = findPipelines plg
        nodesPerPipeline = -- concat $
                map (\x -> findNodesPGraph selector x)
                pls

findRXQueuesPerPL = findNodesPerPL isFlowNode

findGoldRXQueuesPerPL = findNodesPerPL isGoldFlow

-- Return only those pipelines which have RX queues in them
getPLwithRxQueues :: PL.PLGraph -> [PG.PGraph]
getPLwithRxQueues plg = plWithRx
    where
        pls = findPipelines plg
        plWithRx =
                filter
                (\x -> (length $ findNodesPGraph isRXQueueNode x) == 1)
                $ pls

{-
getRXqueues :: PL.PLGraph -> [PG.Node]
getRXqueues plg =
    where
        allQs = findNodesPerPL isRXQueueNode plg
        verifiedQs = filter (\x -> (length x == 1)) allQs
-}

{-
-- get flows going through each HWQ
getFlowsInHWQ :: PL.PLGraph -> PL.Node
getFlowsG plg =
    where
        flowsPerQueue = findFlowQueues plg
        flows = map fst flowsPerQueue
-}





