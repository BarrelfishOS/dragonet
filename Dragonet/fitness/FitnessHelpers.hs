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
    , getPLwithRxQueuesFlows
    , getPLwithRXQueueIDs
    , FlowDetails
    , isGoldFlow
    , isGoldFlowLabel
) where


import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Data.Graph.Inductive as DGI
import qualified Dragonet.ProtocolGraph as PG

import qualified Data.List as L
import qualified Data.Map as M

import Dragonet.Conventions (rxQPref, txQPref, qTag)

import Stack as SS

type FlowDetails = String

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
toPriorityClass n = ans
    where
    ans
        | isGoldFlow n  = 2
        | otherwise     = 0



isFnode :: PG.Node -> Bool
isFnode (PG.ONode {})   = False
isFnode (PG.CNode {}) = False
isFnode (PG.FNode {}) = True


strToInt :: [Char] -> Int
strToInt n = read n :: Int



isGoldFlowLabel :: String -> Bool
isGoldFlowLabel [] = False
isGoldFlowLabel label = ans
    where
        lab = drop 1 $ takeWhile (/= ')') $ dropWhile (\x-> x /= '(') label
        lport =  drop 1 $ dropWhile (/= ':') $ drop 1 $ dropWhile (/= ':') lab
        ans
            | length lab == 0 = False
            | strToInt lport == 222 = True
            | otherwise = False


-- FIXME: implement this function correctly
isGoldFlow :: PG.Node -> Bool
isGoldFlow (PG.ONode _ _ _ _ _)   = False
isGoldFlow (PG.CNode _ _ _ _ _ _) = False
isGoldFlow (PG.FNode {PG.nLabel = label}) =
    isGoldFlowLabel  label


getQueueLabel :: PG.Node -> [Char]
getQueueLabel (PG.ONode _ _ _ _ _)   = []
getQueueLabel (PG.CNode _ _ _ _ _ _) = []
getQueueLabel (PG.FNode {PG.nLabel = label}) = ans
    where
        ans
            | rxQPref  `L.isPrefixOf` label = drop (length rxQPref) label
            | otherwise = []


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
getGoldFlowsG plg =  sum flows
    where
        flowsPerQueue = findGoldRXQueuesPerPL plg
        flows = map length flowsPerQueue



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


getPLwithRxQueuesFlows :: PL.PLGraph -> [(Int, [FlowDetails])]
getPLwithRxQueuesFlows plg = toFlows
    where
        pls = getPLwithRXQueueIDs plg
        toFlows = map
            (\(qid, graph) ->
                (qid,
                (map (show . PG.nLabel) $ findNodesPGraph isFlowNode graph))
            )
            pls


getPLwithRXQueueIDs :: PL.PLGraph -> [(Int, PG.PGraph)]
getPLwithRXQueueIDs plg = toQid
    where
        pls = getPLwithRxQueues plg
        toQid = map
            (\x ->
                ((strToInt $ getQueueLabel $ head $ findNodesPGraph isRXQueueNode x)
                , x)
            )
            pls

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





