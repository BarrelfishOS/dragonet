module Fitness (
    fitnessFunction
    , priorityFitness
) where



import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Data.Graph.Inductive as DGI
import qualified Dragonet.ProtocolGraph as PG

import qualified Data.List as L
import qualified Data.Map as M

import Dragonet.Conventions (rxQPref, txQPref, qTag)

import qualified Stack as SS

--import FitnessHelpers
import qualified FitnessHelpers as FH



--type CostFunction a = PL.PLGraph -> a
-- Does not really matter as we only have one config
--costFunction :: StackState -> PL.PLGraph -> Int
-- [NOTE] Lower cost is better...
-- [NA] Howto Identify HW queue, app node, socket node?
-- [NA] How many flows in each queue?
-- [NA] Does flow-X having dedicated HW?
-- [NA] How many SW nodes flow-X will cross?
-- [NA] How well balanced are the flows across HW queues?
--fitnessFunction :: StackState -> O.CostFunction Int
--fitnessFunction ss plg = (ans, dbg)
fitnessFunction :: SS.StackState -> [SS.EndpointDesc] -> O.CostFunction Float
--fitnessFunction ss plg = ((sum qcount), dbg2)
fitnessFunction ss eps plg = (ans2, dbg2)
    where
        dbg = "cost: " ++ (show ans2) ++ ", DEBUGINFO: [[" ++ (show msg) ++ "]]\n"
        dbg2 = "cost: " ++ (show qcount) ++ " = " ++ (show ans2) ++  ", DEBUGINFO: [[" ++ (show dbgMsg) ++ "]]\n"
        ans = length unusedQueues
        ele = DGI.labNodes plg
        nonemptyEles1 = filter (\(_, x) -> (length $ DGI.labNodes (PL.plGraph  x)) /= 0) ele

        -- counting unconnected pipelines to estimate unused queues
        unusedQueues = filter (\x@(a,b) -> ((length $ DGI.neighbors plg a) == 0)) ele
        msg = show unusedQueues


        -- Finding RX queues
--        (dbgMsg, qcount) = findRxTXQueues plg
        flowsPerQueue = FH.findFlowQueues plg
        qcount = map fst flowsPerQueue
        dbgMsg = map snd flowsPerQueue
        ans2 = FH.stdDev qcount
--        msg = show ele
        endpoints = length $ eps

        --endpoints =  length $ M.elems  -- converts endpoint map into list
        --            $ SS.ssEndpoints ss -- get all endpoints from stack-state



-- check if flows going to each pipeline are mixed, or belong to separate classs
areFlowsIsolated :: PL.PLGraph -> Bool
areFlowsIsolated plg = ans
    where
        flowsPerPL = FH.findRXQueuesPerPL plg
        isPlGood = L.nub $ map validateFlows flowsPerPL

        ans
            | (length isPlGood == 1) = head isPlGood
            | otherwise = False

        validateFlows flow
              | (length flow) == 0  = True
              | (length flow) == 1  = True
              | (length $ L.nub $ map FH.isGoldFlow flow) == 1 = True
              | otherwise  = False


-- check if flows going to each pipeline are mixed, or belong to separate classs
areFlowsIsolatedLabel :: [(Int, [FH.FlowDetails])] -> Bool
areFlowsIsolatedLabel epsQueuesMaping = ans
    where
        flowsPerPL = map snd epsQueuesMaping

        isPlGood = L.nub $ map validateFlows flowsPerPL

        ans
            | (length isPlGood == 1) = head isPlGood
            | otherwise = False

        validateFlows flow
              | (length flow) <= 1  = True
              | (length $ L.nub $ map FH.isGoldFlowLabel flow) == 1 = True
              | otherwise  = False


-- get flows going through each HWQ
maxFlowsPerGoldQueue = 1

costFn :: [(Int, [FH.FlowDetails])] -> (Float, [Char])
costFn epsQueuesMaping = fv -- 0.0
    where

        queues = length $ L.nub $ map fst epsQueuesMaping
        flows  = map snd epsQueuesMaping
        goldFlows = map (\(x, ll) -> (x, (filter FH.isGoldFlowLabel ll))) epsQueuesMaping

        stdDevAllQueues     = FH.getStdDev $ map snd epsQueuesMaping
        stdDevGoldQueues    = FH.getStdDev $ map snd goldFlows

        aFlows = sum $ map (\(qid, ff) -> length ff) epsQueuesMaping
        gFlows = sum $ map (\(qid, ff) -> length ff) goldFlows

        minGQueues
            | (gFlows `mod` maxFlowsPerGoldQueue) > 0 = (gFlows `div` maxFlowsPerGoldQueue) + 1
            | otherwise  = (gFlows `div` maxFlowsPerGoldQueue)

        dbgMsg = " HW Queues: " ++ (show queues) ++
                    ", gold flows: " ++ (show gFlows) ++
                    ", goldQueusNeeded: " ++ (show minGQueues) ++
                    ", AllFlows: " ++ (show aFlows) ++
                    " "
        fv
            | (queues - 1)  < minGQueues                = (0.0, ("WARNING: Not enough HW queues (" ++
                           dbgMsg ++ ")"))
            | not $ areFlowsIsolatedLabel epsQueuesMaping = (0.0, "WARNING: gold and normal flows are mixed (" ++
                           dbgMsg ++ ")")
            | otherwise                                 =
                                    (
                                        (stdDevGoldQueues + 100 + (stdDevAllQueues + 10))
                                        , ("Acceptable conf (" ++
                                                dbgMsg ++ ")")
                                    )


priorityFitness :: SS.StackState -> [SS.EndpointDesc] -> O.CostFunction Float
priorityFitness ss eps plg = costFn $ FH.getPLwithRxQueuesFlows plg



priorityFitness' :: SS.StackState -> [SS.EndpointDesc] -> O.CostFunction Float
priorityFitness' ss eps plg = fv
    where

        stdDevAllQueues     = FH.getStdDev $ FH.findRXQueuesPerPL plg
        stdDevGoldQueues    = FH.getStdDev $ FH.findGoldRXQueuesPerPL plg

        aFlows = FH.getFlowsG plg
        gFlows = FH.getGoldFlowsG plg
        queues = FH.getRXQueueCount plg

        minGQueues
            | (gFlows `mod` maxFlowsPerGoldQueue) > 0 = (gFlows `div` maxFlowsPerGoldQueue) + 1
            | otherwise  = (gFlows `div` maxFlowsPerGoldQueue)

        dbgMsg = " HW Queues: " ++ (show queues) ++
                    ", gold flows: " ++ (show gFlows) ++
                    ", goldQueusNeeded: " ++ (show minGQueues) ++
                    ", AllFlows: " ++ (show aFlows) ++
                    " "
        fv
            | (queues - 1)  < minGQueues   = (0.0, ("WARNING: Not enough HW queues (" ++
                           dbgMsg ++ ")"))
            | not $ areFlowsIsolated plg             = (0.0, "WARNING: gold and normal flows are mixed (" ++
                           dbgMsg ++ ")")
            | otherwise                     =
                                    (
                                        (stdDevGoldQueues * 100 + (stdDevAllQueues * 10))
                                        , ("Acceptable conf (" ++
                                                dbgMsg ++ ")")
                                    )

