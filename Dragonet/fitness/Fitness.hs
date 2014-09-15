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
fitnessFunction :: SS.StackState -> O.CostFunction Float
--fitnessFunction ss plg = ((sum qcount), dbg2)
fitnessFunction ss plg = (ans2, dbg2)
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
        endpoints =  length $ M.elems  -- converts endpoint map into list
                    $ SS.ssEndpoints ss -- get all endpoints from stack-state




-- check if flows going to each pipeline are mixed, or belong to separate classs
areFlowsMixed :: PL.PLGraph -> Bool
areFlowsMixed plg = ans
    where
        flowsPerPL = FH.findRXQueuesPerPL plg
        isPlGood = map validateFlows flowsPerPL
        anss = filter (\x -> if x then False else True) isPlGood
        ans
            | anss == [] = True
            | otherwise = False
        validateFlows flow
              | (length flow) == 0  = False
              | (length flow) == 1  = False
              | (length $ L.nub $ map FH.toPriorityClass flow) > 1 =  False
              | otherwise  = False


-- get flows going through each HWQ
maxFlowsPerGoldQueue = 1

priorityFitness :: SS.StackState -> O.CostFunction Float
priorityFitness ss plg = fv
    where

        stdDevAllQueues     = FH.getStdDev $ FH.findRXQueuesPerPL plg
        stdDevGoldQueues    = FH.getStdDev $ FH.findGoldRXQueuesPerPL plg

        --gFlows = FH.getGoldFlowsS ss
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
            | areFlowsMixed plg             = (0.0, "WARNING: gold and normal flows are mixed (" ++
                           dbgMsg ++ ")")
            | otherwise                     =
                                    (
                                        (stdDevGoldQueues * 100 + (stdDevGoldQueues * 10))
                                        , ("Acceptable conf (" ++
                                                dbgMsg ++ ")")
                                    )


