
module Main (main) where

import qualified DecisionTree.DecisionTree as DT
import qualified  DecisionTree.ConvertDecision as CD


-- Config
-- eBCMNumQueues = 16 -- Actual number

-- Using 4 to reduce the complexity
eBCMNumQueues = 4


-- Receive queue
qname s i = s ++ (show i)

eBCMRxInterrupt next = [DT.Decision {
        DT.compute = (DT.Computation "RxInterrupt"),
        DT.possibleActions = next
}]

eBCMInQueue i = d
    where
        [d] = [(DT.InQueue i)]

makeQueues n = map eBCMInQueue [1..n]
eBCMInQueues = makeQueues eBCMNumQueues


eBCMFlexibleFilter next = [DT.Decision {
        DT.compute = (DT.Computation "RuleChecker"),
        DT.possibleActions = [DT.Dropped] ++ eBCMInQueues ++ next
}]

eBCMRxChecksumming next = [DT.Decision {
        DT.compute = (DT.Computation "RxChecksumOffload"),
        DT.possibleActions = next
}]

eBCMQueueSelect =
        eBCMFlexibleFilter []


eBCMVLANTagging next = [DT.Decision {
        DT.compute = (DT.Computation "VLANtagging"),
        DT.possibleActions = next
}]

eBCML2Filter next = eBCMVLANTagging $ next


-- Very basic overview of RX path (see figure 7.1)
eBCMRxPath = d
    where
        [d] = eBCML2Filter $ eBCMRxChecksumming $ eBCMQueueSelect

packetHandlingDes :: DT.Decision
packetHandlingDes = eBCMRxPath

-- #################### Main module ####################

-- main function
main = do
        putStrLn out4
    where
        myTree = CD.convertDT packetHandlingDes
        out4 = CD.printAbstractTree myTree

