
module Main (main) where

import qualified NICState as NS
import qualified DecisionTree as DT
import qualified ConvertDecision as CD


-- Config
e10kNumQueues = 4


-- Receive queue
qname s i = s ++ (show i)

e10kRxDescFetch i next = [DT.Decision {
        DT.compute = (DT.Computation (qname "DescFetch" i)),
        DT.possibleActions = [DT.Dropped] ++ next
}]

e10kRxChecksumming i next = [DT.Decision {
        DT.compute = (DT.Computation (qname "RxChecksumOffload" i)),
        DT.possibleActions = next
}]

e10kRSC i next = [DT.Decision {
        DT.compute = (DT.Computation (qname "RSC" i)),
        DT.possibleActions = next
}]

e10kRxInterrupt i next = [DT.Decision {
        DT.compute = (DT.Computation (qname "RxInterrupt" i)),
        DT.possibleActions = next
}]

e10kInQueue i = d
    where
        [d] = e10kRxDescFetch i $ e10kRxChecksumming i $ e10kRSC i $
                e10kRxInterrupt i $ [(DT.InQueue i)]

makeQueues n = map e10kInQueue [1..n]
e10kInQueues = makeQueues e10kNumQueues


-- Queue assignment (section 7.1.2)
e10kEthertype next = [DT.Decision {
        DT.compute = (DT.Computation "EtherTypeFilter"),
            -- TODO: can packets really be dropped here?
        DT.possibleActions = [DT.Dropped] ++ e10kInQueues ++ next
}]

e10kSynFilter next = [DT.Decision {
        DT.compute = (DT.Computation "TCPSynFilter"),
        DT.possibleActions = e10kInQueues ++ next
}]

e10k5tupleFilter next = [DT.Decision {
        DT.compute = (DT.Computation "FiveTupleFilter"),
        DT.possibleActions = [DT.Dropped] ++ e10kInQueues ++ next
}]

e10kFlowDirectorFilter next = [DT.Decision {
        DT.compute = (DT.Computation "FlowDirectorFilter"),
        DT.possibleActions = [DT.Dropped] ++ e10kInQueues ++ next
}]

e10kRSSFilter next = [DT.Decision {
        DT.compute = (DT.Computation "RSSFilter"),
        DT.possibleActions = e10kInQueues ++ next
}]

e10kQueueSelect =
    e10kEthertype $ e10kSynFilter $ e10k5tupleFilter $
            e10kFlowDirectorFilter $ e10kRSSFilter []


-- Pool select (virtualization), not considered here
e10kPoolSelect next = next
--e10kPoolSelect next = [DT.Decision {
--        DT.compute = (DT.Computation "PoolSelect"),
--        DT.possibleActions = [DT.Dropped] ++ next
--}]


-- L2 filtering (section 7.1.1.1)
e10kL2UnicastFilter next = [DT.Decision {
        DT.compute = (DT.Computation "L2UnicastFilter"),
        DT.possibleActions = [DT.Dropped] ++ next
}]

e10kL2MulticastFilter next = [DT.Decision {
        DT.compute = (DT.Computation "L2MulticastFilter"),
        DT.possibleActions = [DT.Dropped] ++ next
}]

e10kVLANFilter next = [DT.Decision {
        DT.compute = (DT.Computation "VLANFilter"),
        DT.possibleActions = [DT.Dropped] ++ next
}]

e10kL2Filter next = e10kL2UnicastFilter $ e10kL2MulticastFilter $ e10kVLANFilter $ next


-- Very basic overview of RX path (see figure 7.1)
e10kRxPath = d
    where
        [d] = e10kL2Filter $ e10kPoolSelect $ e10kQueueSelect

packetHandlingDes :: DT.Decision
packetHandlingDes = e10kRxPath

-- #################### Main module ####################

-- main function
main = do
        putStrLn out4
    where
        nicState = NS.updateQueueElement NS.initNICState 6 1 1
        out2 = show $ packetHandlingDes
        myTree = CD.convertDT packetHandlingDes
        out3 = "\n\n\n\n\n"
        out4 = CD.printAbstractTree myTree

