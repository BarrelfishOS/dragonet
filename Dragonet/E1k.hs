#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Physical Resource Graph (PRG) for the E1k NIC (Intel 82576 1GbE).
 - This PRG only shows the receive side and not the send side.
 - Currently, this graph captures only one of the possible combination
 - of all the possible configurations.
 - In future, this and other possible valid configurations needs to be
 - generated automatically.
 -}

--module Main (
module E1k (
    getE1kPRG
    , getE1kPRGConf
    , getE1kPRGConfTest
    , getE1kPRGConfTestV2
    , main
) where


import qualified MyGraph as MG
import qualified Computations as MC
--import qualified Configurations as MConf
import qualified Computations as MConf
import qualified Data.List as DL
import qualified Debug.Trace as DT


getE1kPRGConfTestV2 ::  IO()
getE1kPRGConfTestV2 =
    do

        putStrLn "#################  basicPRG  ###########################\n"
        putStrLn $ show $ basicPRG
        putStrLn "#################  Conf ###########################\n"
        putStrLn $ show $ exampleConf
        putStrLn "#################  activeNodesForConf ###################\n"
        putStrLn $ show $ getActiveNodesForConf basicPRG exampleConf
        putStrLn "#######################################################\n"
        --putStrLn $ show $ getDepListReplacement basicPRG exampleConf [MC.L2EtherValidUnicast]
        --putStrLn $ show $ getDepReplacement basicPRG exampleConf MC.L2EtherValidCRC -- MC.L2EtherValidUnicast
        putStrLn "#######################################################\n"
        putStrLn "#######################################################\n"
        putStrLn "#######################################################\n"

    where
        basicPRG = getE1kBasicPRG
        exampleConf = getExampleConf

getExampleConf :: [MConf.Configuration]
getExampleConf = [
            MConf.Always
--            , MConf.IPv4Checksum
            , MConf.EthernetChecksum True
            , MConf.UDPChecksum True
            , MConf.QueueConf testQueue
            , MConf.FilterConf testFilter testQueue
         ]
         where
            testQueue = MC.Queue 4 4
            testFilter = MC.Filter MC.TCP  192168002001 192168003001   4444 80

getE1kPRGConfTest :: [MG.Gnode MC.Computation]
--getE1kPRGConfTest ::  IO()
getE1kPRGConfTest =  getE1kPRGConf exampleConf
    where
        exampleConf = getExampleConf

{-
 - Based on the given configuration, give the nodes which will be involved
 - in the computation.
 -
 -}
getE1kPRGConf :: [MConf.Configuration] -> [MG.Gnode MC.Computation]
--getE1kPRGConf confList = getE0kPRGGeneric basicPRG confList basicPRG
getE1kPRGConf confList = getE0kPRGGenericV2 basicPRG confList basicPRG
    where
        basicPRG = getE1kBasicPRG


{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kPRGOrig :: [MG.Gnode MC.Computation]
getE1kPRGOrig = [
        (MC.ClassifiedL2Ethernet, [])
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet])
        , (MC.L2EtherValidCRC, [MC.L2EtherValidLen])
        , (MC.L2EtherValidBroadcast, [MC.L2EtherValidCRC])
        , (MC.L2EtherValidMulticast, [MC.L2EtherValidCRC])
        , (MC.L2EtherValidUnicast, [MC.L2EtherValidCRC])
        , (MC.L2EtherValidDest, [MC.L2EtherValidBroadcast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidMulticast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidUnicast])
        , (MC.L2EtherValidType, [MC.L2EtherValidDest])
        , (MC.ClassifiedL3IPv4, [MC.L2EtherValidType])
        , (MC.L3IPv4ValidChecksum, [MC.ClassifiedL3IPv4])
        , (MC.L3IPv4ValidProtocol, [MC.L3IPv4ValidChecksum])
        , (MC.ClassifiedL3IPv6, [MC.L2EtherValidType])
        , (MC.L3IPv6ValidProtocol, [MC.ClassifiedL3IPv6])
        , (MC.ClassifiedL3, [MC.L3IPv4ValidProtocol])
        , (MC.ClassifiedL3, [MC.L3IPv6ValidProtocol])
        , (MC.ClassifiedL4UDP, [MC.ClassifiedL3]) -- UDP classification
        , (MC.ClassifiedL4TCP, [MC.ClassifiedL3]) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3]) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3]) -- UDP classification


        , (MC.L4ReadyToClassify, [MC.ClassifiedL4TCP])
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4UDP])
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4ICMP])
        , (MC.L4ReadyToClassify, [MC.UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [MC.L4ReadyToClassify])



        -- some exaple filters
        , (http_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (telnet_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (tftp_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (q4, [http_flow])
        , (q3, [telnet_flow])
        , (q1, [tftp_flow])
        , (q0, [generic_filter])
        ]
    where
        q0 = MC.ToQueue MC.getDefaultQueue
        q1 = (MC.ToQueue (MC.Queue 1 1))
 --       q2 = (MC.ToQueue (MC.Queue 2 2))
        q3 = (MC.ToQueue (MC.Queue 3 4))
        q4 = (MC.ToQueue (MC.Queue 3 4))

        -- sample http server filter
        generic_filter = MC.getDefaultFilter
        http_flow = (MC.IsFlow (MC.Filter MC.TCP MC.anyIP (MC.toIP "192.168.2.4")
            MC.anyPort 80))
        telnet_flow = (MC.IsFlow (MC.Filter MC.TCP (MC.toIP "255.255.255.255")
            (MC.toIP "192.168.2.4") MC.anyPort 80))
        tftp_flow = (MC.IsFlow (MC.Filter MC.UDP (MC.toIP "201.3.2.5")
            (MC.toIP "192.168.2.4") MC.anyPort 69))


{-
 - Find all dependency nodes for given node.
 - Assumption: there are only OR nodes
 -}
getAllORDeps :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
        -> MC.Computation -> [MC.Computation]
getAllORDeps basicPRG x = DL.map (\(_, b, _) ->
                if length b == 1 then DL.head b else error "PRG contains AND node")
                $ DL.filter (\(aa, _, _) -> aa == x ) basicPRG


{-
 - Find a replacement list for given dependency list
 -}
getDepListReplacement :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
                -> [MConf.Configuration] -> [MC.Computation] -> [MC.Computation]
getDepListReplacement _ _ [] = []
getDepListReplacement basicPRG conflist (x:xs) =
           {- DT.trace ("## replacing "
           ++ show x ++ " with " ++ show replacement ++ " ####")
           -}
           replacement ++ getDepListReplacement basicPRG conflist xs
    where
        replacement = getDepReplacement basicPRG conflist x


{-
 - Find a replacement node for given dependency nodej
 -}
getDepReplacement :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
                -> [MConf.Configuration] -> MC.Computation -> [MC.Computation]
getDepReplacement basicPRG conflist x
    | x `elem` activeNodes  =
        {-
        DT.trace (".... in active nodes " ++ show x ++ "" )
        -}
        [x]
    | otherwise =
        {-
        DT.trace (".... node " ++ show x ++ " is Not in active nodes "
        ++ show activeNodes ++ " so replacing with " ++ show depReps
        ++ " INPUT {" ++ show basicPRG ++ "}.... {" ++ show conflist ++ "}"  )
        -}
        depReps -- for every dependency of x, get a replacement
    where
        activeNodes = getActiveNodesForConf basicPRG conflist
        depReps = getDepListReplacement basicPRG conflist $
            getAllORDeps basicPRG x


isGivenNodeActive :: (MC.Computation, [MC.Computation], MConf.Configuration)
                -> [MConf.Configuration] -> [MC.Computation]
isGivenNodeActive (comp, _, conf) conflist = if conf `elem` conflist then [comp]
                            else []

getActiveNodesForConf :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MConf.Configuration] -> [MC.Computation]
getActiveNodesForConf [] _ = []
getActiveNodesForConf (x:xs) conf = xlist ++ getActiveNodesForConf xs conf
    where
        xlist = isGivenNodeActive x conf


{-
 - Get normal graph from given basicPRG, configuration list, and currently
 - processed nodes
 -}
getE0kPRGGeneric ::  [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MConf.Configuration]
            -> [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MG.Gnode MC.Computation]
getE0kPRGGeneric _ _ [] = []
getE0kPRGGeneric basicPRG confList (x:xs) =
            {-
            DT.trace (
            "getE0kPRGGeneric 1: replaced {{" ++ show x ++ " }} with {{ "
            ++ show replacedNode ++ "}}")
            -}
            replacedNode ++ getE0kPRGGeneric basicPRG confList xs
    where
        activeNodes = getActiveNodesForConf basicPRG confList
        (node, deps, _) = x
        replacementDeps = getDepListReplacement basicPRG confList deps
        replacedNode
            | node `elem` activeNodes = [(node, replacementDeps)]
            | otherwise = []


getE0kPRGGenericV2 ::  [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MConf.Configuration]
            -> [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MG.Gnode MC.Computation]
getE0kPRGGenericV2 basicPRG confList changingList =  additionalEdges ++
                getE0kPRGGeneric basicPRG confList changingList
    where
        additionalEdges = MConf.genAllDependencies confList



getE1kBasicPRGDummy :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
getE1kBasicPRGDummy = [
        (MC.ClassifiedL2Ethernet, [], MConf.Always)
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet], MConf.Always)
        ]



{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kPRG :: [MG.Gnode MC.Computation]
getE1kPRG = [
        (MC.ClassifiedL2Ethernet, [])
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet])

        , (etherChecksum, [MC.L2EtherValidLen])

        , (MC.L2EtherValidBroadcast, [etherChecksum])
        , (MC.L2EtherValidMulticast, [etherChecksum])
        , (MC.L2EtherValidUnicast, [etherChecksum])
        , (MC.L2EtherValidDest, [MC.L2EtherValidBroadcast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidMulticast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidUnicast])
        , (MC.L2EtherValidType, [MC.L2EtherValidDest])
        , (MC.ClassifiedL3IPv4, [MC.L2EtherValidType])
        , (ipv4Checksum, [MC.ClassifiedL3IPv4])
        , (MC.L3IPv4ValidProtocol, [ipv4Checksum])
        , (MC.ClassifiedL3IPv6, [MC.L2EtherValidType])
        , (MC.L3IPv6ValidProtocol, [MC.ClassifiedL3IPv6])
        , (MC.ClassifiedL3, [MC.L3IPv4ValidProtocol])
        , (MC.ClassifiedL3, [MC.L3IPv6ValidProtocol])
        , (MC.ClassifiedL4UDP, [MC.ClassifiedL3]) -- UDP classification
        , (MC.ClassifiedL4TCP, [MC.ClassifiedL3]) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3]) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3]) -- UDP classification


        , (MC.L4ReadyToClassify, [MC.ClassifiedL4TCP])
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4UDP])
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4ICMP])
        , (MC.L4ReadyToClassify, [MC.UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [MC.L4ReadyToClassify])



        -- some exaple filters
        , (http_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (telnet_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (tftp_flow, [MC.L4ReadyToClassify]) -- sample filter
        , (q4, [http_flow])
        , (q3, [telnet_flow])
        , (q1, [tftp_flow])
        , (q0, [generic_filter])
        ]
    where
        etherChecksum = (MConf.IsConfSet  (MConf.ConfDecision
            (MConf.EthernetChecksum True) MC.L2EtherValidCRC MConf.UnConfigured))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
            (MConf.IPv4Checksum True) MC.L3IPv4ValidChecksum MConf.UnConfigured))


--        aq1 = MC.Queue 1 1
--        q1 = (MConf.IsConfSet  (MConf.ConfDecision
--            (MConf.QueueConf aq1) MC.L3IPv4ValidChecksum MConf.UnConfigured))

        q0 = MC.ToQueue MC.getDefaultQueue
        q1 = (MC.ToQueue (MC.Queue 1 1))
 --       q2 = (MC.ToQueue (MC.Queue 2 2))
        q3 = (MC.ToQueue (MC.Queue 3 4))
        q4 = (MC.ToQueue (MC.Queue 3 4))

        -- sample http server filter
        generic_filter = MC.getDefaultFilter
        http_flow = (MC.IsFlow (MC.Filter MC.TCP MC.anyIP (MC.toIP "192.168.2.4")
            MC.anyPort 80))
        telnet_flow = (MC.IsFlow (MC.Filter MC.TCP  (MC.toIP "192.168.2.4")
            (MC.toIP "192.168.2.1") MC.anyPort 23))
        tftp_flow = (MC.IsFlow (MC.Filter MC.UDP (MC.toIP "24.35.17.5") (MC.toIP "192.168.2.4") MC.anyPort 69))


{-
 - Returns list of computations which can happen in the E0k NIC
 - and their dependencies.
 -}
getE1kBasicPRG :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
getE1kBasicPRG = [
        (MC.ClassifiedL2Ethernet, [], MConf.Always)
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet], MConf.Always)

        , (MC.L2EtherValidCRC, [MC.L2EtherValidLen], (MConf.EthernetChecksum True))

        , (MC.L2EtherValidBroadcast, [MC.L2EtherValidCRC], MConf.Always)
        , (MC.L2EtherValidMulticast, [MC.L2EtherValidCRC], MConf.Always)
        , (MC.L2EtherValidUnicast, [MC.L2EtherValidCRC], MConf.Always)

        , (MC.L2EtherValidDest, [MC.L2EtherValidBroadcast], MConf.Always)
        , (MC.L2EtherValidDest, [MC.L2EtherValidMulticast], MConf.Always)
        , (MC.L2EtherValidDest, [MC.L2EtherValidUnicast], MConf.Always)
        , (MC.L2EtherValidType, [MC.L2EtherValidDest], MConf.Always)
        , (MC.ClassifiedL3IPv4, [MC.L2EtherValidType], MConf.Always)
        , (MC.L3IPv4ValidChecksum, [MC.ClassifiedL3IPv4], (MConf.IPv4Checksum True))

        , (MC.L3IPv4ValidProtocol, [MC.L3IPv4ValidChecksum], MConf.Always)

        , (MC.ClassifiedL3IPv6, [MC.L2EtherValidType], MConf.Always)
        , (MC.L3IPv6ValidProtocol, [MC.ClassifiedL3IPv6], MConf.Always)
        , (MC.ClassifiedL3, [MC.L3IPv4ValidProtocol], MConf.Always)
        , (MC.ClassifiedL3, [MC.L3IPv6ValidProtocol], MConf.Always)
        , (MC.ClassifiedL4UDP, [MC.ClassifiedL3], MConf.Always) -- UDP classification
        , (MC.ClassifiedL4TCP, [MC.ClassifiedL3], MConf.Always) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3], MConf.Always) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3], MConf.Always) -- UDP classification

        , (MC.L4ReadyToClassify, [MC.ClassifiedL4TCP], MConf.Always)
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4UDP], MConf.Always)
        , (MC.L4ReadyToClassify, [MC.ClassifiedL4ICMP], MConf.Always)
        , (MC.L4ReadyToClassify, [MC.UnclasifiedL4], MConf.Always)

        -- Filtering the packet
        , (generic_filter, [MC.L4ReadyToClassify], MConf.Always)
        , (q0, [generic_filter], MConf.Always)

        ]
    where
        q0 = MC.ToQueue MC.getDefaultQueue
        generic_filter = MC.getDefaultFilter

{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph getE1kPRG

