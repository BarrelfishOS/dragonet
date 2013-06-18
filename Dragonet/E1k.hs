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
    , main
) where


import qualified MyGraph as MG
import qualified Computations as MC
import qualified Configurations as MConf
import qualified Data.List as DL



--getE1kPRGConfTest :: [MG.Gnode MC.Computation]
getE1kPRGConfTest ::  IO()
getE1kPRGConfTest =  putStrLn $ show  $ getE1kPRGConf exampleConf
    where
        exampleConf = [
            MConf.Always
            , MConf.EthernetChecksum
            , MConf.UDPChecksum
         ]



{-
 - Based on the given configuration, give the nodes which will be involved
 - in the computation.
 -
 -}
getE1kPRGConf :: [MConf.Configuration] -> [MG.Gnode MC.Computation]
getE1kPRGConf confList = getE0kPRGGeneric basicPRG confList
    where
        basicPRG = getE1kBasicPRG

{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kPRG :: [MG.Gnode MC.Computation]
getE1kPRG = [
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
        q0 = MC.getDefaultQueue
        q1 = (MC.CopyToQueue "1")
        q2 = (MC.CopyToQueue "2")
        q3 = (MC.CopyToQueue "3")
        q4 = (MC.CopyToQueue "4")

        -- sample http server filter
        generic_filter = MC.getDefaultFilter
        http_flow = (MC.IsFlow (MC.Filter "TCP" "ANY" "192.168.2.4" "ANY" "80"))
        telnet_flow = (MC.IsFlow (MC.Filter "TCP" "255.255.255.255" "192.168.2.4" "ANY" "80"))
        tftp_flow = (MC.IsFlow (MC.Filter "UDP" "254.255.255.255" "192.168.2.4" "ANY" "69"))


{-
 - Find all dependency nodes for given node.
 - Assumption: there are only OR nodes
 -}
getAllORDeps :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
        -> MC.Computation -> [MC.Computation]
getAllORDeps basicPRG x = DL.map (\(a, b, c) ->
                if length b == 1 then DL.head b else error "PRG contains AND node")
                $ DL.filter (\(a, b, c) -> a == x ) basicPRG


{-
 - Find a replacement list for given dependency list
 -}
getDepListReplacement :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
                -> [MConf.Configuration] -> [MC.Computation] -> [MC.Computation]
getDepListReplacement basicPRG conflist [] = []
getDepListReplacement basicPRG conflist (x:xs) = replacement ++ getDepListReplacement basicPRG conflist xs
    where
        replacement = getDepReplacement basicPRG conflist x

{-
 - Find a replacement node for given dependency nodej
 -}
getDepReplacement :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
                -> [MConf.Configuration] -> MC.Computation -> [MC.Computation]
getDepReplacement basicPRG conflist x
    | x `elem` getActiveNodesForConf basicPRG conflist = [x]
    | otherwise = depReps -- for every dependency of x, get a replacement
    where
        depReps = getDepListReplacement basicPRG conflist $
            getAllORDeps basicPRG x


isGivenNodeActive :: (MC.Computation, [MC.Computation], MConf.Configuration)
                -> [MConf.Configuration] -> [MC.Computation]
isGivenNodeActive (comp, deps, conf) conflist = if conf `elem` conflist then [comp]
                            else []

getActiveNodesForConf :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MConf.Configuration] -> [MC.Computation]
getActiveNodesForConf [] conf = []
getActiveNodesForConf (x:xs)  conf = xlist  ++ getActiveNodesForConf xs conf
    where
        xlist = isGivenNodeActive x conf


{-
 - Get normal graph from given basicPRG and configuration list
 -}
getE0kPRGGeneric ::  [(MC.Computation, [MC.Computation], MConf.Configuration)]
            -> [MConf.Configuration] -> [MG.Gnode MC.Computation]
getE0kPRGGeneric [] confList = []
getE0kPRGGeneric (x:xs) confList = replacedNode ++ getE0kPRGGeneric xs confList
    where
        activeNodes = getActiveNodesForConf (x:xs) confList
        (node, deps, cconf) = x
        replacementDeps = getDepListReplacement (x:xs) confList deps
        replacedNode
            | node `elem` activeNodes = [(node, replacementDeps)]
            | otherwise = []

{-
 - Returns list of computations which can happen in the E0k NIC
 - and their dependencies.
 -}
getE1kBasicPRG :: [(MC.Computation, [MC.Computation], MConf.Configuration)]
getE1kBasicPRG = [
        (MC.ClassifiedL2Ethernet, [], MConf.Always)
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet], MConf.Always)

        , (MC.L2EtherValidCRC, [MC.L2EtherValidLen], MConf.EthernetChecksum)

        , (MC.L2EtherValidBroadcast, [MC.L2EtherValidCRC], MConf.Always)
        , (MC.L2EtherValidMulticast, [MC.L2EtherValidCRC], MConf.Always)
        , (MC.L2EtherValidUnicast, [MC.L2EtherValidCRC], MConf.Always)

        , (MC.L2EtherValidDest, [MC.L2EtherValidBroadcast], MConf.Always)
        , (MC.L2EtherValidDest, [MC.L2EtherValidMulticast], MConf.Always)
        , (MC.L2EtherValidDest, [MC.L2EtherValidUnicast], MConf.Always)
        , (MC.L2EtherValidType, [MC.L2EtherValidDest], MConf.Always)
        , (MC.ClassifiedL3IPv4, [MC.L2EtherValidType], MConf.Always)
        , (MC.L3IPv4ValidChecksum, [MC.ClassifiedL3IPv4], MConf.Always)

        , (MC.L3IPv4ValidProtocol, [MC.L3IPv4ValidChecksum], MConf.IPv4Checksum)

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


        -- some exaple filters
        , (http_flow, [MC.L4ReadyToClassify], MConf.Always) -- sample filter
        , (telnet_flow, [MC.L4ReadyToClassify], MConf.Always) -- sample filter
        , (tftp_flow, [MC.L4ReadyToClassify], MConf.Always) -- sample filter
        , (q4, [http_flow], MConf.Always)
        , (q3, [telnet_flow], MConf.Always)
        , (q1, [tftp_flow], MConf.Always)
        , (q0, [generic_filter], MConf.Always)
        ]
    where
        q0 = MC.getDefaultQueue
        q1 = (MC.CopyToQueue "1")
        q2 = (MC.CopyToQueue "2")
        q3 = (MC.CopyToQueue "3")
        q4 = (MC.CopyToQueue "4")

        -- sample http server filter
        generic_filter = MC.getDefaultFilter
        http_flow = (MC.IsFlow (MC.Filter "TCP" "ANY" "192.168.2.4" "ANY" "80"))
        telnet_flow = (MC.IsFlow (MC.Filter "TCP" "255.255.255.255" "192.168.2.4" "ANY" "80"))
        tftp_flow = (MC.IsFlow (MC.Filter "UDP" "255.255.255.255" "192.168.2.4" "ANY" "69"))

{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph getE1kPRG

