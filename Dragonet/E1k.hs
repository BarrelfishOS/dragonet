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
    , main
) where


import qualified MyGraph as MG
import qualified Computations as MC


{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kPRG :: [MG.Gnode MC.Computation]
getE1kPRG = [
        (MC.ClassifiedL2Ethernet, [])
        , (MC.L2ValidLen, [MC.ClassifiedL2Ethernet])
        , (MC.L2ValidCRC, [MC.L2ValidLen])
        , (MC.L2ValidBroadcast, [MC.L2ValidCRC])
        , (MC.L2ValidMulticast, [MC.L2ValidCRC])
        , (MC.L2ValidUnicast, [MC.L2ValidCRC])
        , (MC.L2ValidDest, [MC.L2ValidBroadcast])
        , (MC.L2ValidDest, [MC.L2ValidMulticast])
        , (MC.L2ValidDest, [MC.L2ValidUnicast])
        , (MC.L2ValidType, [MC.L2ValidDest])
        , (MC.ClassifiedL3IPv4, [MC.L2ValidType])
        , (MC.L3IPv4ValidChecksum, [MC.ClassifiedL3IPv4])
        , (MC.L3IPv4ValidProtocol, [MC.L3IPv4ValidChecksum])
        , (MC.ClassifiedL3IPv6, [MC.L2ValidType])
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
        , (http_flow, [MC.ClassifiedL4TCP]) -- sample filter
        , (telnet_flow, [MC.ClassifiedL4TCP]) -- sample filter
        , (tftp_flow, [MC.ClassifiedL4UDP]) -- sample filter
        , (q4, [http_flow])
        , (q3, [telnet_flow])
        , (q1, [tftp_flow])

        , (q0, [MC.ClassifiedL4TCP]) --
        , (q0, [MC.ClassifiedL4UDP]) --
        , (q0, [MC.UnclasifiedL4]) --
        ]
    where
        q0 = MC.getDefaultQueue
        q1 = (MC.CopyToQueue "1")
        q2 = (MC.CopyToQueue "2")
        q3 = (MC.CopyToQueue "3")
        q4 = (MC.CopyToQueue "4")

        -- sample http server filter
        generic_filter = MC.getDefaultFilter
        http_flow = (MC.IsFlow (MC.Filter "TCP" "255.255.255.255" "192.168.2.4" "ANY" "80"))
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

