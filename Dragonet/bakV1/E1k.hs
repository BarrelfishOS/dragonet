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
    , getE1kPRGminimal
    , getE1kPRGConfTest
    , getTestcaseConfiguration
    , main
) where


import qualified Computations as MC
import qualified Computations as MConf
import qualified PRG as PRG


getE1kPRGConfTest ::  IO()
getE1kPRGConfTest =
    do

        putStrLn "#################  basicPRG  ###########################\n"
        putStrLn $ show $ getE1kPRG
        putStrLn "#################  Conf ###########################\n"
        putStrLn $ show $ getTestcaseConfiguration
        putStrLn "#######################################################\n"

getTestcaseConfiguration :: [MC.Computation]
getTestcaseConfiguration = [
            MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision MC.L2EtherValidCRC MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision  MC.L3IPv4ValidChecksum MC.SKIP)
            , MC.addMode vf1 $ MC.IsConfSet (MC.ConfDecision tcpSegmentation MC.SKIP)
            , MC.addMode vf2 $ MC.IsConfSet (MC.ConfDecision tcpSegmentation MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.ToQueue testQueue) MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.IsFlow f1) MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.IsPartial tcpChecksumPartial) MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.IsPartial hf1) MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.L2Virtualization) MC.ENABLE)
            , MC.addMode MC.genericModeTag $ MC.IsConfSet (MC.ConfDecision (MC.L2NOVirtualization) MC.STOP)
            ]
        where
        vf1 = "VF1"
        vf2 = "VF2"
        testQueue = MC.Queue 4 1 MC.getDefaultBasicQueue
        f1 = MC.Filter 1 MC.TCP MC.anyIP (MC.toIP "192.168.2.4") MC.anyPort 80
        tcpChecksumPartial = MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = MC.PartialComp (MC.IsFlow f4) hf1EmulatedPart
        hf1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter f4))

        f4 = MC.Filter 4 MC.TCP (MC.toIP "192.168.2.4")
            (MC.toIP "192.168.2.1") MC.anyPort 23

        tcpSegmentation = (MC.L4TCPSegmentation MC.getDefaultBasicQueue)


getE1kVFminimum :: MC.ModeType -> [MC.Gnode MC.Computation]
getE1kVFminimum mName = [
        (MC.ClassifiedL2Ethernet, [])
        , (etherChecksum, [MC.ClassifiedL2Ethernet])
        , (MC.L2EtherValidBroadcast, [etherChecksum])

        -- For buffer descriptor registration
        , (MC.ReqBufDescregister, [])
        , (MC.VerifyBufDesc, [MC.ReqBufDescregister])
        , (addBufDesQ1, [MC.VerifyBufDesc])
        , (addBufDesQ0, [MC.VerifyBufDesc])

        , (generic_filter, [MC.L2EtherValidBroadcast])
        , (flow1, [MC.L2EtherValidBroadcast])
        , (toQ0, [generic_filter, addBufDesQ0])
        , (toQ1, [flow1, addBufDesQ1]) -- Added just to make show the queues at proper place
        ]
    where
        etherChecksum = (MConf.IsConfSet
            (MConf.ConfDecision MC.L2EtherValidCRC MConf.Undecided))
        toQ0 = MC.ToQueue q0
        toQ1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue q1)  MConf.Undecided))

        q0 =  MC.getDefaultQueue
        q1 = MC.Queue 1 1 MC.getDefaultBasicQueue


        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)
        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.Undecided))

        addBufDesQ1 = MC.AddBufDescToQueue q1
        addBufDesQ0 = MC.AddBufDescToQueue q0



{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kVF :: MC.ModeType -> [MC.Gnode MC.Computation]
getE1kVF mName = [
        (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet])

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

        , (tcpChecksum, [MC.ClassifiedL3]) -- TCP checksum
        , (tcpSegmentation, [tcpChecksum]) -- TCP classification
        , (MC.ClassifiedL4TCP, [tcpSegmentation]) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3]) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3]) -- UDP classification


        , (l4ReadyToClassify, [MC.ClassifiedL4TCP])
        , (l4ReadyToClassify, [MC.ClassifiedL4UDP])
        , (l4ReadyToClassify, [MC.ClassifiedL4ICMP])
        , (l4ReadyToClassify, [MC.UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [l4ReadyToClassify])

        -- some exaple filters
        , (flow1, [l4ReadyToClassify]) -- sample filter
        , (flow2, [l4ReadyToClassify]) -- sample filter
        , (flow3, [l4ReadyToClassify]) -- sample filter
        , (hf1, [l4ReadyToClassify]) -- sample filter
        , (q4, [flow1]) -- Added just to make show the queues at proper place
        , (q3, [flow2]) -- Added just to make show the queues at proper place
        , (q1, [flow3]) -- Added just to make show the queues at proper place
        , (q3, [hf1]) -- Added just to make show the queues at proper place
        , (q0, [generic_filter])
        ]
    where

        etherChecksum = (MConf.IsConfSet
            (MConf.ConfDecision MC.L2EtherValidCRC MConf.Undecided))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L3IPv4ValidChecksum MConf.Undecided))

        tcpSegmentation = (MConf.IsConfSet (MConf.ConfDecision
            (MC.L4TCPSegmentation MC.getDefaultBasicQueue) MConf.Undecided))


        tcpChecksum = (MConf.IsConfSet (MConf.ConfDecision
            tcpChecksumPartial  MConf.Undecided))
        tcpChecksumPartial = MC.IsPartial (MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart)
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = (MConf.IsConfSet (MConf.ConfDecision
            partialhf1 MConf.Undecided))
        partialhf1  = MC.IsPartial (MC.PartialComp (MC.IsFlow ff1)
            ff1EmulatedPart)
        ff1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter ff1))
        ff1 = MC.getDefaultFitlerForID 4

        l4ReadyToClassify = MC.L4ReadyToClassify
        --l4ReadyToClassify = (MConf.IsConfSet  (MConf.ConfDecision
        --    MC.L4ReadyToClassify MConf.Undecided))


        q0 = MC.ToQueue MC.getDefaultQueue
        q1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 1 1 MC.getDefaultBasicQueue))  MConf.Undecided))

        q3 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 3 3 MC.getDefaultBasicQueue))  MConf.Undecided))

        q4 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 4 4 MC.getDefaultBasicQueue))  MConf.Undecided))

        -- sample http server filter
        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)

        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.Undecided))

        flow2 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 2)) MConf.Undecided))

        flow3 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 3)) MConf.Undecided))



{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}

getE1kPRG :: [MC.Gnode MC.Computation]
getE1kPRG = getE1kWithMultipleModes
-- getE1kPRG = getE1kPRGV1

getE1kPRGminimal:: [MC.Gnode MC.Computation]
getE1kPRGminimal = getE1kWithMultipleModesMinimal


getE1kPRGV1 :: [MC.Gnode MC.Computation]
getE1kPRGV1 = [
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

        , (tcpChecksum, [MC.ClassifiedL3]) -- TCP checksum
        , (MC.ClassifiedL4TCP, [tcpChecksum]) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3]) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3]) -- UDP classification


        , (l4ReadyToClassify, [MC.ClassifiedL4TCP])
        , (l4ReadyToClassify, [MC.ClassifiedL4UDP])
        , (l4ReadyToClassify, [MC.ClassifiedL4ICMP])
        , (l4ReadyToClassify, [MC.UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [l4ReadyToClassify])

        -- some exaple filters
        , (flow1, [l4ReadyToClassify]) -- sample filter
        , (flow2, [l4ReadyToClassify]) -- sample filter
        , (flow3, [l4ReadyToClassify]) -- sample filter
        , (hf1, [l4ReadyToClassify]) -- sample filter
        , (q4, [flow1]) -- Added just to make show the queues at proper place
        , (q3, [flow2]) -- Added just to make show the queues at proper place
        , (q1, [flow3]) -- Added just to make show the queues at proper place
        , (q3, [hf1]) -- Added just to make show the queues at proper place
        , (q0, [generic_filter])
        ]
    where
        etherChecksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L2EtherValidCRC MConf.Undecided))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L3IPv4ValidChecksum MConf.Undecided))


        tcpChecksum = (MConf.IsConfSet (MConf.ConfDecision
            tcpChecksumPartial  MConf.Undecided))
        tcpChecksumPartial = MC.IsPartial (MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart)
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = (MConf.IsConfSet (MConf.ConfDecision
            partialhf1 MConf.Undecided))
        partialhf1  = MC.IsPartial (MC.PartialComp (MC.IsFlow ff1)
            ff1EmulatedPart)
        ff1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter ff1))
        ff1 = MC.getDefaultFitlerForID 4


        l4ReadyToClassify = MC.L4ReadyToClassify
        --l4ReadyToClassify = (MConf.IsConfSet  (MConf.ConfDecision
        --    MC.L4ReadyToClassify MConf.Undecided))


        q0 = MC.ToQueue MC.getDefaultQueue
        q1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 1 1 MC.getDefaultBasicQueue))  MConf.Undecided))

        q3 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 3 3 MC.getDefaultBasicQueue))  MConf.Undecided))

        q4 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 4 4 MC.getDefaultBasicQueue))  MConf.Undecided))

        -- sample http server filter
        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)

        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.Undecided))

        flow2 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 2)) MConf.Undecided))

        flow3 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 3)) MConf.Undecided))


getE1kWithMultipleModes :: [MC.Gnode MC.Computation]
getE1kWithMultipleModes = bootstrap ++ pf' ++ vf1' ++ vf2'

    where
    pfTag = "PF"
    vf1Tag = "VF1"
    vf2Tag = "VF2"
    pf = MC.addModeToAll pfTag $ getE1kVF pfTag
    vf1 = MC.addModeToAll vf1Tag $ getE1kVF vf1Tag
    vf2 = MC.addModeToAll vf2Tag $ getE1kVF vf2Tag

    pf' = [((MC.InMode (MC.Mode pfTag MC.ClassifiedL2Ethernet)) , [pNICConf])] ++ pf
    vf1' = [((MC.InMode (MC.Mode vf1Tag MC.ClassifiedL2Ethernet)) , [vNICConf])] ++ vf1
    vf2' = [((MC.InMode (MC.Mode vf2Tag  MC.ClassifiedL2Ethernet)) , [vNICConf])] ++ vf2

    vNICConf = (MConf.IsConfSet (MConf.ConfDecision
           MC.L2Virtualization MConf.Undecided))

    pNICConf = (MConf.IsConfSet (MConf.ConfDecision
           MC.L2NOVirtualization MConf.Undecided))

    bootstrap = [
        (MC.L0Tag, [])
        , (pNICConf, [MC.L0Tag])
        , (vNICConf, [MC.L0Tag])
        ]



getE1kWithMultipleModesMinimal :: [MC.Gnode MC.Computation]
getE1kWithMultipleModesMinimal =  pf
    where
    pfTag = "PF"
    pf = MC.addModeToAll pfTag $  getE1kVFminimum  pfTag

    pf' = [((MC.InMode (MC.Mode pfTag MC.ClassifiedL2Ethernet)) , [pNICConf])] ++ pf

    pNICConf = (MConf.IsConfSet (MConf.ConfDecision
           MC.L2NOVirtualization MConf.Undecided))

    bootstrap = [
        (MC.L0Tag, [])
        , (pNICConf, [MC.L0Tag])
        ]


{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = show getE1kPRG

