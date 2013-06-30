#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Records wiring of E10k to represent PRG graph
 -
 -}

module E10kPRG(
    getE1kPRGminimal
    , getE1kPRG
--    , getTestcaseConfiguration
) where

import qualified Data.List as DL
--import qualified Data.Set as Set

import qualified NetBasics as NB
import qualified Operations as OP


-- Get simplest possible datatype for E10k (for testing purpose only)
getE1kPRGminimal :: OP.Node
getE1kPRGminimal = etherClassified
    where
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet ""
        (OP.BinaryNode (
            [],
            []))

-- Get the datatype for E10k
getE1kPRG :: OP.Node
getE1kPRG = etherClassified
    where

    dropnode = OP.getDropNode
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet ""
        (OP.BinaryNode (
            [etherValidLen],
            [dropnode]))

    etherValidLen = OP.getDecNode NB.L2EtherValidLen ""
        (OP.BinaryNode (
            [etherCRCconf],
            [dropnode]))

    etherCRCconf = OP.getConfNode "IsCRCCalcON" ""
        (OP.BinaryNode (
            [validCRC],
            l2AddrCheckList))

    validCRC = OP.getDecNode NB.L2EtherValidCRC "PF"
        (OP.BinaryNode (
            l2AddrCheckList,
            [dropnode]))

    validAddrOptions = [NB.L2EtherValidBroadcast, NB.L2EtherValidMulticast,
        NB.L2EtherValidUnicast]

    toORop =  OP.BinaryNode ([opORL2EtherValidDest], [opORL2EtherValidDest])
    l2AddrCheckList = DL.map (\ x -> OP.getDecNode x "PF" toORop) validAddrOptions

    opORL2EtherValidDest = OP.getOperatorNode NB.OR "L2ValidDest"
        (OP.BinaryNode (
            [etherValidType],
            [dropnode]))

    etherValidType = OP.getDecNode NB.L2EtherValidType "PF"
        (OP.BinaryNode (
            [classifiedL3IPv4, classifiedL3IPv6],
            [dropnode]))

    classifiedL3IPv4 = OP.getDecNode NB.ClassifiedL3IPv4 "PF"
        (OP.BinaryNode (
            [ipv4ChecksumConf],
            [dropnode]))

    ipv4ChecksumConf = OP.getConfNode "IsIPv4ChecksumON" ""
        (OP.BinaryNode (
            [ipv4Checksum],
            [l3IPv4ValidProto]))

    ipv4Checksum = OP.getDecNode NB.L3IPv4ValidChecksum "PF"
        (OP.BinaryNode (
            [l3IPv4ValidProto],
            [dropnode]))

    l3IPv4ValidProto = OP.getDecNode NB.L3IPv4ValidProtocol "PF"
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))

    classifiedL3IPv6 = OP.getDecNode NB.ClassifiedL3IPv6 "PF"
        (OP.BinaryNode (
            [l3IPv6ValidProto],
            [dropnode]))

    l3IPv6ValidProto = OP.getDecNode NB.L3IPv6ValidProtocol "PF"
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))

    opORclassifiedL3 = OP.getOperatorNode NB.OR "L3Classified"
        (OP.BinaryNode (
            [classifiedUDP, classifiedTCP, classifiedICMP, unclassfiedL4],
            [dropnode]))


    classifiedUDP = OP.getDecNode NB.ClassifiedL4UDP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    classifiedTCP = OP.getDecNode NB.ClassifiedL4TCP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    classifiedICMP = OP.getDecNode NB.ClassifiedL4ICMP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    unclassfiedL4 = OP.getDecNode NB.UnclasifiedL4 "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    opORl4ReadyToClassify = OP.getOperatorNode NB.OR "L4Classified"
        (OP.BinaryNode (
            [genFilter, filter1, filter2, filter3],
            [dropnode]))

    genFilter = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 0)) "PF"
        (OP.BinaryNode (
            [defaultQueue],
            []))

    defaultQueue = OP.getDecNode (NB.ToQueue NB.getDefaultQueue) "PF"
        (OP.BinaryNode (
            [],
            []))

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    queue1 = OP.getDecNode (NB.ToQueue q1) "PF"
        (OP.BinaryNode (
            [],
            []))

    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    queue2 = OP.getDecNode (NB.ToQueue q2) "PF"
        (OP.BinaryNode (
            [],
            []))

    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    queue3 = OP.getDecNode (NB.ToQueue q3) "PF"
        (OP.BinaryNode (
            [],
            []))


    filter1 = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 1)) "PF"
        (OP.BinaryNode (
            [queue1],
            []))

    filter2 = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 2)) "PF"
        (OP.BinaryNode (
            [queue2],
            []))

    filter3 = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 3)) "PF"
        (OP.BinaryNode (
            [queue3],
            []))















