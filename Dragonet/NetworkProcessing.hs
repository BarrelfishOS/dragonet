#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - All the possible computations which can happen in the Network processing.
 -
 -
 -}

--module Main (
module NetworkProcessing (
    getNetworkDependency
    , getNetworkDependencySmall
) where

import qualified Data.List as DL
--import qualified Data.Set as Set


import qualified NetBasics as NB
import qualified Operations as OP


{-
addToTrue :: OP.GNode -> OP.Node -> OP.Node
addToTrue nbig nsmall = case nbig of
        ()
-}

getEthernetProcessingLPG :: OP.NodeEdges -> OP.NodeEdges -> OP.Node -> OP.Node
getEthernetProcessingLPG classified verified dropnode = etherClassified
     where
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet "PF"
        (OP.BinaryNode (
            [etherValidType, etherValidSrc, etherValidMulticast,
            etherValidBroadcast, etherValidUnicast, etherChecksumCalc],
            [dropnode]))  []
    etherValidSrc = OP.getDecNode NB.L2EtherValidSrc "PF"
        (OP.BinaryNode (
            [opANDverifiedEthernet],
            [opANDverifiedEthernet]))  []
    etherValidMulticast = OP.getDecNode NB.L2EtherValidMulticast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest]))  []
    etherValidBroadcast = OP.getDecNode NB.L2EtherValidBroadcast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest])) []
    etherValidUnicast = OP.getDecNode NB.L2EtherValidUnicast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest])) []

    etherChecksumCalc = OP.getDecNode NB.L2EtherValidCRC "PF"
        (OP.BinaryNode (
            [opANDverifiedEthernet],
            [opANDverifiedEthernet])) []

    opORL2validDest = OP.getOperatorNode NB.OR NB.L2EtherValidDest ""
        (OP.BinaryNode (
            [opANDverifiedEthernet],
            [opANDverifiedEthernet]))

    opANDverifiedEthernet = OP.getOperatorNode NB.AND NB.VerifiedL2 "" verified

    etherValidType = OP.getDecNode NB.L2EtherValidType "PF" classified  []


getNetworkDependencySmall :: OP.Node
getNetworkDependencySmall = etherClassified
    where
    dropnode = OP.getDropNode

    l2Verified = OP.BinaryNode ([opANDL5ToKernel],
        [dropnode])
    l2Classified = OP.BinaryNode([queue0], [dropnode])

    etherClassified = getEthernetProcessingLPG l2Classified l2Verified dropnode

    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue
    queue0 = OP.getDecNode (NB.ToQueue q0) ""
        (OP.BinaryNode (
            [opANDL5ToKernel],
            [opANDL5ToKernel]) ) []

    opANDL5ToKernel = OP.getOperatorNode NB.AND NB.ToDefaultKernelProcessing ""
        (OP.BinaryNode ([], []))




    -- ClassifiedL3
getNetworkDependency :: OP.Node
getNetworkDependency = etherClassified
    where


    dropnode = OP.getDropNode

    l2Verified = OP.BinaryNode ([verifiedL3IPv4, verifiedL3IPv6],
        [verifiedL3IPv4, verifiedL3IPv6])
    l2Classified = OP.BinaryNode([classifyIPv4, classifyIPv6],
            [dropnode])

    etherClassified = getEthernetProcessingLPG l2Classified l2Verified dropnode

    toVerifyIPv4 = [NB.L3IPv4ValidVersion, NB.L3IPv4ValidLength,
        NB.L3IPv4ValidTTL, NB.L3IPv4ValidChecksum, NB.L3IPv4ValidSrc,
        NB.L3IPv4ValidDest, NB.L3IPv4ValidReassembly]

    toVerifyIPv6 = [NB.L3IPv6ValidVersion, NB.L3IPv6ValidLength,
        NB.L3IPv6ValidHops, NB.L3IPv6ValidSrc, NB.L3IPv6ValidSrc,
        NB.L3IPv6ValidDest]

    toClassifiedL3 =  (OP.BinaryNode (
            [classifiedL3],
            [classifiedL3]))

    toL3Verified = (OP.BinaryNode (
            [verifiedL3],
            [verifiedL3]))

    (classifyIPv4, verifiedL3IPv4) = getProtoProcessing NB.ClassifiedL3IPv4
        (NB.L3IPv4ValidProtocol, toClassifiedL3) NB.VerifiedL3IPv4
        toL3Verified dropnode toVerifyIPv4

    (classifyIPv6, verifiedL3IPv6) = getProtoProcessing NB.ClassifiedL3IPv6
        (NB.L3IPv6ValidProtocol, toClassifiedL3) NB.VerifiedL3IPv6
        toL3Verified dropnode toVerifyIPv6

    classifiedL3 = OP.getOperatorNode NB.OR NB.ClassifiedL3 ""
        (OP.BinaryNode (
            [classifiedUDP, classifiedTCP], []))

    verifiedL3 = OP.getOperatorNode NB.OR NB.VerifiedL3 ""
        (OP.BinaryNode (
            [verifidTCP, verifidUDP],
            [verifidTCP, verifidUDP]))


    toVerifyUDP = [NB.L4UDPValidSrc, NB.L4UDPValidDest, NB.L4UDPValidLength,
           NB.L4UDPValidChecksum]

    toVerifyTCP = [NB.L4TCPValidState,  NB.L4TCPValidOffset
        , NB.L4TCPValidUrgent, NB.L4TCPValidUrgent, NB.L4TCPValidWindow
        , NB.L4TCPValidFlags, NB.L4TCPValidFin, NB.L4TCPValidSyn
        , NB.L4TCPValidAck, NB.L4TCPValidAckNo, NB.L4TCPValidSequence
        , NB.L4TCPValidSrc, NB.L4TCPValidDest, NB.L4TCPValidLength
        , NB.L4TCPValidChecksum]


    (classifiedUDP, verifidUDP) = getProtoProcessingV2 NB.ClassifiedL4UDP
        toL4readyToClassify NB.VerifiedL4UDP toL4Verified toVerifyUDP

    (classifiedTCP, verifidTCP) = getProtoProcessingV2 NB.ClassifiedL4TCP
        toL4readyToClassify NB.VerifiedL4TCP toL4Verified toVerifyTCP

    opORL4readyToClassify = OP.getOperatorNode NB.OR NB.ClassifiedL4 ""
        (OP.BinaryNode (
            [queue0],
            [dropnode]))

    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue
    queue0 = OP.getDecNode (NB.ToQueue q0) ""
        (OP.BinaryNode (
            [opANDL5ToKernel],
            [opANDL5ToKernel]) ) []

    toL4readyToClassify = (OP.BinaryNode (
            [opORL4readyToClassify],
            [opORL4readyToClassify]))

    toL4Verified = (OP.BinaryNode (
            [opORL4Verified],
            [opORL4Verified]))

    opANDL5ToKernel = OP.getOperatorNode NB.AND NB.ToDefaultKernelProcessing ""
        (OP.BinaryNode ([], []))

    opORL4Verified = OP.getOperatorNode NB.OR NB.VerifiedL4 ""
        (OP.BinaryNode (
            [opANDL5ToKernel],
            [opANDL5ToKernel]))


{-
 - For given protocol with lot of processing for verifying protocol
 - and separate exit for just classifying protocol, this will capture
 - the notation
 -}
getProtoProcessing :: NB.NetOperation -> (NB.NetOperation, OP.NodeEdges) ->
    NB.NetOperation -> OP.NodeEdges -> OP.Node -> [NB.NetOperation] -> (OP.Node, OP.Node)
getProtoProcessing startNodeLabel (classifiedLabel, classifiedForward)
   verifiedLabel verifiedForward dropnode toVerify = (protoClassified, opANDverified)
    where

    opANDverified = OP.getOperatorNode NB.AND verifiedLabel "" verifiedForward
    toANDop =  OP.BinaryNode ([opANDverified], [opANDverified])
    trueList = DL.map (\ x -> OP.getDecNode x "PF" toANDop []) toVerify

    validProto = OP.getDecNode classifiedLabel "PF" classifiedForward []
    protoClassified = OP.getDecNode startNodeLabel "PF"
        (OP.BinaryNode (trueList ++ [validProto], [dropnode])) []


getProtoProcessingV2 :: NB.NetOperation -> OP.NodeEdges -> NB.NetOperation ->
    OP.NodeEdges -> [NB.NetOperation] -> (OP.Node, OP.Node)
getProtoProcessingV2 startNodeLabel classifiedForward
    verifiedLabel verifiedForward toVerify = (protoClassified, opANDverified)
    where
    opANDverified = OP.getOperatorNode NB.AND verifiedLabel "" verifiedForward
    toANDop =  OP.BinaryNode ([opANDverified], [opANDverified])
    trueList = DL.map (\ x -> OP.getDecNode x "PF" toANDop  []) toVerify

    (tlist, flist) = case classifiedForward of
        OP.BinaryNode (a, b) ->  (a, b)
        OP.NaryNode _ -> error "NetworkProcessing.getProtoProcessingV2: binary node merging with Nary node"

    protoClassified = OP.getDecNode startNodeLabel "PF"
        (OP.BinaryNode (trueList ++ tlist, flist)) []






