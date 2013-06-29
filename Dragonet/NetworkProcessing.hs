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
            etherValidBroadcast, etherValidUnicast],
            [dropnode]))
    etherValidSrc = OP.getDecNode NB.L2EtherValidSrc "PF"
        (OP.BinaryNode (
            [opANDverifiedEthernet],
            [opANDverifiedEthernet]))
    etherValidMulticast = OP.getDecNode NB.L2EtherValidMulticast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest]))
    etherValidBroadcast = OP.getDecNode NB.L2EtherValidBroadcast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest]))
    etherValidUnicast = OP.getDecNode NB.L2EtherValidUnicast "PF"
        (OP.BinaryNode (
            [opORL2validDest],
            [opORL2validDest]))

    opORL2validDest = OP.getOperatorNode NB.OR "L2ValidDestination"
        (OP.BinaryNode (
            [opANDverifiedEthernet],
            [opANDverifiedEthernet]))

    opANDverifiedEthernet = OP.getOperatorNode NB.AND "L2Verified" verified

    etherValidType = OP.getDecNode NB.L2EtherValidType "PF" classified


    -- ClassifiedL3
getNetworkDependency :: OP.Node
getNetworkDependency = etherClassified
    where


    dropnode = OP.getDropNode

    l2Verified = OP.BinaryNode ([], [dropnode])
    l2Classified = OP.BinaryNode([classifyIPv4, classifyIPv6],
            [dropnode])

    etherClassified = getEthernetProcessingLPG l2Classified l2Verified dropnode

    toVerifyIPv4 = [NB.L3IPv4ValidVersion, NB.L3IPv4ValidLength,
        NB.L3IPv4ValidTTL, NB.L3IPv4ValidChecksum, NB.L3IPv4ValidSrc,
        NB.L3IPv4ValidDest, NB.L3IPv4ValidReassembly]

    toVerifyIPv6 = [NB.L3IPv6ValidVersion, NB.L3IPv6ValidLength,
        NB.L3IPv6ValidHops, NB.L3IPv6ValidSrc, NB.L3IPv6ValidSrc,
        NB.L3IPv6ValidDest]


    classifiedL3 = OP.getOperatorNode NB.OR "IPL3Classify"
        (OP.BinaryNode (
            [],
            []))

    verifiedL3 = OP.getOperatorNode NB.OR "IPL3verify"
        (OP.BinaryNode (
            [],
            []))


    toClassifiedL3 =  (OP.BinaryNode (
            [classifiedL3],
            [classifiedL3]))

    toL3Verified = (OP.BinaryNode (
            [verifiedL3],
            [verifiedL3]))

    classifyIPv4 = getProtoProcessing NB.ClassifiedL3IPv4
        (NB.L3IPv4ValidProtocol, toClassifiedL3) toL3Verified dropnode
        toVerifyIPv4

    classifyIPv6 = getProtoProcessing NB.ClassifiedL3IPv6
        (NB.L3IPv6ValidProtocol, toClassifiedL3) toL3Verified dropnode
        toVerifyIPv6


{-
 - For given protocol with lot of processing for verifying protocol
 - and separate exit for just classifying protocol, this will capture
 - the notation
 -}
getProtoProcessing :: NB.NetOperation -> (NB.NetOperation, OP.NodeEdges) ->
    OP.NodeEdges -> OP.Node -> [NB.NetOperation] -> OP.Node
getProtoProcessing startNodeLabel (classifiedLabel, classifiedForward)
    verifiedForward dropnode toVerify = protoClassified
    where
    opANDverified = OP.getOperatorNode NB.AND "verified" verifiedForward
    toANDop =  OP.BinaryNode ([opANDverified], [opANDverified])
    trueList = DL.map (\ x -> OP.getDecNode x "PF" toANDop) toVerify
    validProto = OP.getDecNode classifiedLabel "PF" classifiedForward
    protoClassified = OP.getDecNode startNodeLabel "PF"
        (OP.BinaryNode (trueList ++ [validProto], [dropnode]))


