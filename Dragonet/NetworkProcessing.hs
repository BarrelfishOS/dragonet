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

--import qualified Data.List as DL
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



getNetworkDependency :: OP.Node
getNetworkDependency = etherClassified
    where


    dropnode = OP.getDropNode

    classifyIPv4 = OP.getDecNode NB.ClassifiedL3IPv4 "PF"
        (OP.BinaryNode (
            [],
            []))
    classifyIPv6 = OP.getDecNode NB.ClassifiedL3IPv6 "PF"
        (OP.BinaryNode (
            [],
            []))
    l2Verified = OP.BinaryNode ([], [dropnode])
    l2Classified = OP.BinaryNode([classifyIPv4, classifyIPv6],
            [dropnode])

    etherClassified = getEthernetProcessingLPG l2Classified l2Verified dropnode

