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

--    prg = OP.appendToTrue etherClassified etherValidLen

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
            [],
            [dropnode]))



