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
getE1kPRG = prg
    where

    dropnode = OP.getDropNode
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet ""
        (OP.BinaryNode (
            [],
            [dropnode]))

    etherValidLen = OP.getDecNode NB.L2EtherValidLen ""
        (OP.BinaryNode (
            [],
            [dropnode]))

    prg = OP.appendToTrue etherClassified etherValidLen



