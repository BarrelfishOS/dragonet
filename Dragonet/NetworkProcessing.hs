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
import qualified Data.Set as Set


import qualified NetBasics as NB
import qualified Operations as OP



getNetworkDependency :: OP.Node
getNetworkDependency = etherClassified
    where
    etherValidDest = OP.getDecNode NB.L2EtherValidDest "PF" (OP.BinaryNode ([], []))
--    orNode getOperatorNode NB.XOR "+"  [etherValidDest] (OP.BinaryNode ([], []))
    etherLen = OP.getDecNode NB.L2EtherValidLen "PF" (OP.BinaryNode ([], []))
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet "PF" (OP.BinaryNode ([], []))

{-

        (ClassifiedL2Ethernet, [])
        , (L2EtherValidLen, [ClassifiedL2Ethernet])
        , (L2EtherValidType, [ClassifiedL2Ethernet])
        , (L2EtherValidCRC, [ClassifiedL2Ethernet])
        , (L2EtherValidBroadcast, [ClassifiedL2Ethernet])
        , (L2EtherValidMulticast, [ClassifiedL2Ethernet])
        , (L2EtherValidUnicast, [ClassifiedL2Ethernet])
        , (L2EtherValidDest, [L2EtherValidBroadcast])

        , (VerifiedL2Ethernet, [L2EtherValidCRC, L2EtherValidLen, L2EtherValidType
                       , L2EtherValidDest, L2EtherValidSrc])
-- getOperatorNode
-}

