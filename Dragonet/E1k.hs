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

--module E1k (
module Main (
    main
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
        , (MC.L4Unclasified, [MC.ClassifiedL3]) -- all other packets
        , (MC.ToKernelMemory, [MC.ClassifiedL4TCP]) --
        , (MC.ToKernelMemory, [MC.ClassifiedL4UDP]) --
        , (MC.ToKernelMemory, [MC.L4Unclasified]) --
        ]


{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph getE1kPRG

