#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - This file orchastrates the whole flow
 -}

--module Dragonet(
module Main (
    main
) where


import qualified MyGraph as MG
import qualified Computations as MC
import qualified E1k as E1k
import qualified LPG as LPG
import qualified NetworkProcessing as NP
import qualified Embedding as EMBD


{-
 - Generates NetworkDependency graph
 -}
genNetGraph :: IO ()
genNetGraph = writeFile "NetDep.dot" $ MG.showFlowGraph
                        NP.getNetworkProcessingGraph

{-
 - Generates PRG for E1k
 -}
genE10kPRGGraph :: IO ()
genE10kPRGGraph = writeFile "E1kPRG.dot" $ MG.showFlowGraph E1k.getE1kPRG

testE1kConf :: IO()
testE1kConf =
    do
        putStrLn "####################################\n\n"
        writeFile "E1kbasePRG.dot" $ MG.showFlowGraph basePRG
        writeFile "E1kconf1PRG.dot" $ MG.showFlowGraph conf1PRG
        writeFile "E1kconf1PurgedPRG.dot" $ MG.showFlowGraph conf1PRG'
        writeFile "E1kconf2PRG.dot" $ MG.showFlowGraph conf2PRG
        writeFile "E1kconf2PurgedPRG.dot" $ MG.showFlowGraph conf2PRG'
    where
        basePRG = E1k.getE1kPRG
        conf1PRG = E1k.applyConfigList basePRG conf1
        conf1PRG' = E1k.purgeFixedConfigs conf1PRG

        conf2PRG = E1k.applyConfigList conf1PRG' conf1
        conf2PRG' = E1k.purgeFixedConfigs conf2PRG

        conf1 = E1k.getTestcaseConfiguration
        conf2 = [
            (MC.ConfDecision MC.L4UDPValidChecksum MC.ON)
            ]


{-
 - Generates LPG for sample case of two applications
 -}
genLPGGraph :: IO ()
genLPGGraph = writeFile "LPG.dot" $ MG.showFlowGraph $ LPG.getSampleLPG2Apps $
            MC.getNetworkDependency

{-
 - Generates embedded graph for PRG and LPG
 -}
genEmbeddedPraph :: IO ()
genEmbeddedPraph = writeFile "Embedded.dot" $ EMBD.embedSimple lpg' prg'
    where

        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.purgeFixedConfigs $ E1k.applyConfigList E1k.getE1kPRG E1k.getTestcaseConfiguration
        lpg' = MC.sortGraph lpg
        prg' = MC.sortGraph prg

{-
 - Generates embedded graph for PRG and LPG
 -}
testEmbedV2:: IO ()
testEmbedV2 = writeFile "EmbeddedV2.dot" $ EMBD.embedV2 lpg prg
    where
        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.purgeFixedConfigs $ E1k.applyConfigList E1k.getE1kPRG E1k.getTestcaseConfiguration



{-
 -
 -}
testSorting :: IO ()
testSorting =
        do
        putStrLn "Before Sorting"
        putStrLn beforeSort
        putStrLn lineBreak
        putStrLn "After sorting"
        putStrLn afterSort
        putStrLn lineBreak
        where
        lineBreak = "\n\n"
        graph = MC.getNetworkDependency
        beforeSort = show graph
        sorted = MC.sortGraph graph
        afterSort = show sorted
{-
 - main function:
 -}
main  :: IO()
main = do
        putStrLn "Generating NetDep.dot"
        genNetGraph
        putStrLn "Generating E1kPRG.dot"
        genE10kPRGGraph
        putStrLn "Generating LPG.dot"
        genLPGGraph
        putStrLn "Generating Embedded.dot"
        genEmbeddedPraph
        putStrLn "testing generic graph embedding EmbeddedV2.dot"
        testEmbedV2
--        testSorting
        putStrLn "Testing PRG with configuration"
        testE1kConf
        putStrLn outmsg
    where
        outmsg = "Done!"

mainV2 :: IO()
mainV2  = testE1kConf
