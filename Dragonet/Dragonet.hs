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
        prg = E1k.getE1kPRG
        lpg' = MC.sortGraph lpg
        prg' = MC.sortGraph prg

{-
 - Generates embedded graph for PRG and LPG
 -}
testEmbedV2:: IO ()
testEmbedV2 = writeFile "EmbeddedV2.dot" $ EMBD.embedV2 lpg prg
    where
        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.getE1kPRG


testE1kConf :: IO()
testE1kConf = E1k.getE1kPRGConfTest
--    do
--        putStrLn $ MG.showFlowGraph E1k.getE1kPRGConfTest
--        writeFile "E1kPRGConf.dot" $ MG.showFlowGraph E1k.getE1kPRGConfTest


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
mainV1  :: IO()
mainV1 = do
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
        testSorting
        putStrLn outmsg
    where
        outmsg = "Done!"

main :: IO()
main  = testE1kConf
