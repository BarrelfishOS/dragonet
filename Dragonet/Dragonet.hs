#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - This file orchastrates the whole flow
 -}

--module Dragonet(
module Main (
    main
) where

import qualified Data.List as DL

import qualified NetBasics as NB
import qualified Operations as OP
import qualified NetworkProcessing as NP
import qualified E10kPRG as E10k
import qualified DotGenerator as DG
import qualified Embedding as EM

testOp :: IO ()
testOp =
        do
        putStrLn "Testing Operations"
        putStrLn $ show op
        putStrLn lineBreak
        putStrLn "Done..."
        where
        lineBreak = "\n\n"
        op = OP.testOperation


testNetworkProcessing :: IO ()
testNetworkProcessing =
        do
        putStrLn "Testing NetworkProcessing"
        writeFile fileName $ DG.toDot op
        putStrLn ("Generated " ++ fileName)
        putStrLn "Done..."
        where
        fileName = "NetProcessing.dot"
        op = NP.getNetworkDependency

testE10k :: IO ()
testE10k =
        do
        putStrLn "Testing E10kPRG"
        writeFile fileName $ DG.toDot op
        putStrLn ("Generated " ++ fileName)
        putStrLn "Done..."
        where
        fileName = "E10k.dot"
        op = E10k.getE1kPRG


testE10kConfig :: IO ()
testE10kConfig =
        do
        putStrLn "Applying config to E10kPRG"
        writeFile fileName $ DG.toDot tree'
        putStrLn ("Generated " ++ fileName)
        putStrLn "Done..."
        where
        fileName = "E10kConfig.dot"
        tree' = OP.applyConfigWrapperList E10k.getE1kPRG E10k.getTestcaseConfiguration

testEmbeddingLarge :: IO ()
testEmbeddingLarge =
        do
        putStrLn "Generating embedding for large graphs"

        putStrLn "Edges in PRG graph"
        putStrLn prgEdges

        putStrLn "Edges in LPG graph"
        putStrLn lpgEdges

        writeFile "LPGsmall.dot" $ DG.toDotFromDL lpgDep
        writeFile "PRGsmallUnconf.dot" $ DG.toDot prgUnconf
        writeFile "PRGsmall.dot" $ DG.toDotFromDL prgDep

        writeFile "EMBEDDsmall.dot" $ DG.toDotFromDL embedded

        putStrLn "Done..."

        where
        prgUnconf = E10k.getE1kPRG
        prg = OP.applyConfigWrapperList prgUnconf E10k.getTestcaseConfiguration

        prgDep = EM.removeDroppedNodes $ EM.getDepEdges prg
        prgEdges = EM.testEmbeddingSTR prg
        lpgEdges = EM.testEmbeddingSTR lpg
        lpg = NP.getNetworkDependency
        lpgDep = EM.removeDroppedNodes $ EM.getDepEdges lpg

        embedded = EM.testEmbeddingV2 prg lpg


testEmbeddingSmall :: IO ()
testEmbeddingSmall =
        do
        putStrLn "Generating small testgraphs"

        putStrLn "Edges in PRG graph"
        putStrLn prgEdges

        putStrLn "Edges in LPG graph"
        putStrLn lpgEdges

        writeFile "LPGsmall.dot" $ DG.toDotFromDL lpgDep
        writeFile "PRGsmallUnconf.dot" $ DG.toDot prgUnconf
        writeFile "PRGsmall.dot" $ DG.toDotFromDL prgDep
        writeFile "EMBEDDsmall.dot" $ DG.toDotFromDL embedded
        putStrLn "Done..."

        where
        prgUnconf = E10k.getE1kPRGSmall
        prg = OP.applyConfigWrapperList prgUnconf E10k.getTestcaseConfiguration

        prgDep = EM.removeDroppedNodes $ EM.getDepEdges prg
        prgEdges = EM.testEmbeddingSTR prg
        lpgEdges = EM.testEmbeddingSTR lpg
        lpg = NP.getNetworkDependencySmall
        lpgDep = EM.removeDroppedNodes $ EM.getDepEdges lpg
        embedded = EM.testEmbeddingV2 prg lpg


testPRGAdjustment :: IO ()
testPRGAdjustment =
        do
        putStrLn "Testing PRG readjustment"

        putStrLn "Edges in PRG graph"
        putStrLn prgEdges

        writeFile "PRGAdjustUnconf.dot" $ DG.toDot prgUnconf
        writeFile "PRGAdjustBefore.dot" $ DG.toDotFromDL prgDep
        writeFile "PRGAdjustAfter.dot" $ DG.toDotFromDL prgAdjusted
        putStrLn "Done..."

        where
        prgUnconf = E10k.getE1kPRG
        prg = OP.applyConfigWrapperList prgUnconf E10k.getTestcaseConfiguration

        prgDep = EM.removeDroppedNodes $ EM.getDepEdges prg

        prgEdges = EM.testEmbeddingSTR prg

        prgAdjusted = EM.testPRGrearrangement prg


allTests :: IO ()
allTests =
    do
    testNetworkProcessing
    testE10k
    testE10kConfig

main :: IO()
--main = testOp
--main = testE10k
--main = testNetworkProcessing
--main = allTests
--main = testEmbeddingV2
--main = testEmbeddingSmall
--main = testEmbeddingLarge
main = testPRGAdjustment


