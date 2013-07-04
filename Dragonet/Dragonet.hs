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

testE10kEmbedding :: IO ()
testE10kEmbedding =
        do
        putStrLn "Applying embedding to configured E10kPRG"
        writeFile fileName $ DG.toDot tree'
        putStrLn ("Generated " ++ fileName)
--        putStrLn "common nodes are"
--        putStrLn commonNodes
        putStrLn "Done..."
        where
        fileName = "LPGembedded.dot"
        prg = OP.applyConfigWrapperList E10k.getE1kPRG E10k.getTestcaseConfiguration
        lpg = NP.getNetworkDependency
        tree' = OP.embeddGraphs prg lpg

testConfEmbedding :: IO ()
testConfEmbedding =
        do
        putStrLn "Generating minimal configuration for PRG"
        --writeFile fileName $ DG.toDot tree'
        --putStrLn ("Generated " ++ fileName)
        putStrLn output
        putStrLn "Done..."
        where
        output = OP.testGenConf E10k.getE1kPRG
        -- fileName = "E10kConfig.dot"
        -- tree' = OP.applyConfigWrapperList E10k.getE1kPRG E10k.getTestcaseConfiguration

testEmbeddingV2 :: IO ()
testEmbeddingV2 =
        do
        putStrLn "Generating resursive embedding"
        putStrLn output
        putStrLn "Done..."
        where
        output = OP.testEmbeddingV2 NP.getNetworkDependency -- E10k.getE1kPRG


allTests :: IO ()
allTests =
    do
    testNetworkProcessing
    testE10k
    testE10kConfig
    testE10kEmbedding

main :: IO()
--main = testOp
--main = testE10k
--main = testNetworkProcessing
--main = allTests
--main = testConfEmbedding
main = testEmbeddingV2




