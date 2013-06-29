#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - This file orchastrates the whole flow
 -}

--module Dragonet(
module Main (
    main
) where


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

allTests :: IO ()
allTests =
        do
        testNetworkProcessing
        testE10k


main :: IO()
--main = testOp
--main = testE10k
--main = testNetworkProcessing
main = allTests

