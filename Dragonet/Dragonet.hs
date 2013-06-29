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
        putStrLn $ DG.toDot op
        putStrLn lineBreak
        putStrLn "Done..."
        where
        lineBreak = "\n\n"
        op = NP.getNetworkDependency

testE10k :: IO ()
testE10k =
        do
        putStrLn "Testing E10kPRG"
        putStrLn $ DG.toDot op
        putStrLn lineBreak
        putStrLn "Done..."
        where
        lineBreak = "\n\n"
        op = E10k.getE1kPRG



main :: IO()
--main = testOp
main = testNetworkProcessing
--main = testE10k

