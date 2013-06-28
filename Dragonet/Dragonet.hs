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
        putStrLn $ show op
        putStrLn lineBreak
        putStrLn "Done..."
        where
        lineBreak = "\n\n"
        op = NP.getNetworkDependency


main :: IO()
--main = testOp
main = testNetworkProcessing

