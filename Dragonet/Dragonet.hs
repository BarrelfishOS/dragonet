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


main :: IO()
main =  testOp

