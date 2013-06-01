#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

--module E1k (
module Main (
    main
) where


import qualified MyGraph as MG
import qualified Conditions as MC

{-
 - main function: mostly used to test e1k by generating it
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph MC.getNetworkDependency


