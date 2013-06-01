#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Simple code with an objective to generate Network Flow graph.
 -
 - TODO: Maybe I should move the getNetworkDependency code in this file.
 -}

--module NetworkProcessing (
module Main (
    main
) where


import qualified MyGraph as MG
import qualified Computations as MC

{-
 - main function: mostly used to test e1k by generating it
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph MC.getNetworkDependency


