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
genEmbeddedPraph = writeFile "Embedded.dot" $ EMBD.embedSimple lpg prg
    where
        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.getE1kPRG

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
        putStrLn outmsg
    where
        outmsg = "Done!"


