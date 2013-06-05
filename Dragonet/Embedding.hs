#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - This file orchastrates the whole flow
 -}

--module Dragonet(
module Embedding(
    embedSimple
    , embedV2
    , main
) where

import qualified MyGraph as MG
import qualified Computations as MC
import qualified E1k as E1k
import qualified LPG as LPG

{-
 - Embeds the small graph inside large graph
 - This emplementation just marks all the nodes in both graphs as embedded ones.
 -}
embedSimple :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation] ->
        String
embedSimple bigGraph smallGraph = embeddedGraph
    where
        embeddedGraph = MG.showEmbeddedGraph bigGraph smallGraph

embedV2 :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation] ->
--         [MG.Gnode MC.Computation]
        String
embedV2 bigGraph smallGraph = embeddedGraphStr
    where
        lpg = MC.sortGraph bigGraph
        prg = MC.sortGraph smallGraph

        embeddedGraph = MC.embeddGraphs lpg prg MC.InSoftware MC.getDefaultQueue
        embeddedGraphStr = MG.showFlowGraph embeddedGraph

main  :: IO()
main = do
         putStrLn outDot
    where
        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.getE1kPRG
        outDot = embedSimple lpg prg



