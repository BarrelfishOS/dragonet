#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - This file orchastrates the whole flow
 -}

--module Dragonet(
module Embedding(
    embedSimple
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
         [MG.Gnode MC.Computation]
embedV2 bigGraph smallGraph = embeddedGraph
    where
        embeddedGraph = MC.embeddGraphs bigGraph smallGraph


main  :: IO()
main = do
         putStrLn outDot
    where
        lpg = LPG.getSampleLPG2Apps $ MC.getNetworkDependency
        prg = E1k.getE1kPRG
        outDot = embedSimple lpg prg



