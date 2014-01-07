{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGEx1 (
    lpg, lpgClusters,
) where


import Dragonet.Unicorn
import LPGImpl

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Implementation as Impl

lpgClusters :: [(Int, [String])]
lpgNodes :: [(Int, PG.Node Impl.Implementation)]
lpgEdges :: [PG.PGEdge]
lpg :: PG.PGraph Impl.Implementation

-- The protocol graph
[unicornImpl_f|lpgImpl.unicorn|]

