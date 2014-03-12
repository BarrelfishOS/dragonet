{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGicmp (
    lpg, lpgClusters,
) where


import Dragonet.Unicorn
import LPGImpl

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Implementation as Impl

lpgClusters :: [(Int, [String])]
lpgNodes :: [(Int, PG.Node)]
lpgEdges :: [PG.PGEdge]
lpg :: PG.PGraph

-- The protocol graph
[unicornImpl_f|lpgIcmpImpl.unicorn|]

