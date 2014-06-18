{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGEx1 (
    lpg, lpgClusters,
) where


import Dragonet.Unicorn
import LPGImpl

import Dragonet.ProtocolGraph
import qualified Dragonet.Implementation as Impl


-- The protocol graph
[unicorn_f|lpgImpl.unicorn|]

