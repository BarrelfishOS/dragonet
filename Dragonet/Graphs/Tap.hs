module Graphs.Tap (
    graphH
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as SEM

import Graphs.Helpers

graphH :: IO (PG.PGraph,SEM.Helpers)
graphH = parseGraph "Graphs/Tap/prgTapImpl.unicorn"

