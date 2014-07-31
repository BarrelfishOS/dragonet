import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap

import Stack


-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- TAP config is trivial
oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[])]

-- So is implementing it
implCfg :: PLI.StateHandle -> C.Configuration -> IO ()
implCfg _ _ = return ()

-- Split Rx and Tx into different pipelines
plAssignSplit :: StackState -> String -> PG.PGNode -> String
plAssignSplit _ _ (_,n)
    | ('R':'x':_) <- PG.nLabel n = "Rx"
    | otherwise = "Tx"



main = do
    -- Prepare graphs and so on
    prgH <- Tap.graphH
    instantiate prgH "llvm-helpers-tap" costFunction oracle implCfg plAssignSplit

