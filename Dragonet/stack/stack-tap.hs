import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap

import Stack


-- Does not really matter as we only have one config
costFunction :: StackState -> [EndpointDesc] -> O.CostFunction Int
costFunction _ _ _ = (1, "cost:always 1")

-- TAP config is trivial
optOracle :: StackState -> [(String,C.Configuration)]
optOracle _ = [("default",[])]

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
    instantiateOpt prgH "llvm-helpers-tap" costFunction optOracle implCfg plAssignSplit
    --instantiateFlows (\_ -> []) prgH "llvm-helpers-tap" implCfg plAssignSplit

