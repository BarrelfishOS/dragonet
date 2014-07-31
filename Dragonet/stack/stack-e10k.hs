import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.E10k as E10k

import Stack


-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- Start out with a dummy configuration for e10k
oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[
                ("RxCFDirFilter", PG.CVList []),
                ("RxC5TupleFilter", PG.CVList [])
                ])]

-- Nothing to be implemented yet
implCfg :: C.Configuration -> IO ()
implCfg _ = return ()

-- Assign all nodes to same pipeline
plAssign :: StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n

main = do
    -- Prepare graphs and so on
    prgH <- E10k.graphH
    instantiate prgH "llvm-helpers-e10k" costFunction oracle implCfg plAssign

