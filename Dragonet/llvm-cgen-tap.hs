import Dragonet.ProtocolGraph  as PG
import qualified Dragonet.Incremental as INC

import Runner.Common

-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = tagNodes "" $ renameQueues "TapRxQueue" "TapTxQueue" pg

main :: IO ()
main = do
    (nQ, apps) <- parseDNArgs
    putStrLn $ "Running hardware queues: " ++ show nQ
    putStrLn $ "Running with app slots: " ++ show apps
    let helpers = "llvm-helpers-tap"
        pstate = INC.policyStateInit nQ () dummyHwPolicy
    runStackParsed apps pg4tap pstate dummyHwAction helpers


