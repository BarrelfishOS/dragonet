import Dragonet.ProtocolGraph  as PG
import qualified Dragonet.Incremental as INC

import Runner.Common

-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = tagNodes "" $ renameQueues "SFRxQueue" "SFTxQueue" pg

main :: IO ()
main = do
    let helpers = "llvm-helpers-sf"
        pstate = INC.policyStateInit 1 () dummyHwPolicy

    runStack pg4tap pstate dummyHwAction helpers


