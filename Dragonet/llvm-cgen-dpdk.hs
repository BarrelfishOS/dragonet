import Dragonet.ProtocolGraph  as PG
import qualified Dragonet.Incremental as INC

import Runner.Common

-- Simulates a basic embedding (replace rx and tx queue by dpdk specific node)
pg4dpdk :: PGraph -> PGraph
pg4dpdk pg = tagNodes "" $ renameQueues "DPDKRxQueue" "DPDKTxQueue" pg

main :: IO ()
main = do
    let helpers = "llvm-helpers-dpdk"
        pstate = INC.policyStateInit 1 () dummyHwPolicy

    runStack pg4dpdk pstate dummyHwAction helpers


