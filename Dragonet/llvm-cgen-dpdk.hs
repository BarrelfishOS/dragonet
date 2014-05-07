import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import qualified Dragonet.Incremental as INC

import Runner.Common

-- Simulates a basic embedding (replace rx and tx queue by dpdk specific node)
pg4dpdk :: PGraph -> PGraph
pg4dpdk pg = tagNodes "" $ renameQueues "DPDKRxQueue" "DPDKTxQueue" pg

main :: IO ()
main = do
    let fname = "lpgImpl.unicorn"
        helpers = "llvm-helpers-dpdk"
        pstate = INC.policyStateInit 1 () dummyHwPolicy

    -- Parse graph and perform pseudo-embedding
    b <- readFile fname >>= UnicornAST.parseGraph
    let pgraph = pg4dpdk $ Unicorn.constructGraph b

    runStack pgraph pstate dummyHwAction helpers


