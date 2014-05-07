import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import qualified Dragonet.Incremental as INC

import Runner.Common

-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = tagNodes "" $ renameQueues "TapRxQueue" "TapTxQueue" pg

main :: IO ()
main = do
    let fname = "lpgImpl.unicorn"
        helpers = "llvm-helpers"
        pstate = INC.policyStateInit 1 () dummyHwPolicy

    -- Parse graph and perform pseudo-embedding
    b <- readFile fname >>= UnicornAST.parseGraph
    let pgraph = pg4tap $ Unicorn.constructGraph b

    runStack pgraph pstate dummyHwAction helpers


