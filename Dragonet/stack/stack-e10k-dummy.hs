--{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Graphs.Helpers as GH
import qualified Stack as S
import qualified Dragonet.Embedding as Emb

import qualified Graphs.LPG as LPG

{-
import qualified Dragonet.Unicorn as U
import Text.RawString.QQ (r)

using r does not work for some llvm-related reason
it might be this issue:
 . https://github.com/bscarlet/llvm-general/issues/63
which seems to be fixed in ghc 7.8:
 . https://ghc.haskell.org/trac/ghc/ticket/3333

prgH = U.strToGraph [r|
graph dummy {
    node RxQueue {
        implementation TapRxQueue
        attr "init"
        attr "source"
        attr "software"
        spawn poll RxQueue
        port out[]
        port drop[]
        port init[] }

    node TxQueue {
        implementation TapTxQueue
        attr "init"
        attr "sink"
        attr "software" }
}
|]
-}

-- the purpose here is to create a dummy PRG for E10k that consists of only
-- software nodes to experiment with embedding. We are going to execute it using
-- tap. At this point it does not look much like E10k though.

-- Does not really matter as we only have one config
costFunction :: S.StackState -> O.CostFunction Int
costFunction _ _ = 1

-- TAP config is trivial
oracle :: PG.PGraph -> S.StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[])]

-- So is implementing it
implCfg :: PLI.StateHandle -> C.Configuration -> IO ()
implCfg _ _ = return ()

-- Split Rx and Tx into different pipelines
plAssignSplit :: S.StackState -> String -> PG.PGNode -> String
plAssignSplit _ _ (_,n)
    | ('R':'x':_) <- PG.nLabel n = "Rx"
    | otherwise = "Tx"

main = do
    (prgU, prgH) <- GH.parseGraph "Graphs/E10k/prgE10kDummy.unicorn"
    (lpgU, lpgH) <- LPG.graphH_   "Graphs/LPG/lpgConfImpl-offload.unicorn"
    --(lpgU, lpgH) <- LPG.graphH
    S.startStack (lpgU,lpgH) (prgU, prgH) Emb.embeddingRxTx "llvm-helpers-tap" plAssignSplit

