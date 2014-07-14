module Dragonet.Embedding(
    embeddingRxTx
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers as GH

import qualified Data.Graph.Inductive as DGI
import Data.Maybe
import qualified Data.List as L


--------------------------------------------------------------------------------
-- Embedding for both RX and TX path

rxPref = "RxQueue"
txPref = "TxQueue"

qTag pref = "Q" ++ pref

tagNodes :: String -> PG.PGraph -> PG.PGraph
tagNodes tag = DGI.nmap tagN
    where tagN n = n { PG.nTag = tag }

tagPrgQueues :: PG.PGraph -> PG.PGraph
tagPrgQueues = DGI.nmap tagQueue
    where
        isQueueNode n = rxPref `L.isPrefixOf` l || txPref `L.isPrefixOf` l
            where l = PG.nLabel n
        tagQueue n
            | isQueueNode n = n {
                    PG.nTag = qTag $ drop (length rxPref) $ PG.nLabel n }
            | otherwise = n

embeddingRxTx :: PG.PGraph -> PG.PGraph -> PG.PGraph
embeddingRxTx prg lpg = withLPGs
    where
        prg' = tagPrgQueues prg
        -- List of queue identifiers
        rxQs = [ drop (length rxPref) l |
                    (_, n)<- DGI.labNodes prg,
                    let l = PG.nLabel n,
                    rxPref `L.isPrefixOf` l]
        -- PRG with full LPG added for each queue
        withLPGs = foldl addLPG prg' rxQs
        mergeP pr a b
            | lB == rxPref ++ pr && lA == rxPref = True
            | lB == txPref ++ pr && lA == txPref = True
            | otherwise = False
            where
                lA = PG.nLabel a
                lB = PG.nLabel b
        addLPG g pref = GH.mergeGraphsBy (mergeP pref) g $
                            tagNodes (qTag pref) lpg


