module Dragonet.Embedding(
    embeddingRxTx
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers as GH
import Dragonet.Embedding.Offload (embedOffload)
import Dragonet.Conventions (rxQPref, txQPref, qTag)

import qualified Data.Graph.Inductive as DGI
import Data.Maybe
import qualified Data.List as L
import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

--------------------------------------------------------------------------------
-- Embedding for both RX and TX path

tagNodes :: String -> PG.PGraph -> PG.PGraph
tagNodes tag = DGI.nmap tagN
    where tagN n = n { PG.nTag = tag }

setOrigin :: String -> PG.PGraph -> PG.PGraph
setOrigin origin = DGI.nmap f
    where f n@(PG.FNode {}) = n { PG.nOrigin = origin }
          f n               = n

tagPrgQueues :: PG.PGraph -> PG.PGraph
tagPrgQueues = DGI.nmap tagQueue
    where
        isQueueNode n = rxQPref `L.isPrefixOf` l || txQPref `L.isPrefixOf` l
            where l = PG.nLabel n
        tagQueue n
            | isQueueNode n = n {
                    PG.nTag = qTag $ drop (length rxQPref) $ PG.nLabel n }
            | otherwise = n


-- connect one LPG to the PRG queues identified by rxQ:
-- This also tags the lpg nodes with the queue identifier
addLPG :: PG.PGraph -> PG.PGraph -> String -> PG.PGraph
addLPG lpg prg rxQ = GH.mergeGraphsBy mergeP lpg' prg
    where lpg'  = tagNodes (qTag rxQ) lpg -- tag lpg nodes
          mergeP prgN lpgN
            | lpn == rxQPref ++ rxQ && lln == rxQPref = True -- rx queue match
            | lpn == txQPref ++ rxQ && lln == txQPref = True -- tx queue match
            | otherwise = False
            where lpn = PG.nLabel prgN -- prg node label
                  lln = PG.nLabel lpgN -- lpg node label

embeddingRxTx :: PG.PGraph -> PG.PGraph -> PG.PGraph
embeddingRxTx prg lpg = embg'
    where
        prg' = setOrigin "PRG" $ tagPrgQueues prg
        lpg' = setOrigin "LPG" lpg
        -- List of queue identifiers
        rxQs = [ drop (length rxQPref) l |
                    (_, n)<- DGI.labNodes prg,
                    let l = PG.nLabel n,
                    rxQPref `L.isPrefixOf` l]
        -- PRG with full LPG added for each queue
        -- rxQs contains the name of the queue without the prefix
        embg  = foldl (addLPG lpg') prg' rxQs
        embg' = foldl embedOffload  embg rxQs
