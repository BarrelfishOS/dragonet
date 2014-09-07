module Dragonet.Conventions(
    rxQPref, txQPref, qTag,
    isTruePort, isFalsePort
) where

import qualified Data.List as L

-- queue nodes in the:
--  . PRG should have the following label prefixes:
--  . LPG should have the following labels
rxQPref = "RxQueue"
txQPref = "TxQueue"

-- LPG queue nodes will be tagged as follows when duplicated, where suffix is
-- the coreesponding suffix of the PRG queue
-- (e.g., Q1, Q2, etc.)
qTag suffix = "Q" ++ suffix


-- True/False ports in O-nodes
isTruePort  = L.isSuffixOf "true"
isFalsePort = L.isSuffixOf "false"
