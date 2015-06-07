-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.Conventions(
    rxQPref, txQPref, qTag,
    isTruePort, isFalsePort,
    constTrueName, constFalseName,
    QueueId,
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

-- constant nodes (used for optimization)
constTrueName  = "_True"
constFalseName = "_False"


type QueueId = Int
