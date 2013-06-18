#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Different configurations for NICs
 -
 -}

module Configurations (
    Configuration(..)
    , genDependencies
    , genAllDependencies
) where

import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Computations as MC


data Configuration = Always
                | EthernetChecksum
                | IPv4Checksum
                | TCPChecksum
                | UDPChecksum
                | QueueConf MC.Queue
                | FilterConf MC.Filter MC.Queue
                deriving (Show, Eq, Ord, DD.Typeable, DD.Data)

{-
 - Generates additional edges needed to support given configuration
 - This is specially needed for configurations which can add dynamic number
 - of edges.
 - eg: hardware queues, filters
 -}
genDependencies :: Configuration -> [(MC.Computation, [MC.Computation])]
genDependencies (QueueConf q) = [(node, deps)]
    where
        node =  MC.ToQueue q
        deps = []
genDependencies (FilterConf f q) = deps
    where
        node =  MC.IsFlow f
        deps = [(node, [MC.L4ReadyToClassify])
                , ((MC.ToQueue q), [node])]
genDependencies _ = []


genAllDependencies :: [Configuration] -> [(MC.Computation, [MC.Computation])]
genAllDependencies confList = DL.concatMap genDependencies confList

