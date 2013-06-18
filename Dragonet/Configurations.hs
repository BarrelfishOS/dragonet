#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Different configurations for NICs
 -
 -}

module Configurations (
    Configuration(..)
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
                deriving (Show, Eq, Ord, DD.Typeable, DD.Data)



