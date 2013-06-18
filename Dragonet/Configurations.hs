#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Different configurations for NICs
 -
 -}

module Configurations (
    Configuration(..)
    , getConfToCompList
    , getComputationForConf
) where

import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Computations as MC


data Configuration = Always
                | EthernetChecksum
                | IPv4Checksum
                | TCPChecksum
                | UDPChecksum
                deriving (Show, Eq, Ord, DD.Typeable, DD.Data)


getConfToCompList :: [(Configuration, MC.Computation)]
getConfToCompList = [
        (IPv4Checksum, MC.L3IPv4ValidChecksum)
        , (EthernetChecksum, MC.L2EtherValidCRC)
        , (TCPChecksum, MC.L4TCPValidChecksum)
        , (UDPChecksum, MC.L4UDPValidChecksum)
    ]

-- convert conf value into configuration
getComputationForConf :: Configuration -> [MC.Computation]
getComputationForConf conf =
       DL.map (\x -> snd x) $ DL.filter (\ x -> conf == fst x) getConfToCompList


