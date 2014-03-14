{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGImpl.LPGImplBase (
    pbool,
    --toPort,
    cfgLocalMAC,
    cfgLocalIP,
) where

import Dragonet.DotGenerator

import qualified Dragonet.ProtocolGraph as PG
import Dragonet.Unicorn
import Dragonet.Implementation

import qualified Dragonet.Implementation.Ethernet as ETH
import qualified Dragonet.Implementation.IPv4 as IP4

import Data.Maybe
import Data.Bits
import Data.Word

-----------------------------------------------------------------------------

pbool b = if b then "true" else "false"


-- FIXME: figure out a way to get correct IP automatically based
-- on TUNTAP or DPDK interface.

-- FOR DPDK
-- --cfgLocalIP = fromJust $ IP4.ipFromString "10.111.4.36"
--cfgLocalMAC = fromJust $ ETH.macFromString "00:0f:53:07:48:d5"
--cfgLocalIP = fromJust $ IP4.ipFromString "10.111.4.37"

-- FOR TUNTAP
cfgLocalMAC = fromJust $ ETH.macFromString "00:1b:22:54:69:f8"
cfgLocalIP = fromJust $ IP4.ipFromString "192.168.123.1"

-- FOR openonload
--cfgLocalMAC = fromJust $ ETH.macFromString "00:0f:53:07:51:49"
--cfgLocalIP = fromJust $ IP4.ipFromString "10.113.4.71"


