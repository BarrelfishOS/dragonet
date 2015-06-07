-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Scenarios.S2 (
    endpoints,
    lpgConf
) where

import qualified Dragonet.Implementation.IPv4 as IPv4
import Dragonet.Endpoint (EndpointDesc(..),SocketId,AppId)

import qualified Graphs.LPG as LPG

import Data.Maybe


endpoints = [
    -- listen endpoint
      EndpointUDPv4 {
            epSockets = [(1,1)]
          , epRemoteIp = Nothing
          , epRemotePort = Nothing
          , epLocalIp = Nothing
          , epLocalPort = Just 53
      }
    , EndpointUDPv4 {
            epSockets = [(2,1)]
          -- yeah, Just $ fromJust is weird but ipFromString returns Noting as
          -- an error condition, Nothing in epRemoteIp acts as a wildcard
          , epRemoteIp = Just $ fromJust $ IPv4.ipFromString "192.168.200.100"
          , epRemotePort = Just 89891
          , epLocalIp  = Nothing
          , epLocalPort = Just 53
    }
 ]

lpgConf = LPG.lpgConfig endpoints
