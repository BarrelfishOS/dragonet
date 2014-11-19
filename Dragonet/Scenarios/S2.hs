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
