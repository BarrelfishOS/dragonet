module Scenarios.S1 (
    priorityCost
    , sortedRealFlows
    , real40Flows
    , sortFlows
) where

import Dragonet.Flows(Flow (..), flowPred)
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Search as Search

--fs2 = [ FlowUDPv4 {
--     flDstIp    = Just 127
--   , flDstPort  = Just $ fromIntegral $ 7777
--   , flSrcIp    = IP4.ipFromString "10.113.4.51"
--   , flSrcPort  = Just $ fromIntegral $ 8000 + i } | i <- [0..30] ]
--
---- These two should be the gold flows
---- ziger1 : 10.113.4.51:8000
---- ziger2 : 10.113.4.57:8000
--
myFromMaybe (Just x) = x
myFromMaybe _ = error "No IP address"

goldFlPerQ = 1
isGoldFl FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip} = ans
    where
        ans
            | (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.51")) =  True
            | (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.57")) =  True
            | otherwise = False
isGoldFl _ = False

priorityCost = Search.priorityCost isGoldFl goldFlPerQ



sortFlows isImp fl = sortedFlows
    where
        allFlows = fl
        impList = filter isImp $ allFlows
        otherList = filter (not . isImp) allFlows
        sortedFlows = impList ++  otherList

sortedRealFlows = sortFlows isGoldFl real40Flows

-- All flows are:
real40Flows = [FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178847, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002}]

