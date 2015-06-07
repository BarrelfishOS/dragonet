-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Scenarios.S1 (
      priorityCost, prioritySort
    , priorityCost' , prioritySort'
    , priorityCost'' , prioritySort''
    , sortedRealFlows
    , real40Flows
    , sortFlows
    , staticCost
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

--hpm1 = "10.113.4.51"  -- ziger1
--hpm2 = "10.113.4.57"  -- ziger2

hpm1 = "10.113.4.26"  -- sbrinz1
hpm2 = "10.113.4.29"  -- sbrinz2



goldFlPerQ = 1
isGoldFlOld FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip} = ans
    where
        ans
            | (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString hpm1)) =  True
            | (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString hpm2)) =  True
            | otherwise = False
isGoldFlOld _ = False

isGoldFl =  isGoldFlv2

isGoldFlv2 fPerApp FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip, flDstPort = Just dstPort} = ans
    where
        fpa = fromIntegral fPerApp
        ans
            -- This is essentially a hole to add a special HP flow later
            | (sport < 8000) && (sport >= (8000 - fpa)) && (sip == (myFromMaybe $ IP4.ipFromString hpm2)) =  True
            | (sport >= 8000) && (sport < (8000 + fpa)) && (sip == (myFromMaybe $ IP4.ipFromString hpm2)) =  True
            | (sport < 8000) && (sport >= (8000 - fpa)) && (sip == (myFromMaybe $ IP4.ipFromString hpm1)) =  True
            | (sport >= 8000) && (sport < (8000 + fpa)) && (sip == (myFromMaybe $ IP4.ipFromString hpm1)) =  True
            | (sport == 5000) && (sip == (myFromMaybe $ IP4.ipFromString hpm1)) =  True        -- For fancyecho
            | (sport == 5000) && (sip == (myFromMaybe $ IP4.ipFromString hpm2)) =  True        -- For fancyecho
            | otherwise = False
isGoldFlv2 _ _ = False

isGoldFlOnlyPort fPerApp FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip, flDstPort = Just dstPort} = ans
    where
        ans
            | (dstPort == 7777)  =  True
            | otherwise = False
isGoldFlOnlyPort _ _ = False


priorityCost'' goldRange fperQ = Search.priorityCost (isGoldFl goldRange) fperQ
prioritySort'' goldRange = Search.prioritySort (isGoldFl goldRange)



-- Using flow per queue as range for gold flows
priorityCost' fperQ =  priorityCost'' fperQ fperQ
prioritySort' fperQ =  prioritySort'' fperQ


priorityQs = 4
staticCost goldRange = Search.staticCost (isGoldFl goldRange) priorityQs

-- Using 1 flow per queue as range for gold flows
priorityCost = priorityCost'' 1 1
prioritySort = prioritySort'' 1


sortFlows isImp fl = sortedFlows
    where
        allFlows = fl
        impList = filter isImp $ allFlows
        otherList = filter (not . isImp) allFlows
        sortedFlows = impList ++  otherList

sortedRealFlows = sortFlows isGoldFlOld real40Flows

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

