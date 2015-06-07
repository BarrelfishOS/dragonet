-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Graphs.E10k.FlowQueue (
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import Dragonet.Flows (Flow (..))

import qualified Util.GraphHelpers as GH

import Graphs.E10k (QueueId, C5Tuple(..), strToC5t)

import Data.Char (isDigit)

import Text.Show.Pretty (ppShow)

-- E10k version of flowQueue. Now deprecated for a generic flowQueue (see
-- Dragonet.Search). Keep around for future reference.

flowQueue :: PG.PGraph -> Flow -> QueueId
flowQueue prgC flow = ret
    where ret = doFlowQueue prgC node1 flow
          --node0_ = "RxIn"
          --port0 = "out"
          node0_ = "RxL2EtherClassifyL3_"
          port0 = "other"
          node0 = case GH.filterNodesByL (\x -> (PG.nLabel x) == node0_) prgC of
            [x] -> x
            []  -> error $ "No matches for node:" ++ node0_
            _   -> error $ "More than one matches for node:" ++ node0_
          node1 = case [ n | (n,e) <- PGU.edgeSucc prgC node0,
                          PGU.edgePort e == port0] of
            [x] -> x
            _   -> error $ "More than one connection matches for node:"
                           ++ node0_ ++ " port:" ++ port0

doFlowQueue :: PG.PGraph -> PG.PGNode -> Flow -> QueueId
doFlowQueue g node fl
    | Just q <- reachedQueue node = q
    | otherwise = ret
    where ret = doFlowQueue g next fl
          port = flowGetPort fl node
          isOnode (PG.ONode {}) = True
          isOnode _             = False
          next = case [ n | (n,e) <- PGU.edgeSucc g node,
                        PGU.edgePort e == port,
                        port == "true" || (not $ isOnode $ snd n)] of
                   [x] -> x
                   l  -> error $ "More than one connection matches for node:"
                                 ++ (PG.nLabel . snd) node ++ "port:" ++ port
                                 ++ (ppShow l)

flowGetPort :: Flow -> PG.PGNode -> PG.NPort
flowGetPort fl (_, nlbl)
    | (take 2 name) == "5T" = case flowMatches5TF fl (strToC5t name) of
                                   True  -> "true"
                                   False -> "false"
    | otherwise = error $ "flowGetPort:"  ++ (PG.nLabel nlbl)
    where name = PG.nLabel nlbl

reachedQueue :: PG.PGNode -> Maybe QueueId
reachedQueue (_,PG.ONode {PG.nLabel = name}) = ret
    where n = filter isDigit name
          ret = case length n of
                  0 -> Nothing
                  _ -> case "RxQ" ++ n ++ "Valid" == name of
                            True  -> Just $ read n
                            False -> Nothing

reachedQueue (_,PG.FNode {PG.nLabel = name})
    | name == "RxToDefaultQueue" =  Just 0
    | otherwise  = Nothing

maybeMatch Nothing _ = True
maybeMatch _ Nothing = True
maybeMatch (Just x1) (Just x2) = x1 == x2


flowMatches5TF :: Flow -> C5Tuple -> Bool
flowMatches5TF (FlowUDPv4 {flSrcIp = srcIp,
                           flDstIp = dstIp,
                           flSrcPort = srcPort,
                           flDstPort = dstPort })
                (C5Tuple {c5tL4Proto = cProt,
                          c5tL3Src   = cSrcIp,
                          c5tL3Dst   = cDstIp,
                          c5tL4Src   = cSrcPort,
                          c5tL4Dst   = cDstPort}) = ret
 where maybeT = maybe True
       ip_match Nothing _ = True
       ip_match _ Nothing = True
       ip_match (Just ip1) (Just ip2) = ip1 == ip2
       ret =   maybeMatch srcIp cSrcIp
            && maybeMatch dstIp cDstIp
            && maybeMatch srcPort cSrcPort
            && maybeMatch dstPort cDstPort
