module Dragonet.Flows (
    Flow(..),
    flowPred
) where

import qualified Dragonet.Predicate as PR

-- Flow definitions

import Data.Word

type IPv4Addr = Word32
type UDPPort  = Word16

data Flow =
  -- UDP flow (Nothing is wildcards)
  FlowUDPv4 {
    flSrcIp    :: Maybe IPv4Addr,
    flDstIp    :: Maybe IPv4Addr,
    flDstPort  :: Maybe UDPPort,
    flSrcPort  :: Maybe UDPPort
  } deriving (Show, Eq, Ord)

flowPred :: Flow -> PR.PredExpr
flowPred (FlowUDPv4 {flSrcIp   = srcIp,
                     flDstIp   = dstIp,
                     flSrcPort = srcPort,
                     flDstPort = dstPort})
 = ret
       where protPreds = [PR.PredAtom "EthType" "IPv4", PR.PredAtom "IpProt" "UDP"]
             srcIpPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "SrcIp" ("p" ++ show x)))
                               srcIp
             dstIpPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "DstIp" ("p" ++ show x)))
                               dstIp
             srcPortPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "SrcPort" ("p" ++ show x)))
                               srcPort
             dstPortPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "DstPort" ("p" ++ show x)))
                               dstPort
             ret = xand $ protPreds ++
                             [srcIpPred,dstIpPred,srcPortPred,dstPortPred]
             bld = PR.predBuildFold
             xand = (PR.buildAND bld)
