 {-# LANGUAGE DeriveGeneric #-}

module Dragonet.Flows (
    Flow(..),
    flowPred,
    flowStr,
) where

import qualified Dragonet.Predicate as PR

-- Flow definitions

import Data.Word
import Data.Maybe
import Dragonet.Implementation.IPv4 as IP4
import Control.Monad (liftM)

import GHC.Generics (Generic)
import Data.Hashable

type IPv4Addr = Word32
type UDPPort  = Word16

data Flow =
  -- UDP flow (Nothing is wildcards)
  FlowUDPv4 {
    flSrcIp    :: Maybe IPv4Addr,
    flDstIp    :: Maybe IPv4Addr,
    flDstPort  :: Maybe UDPPort,
    flSrcPort  :: Maybe UDPPort
  }
 deriving (Show, Eq, Ord, Generic)

instance Hashable Flow

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

flowStr :: Flow -> String
flowStr (FlowUDPv4 {flSrcIp   = srcIp,
                     flDstIp   = dstIp,
                     flSrcPort = srcPort,
                     flDstPort = dstPort})
 = "FL("++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l4p = "UDP"
        showIP = IP4.ipToString
        l3s = maybe "*" showIP $ srcIp
        l3d = maybe "*" showIP $ dstIp
        l4s = fromMaybe "*" $ liftM show $ srcPort
        l4d = fromMaybe "*" $ liftM show $ dstPort
