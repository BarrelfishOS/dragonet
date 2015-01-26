 {-# LANGUAGE DeriveGeneric #-}

module Dragonet.Flows (
    Flow(..),

    FlowsSt(..),
    fsAddFlow,
    fsRemFlow,
    fsReset,
    flowsStInit,

    flowPred,
    flowStr, flowsStr,
    epToFlow,
) where

import qualified Dragonet.Predicate as PR
import qualified Dragonet.NetState  as NS

-- Flow definitions

import Data.Word
import Data.Maybe
import Dragonet.Implementation.IPv4 as IP4
import Control.Monad (liftM)

import qualified Data.Set as S
import qualified Data.List as L

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

data FlowsSt = FlowsSt {
      fsCurrent   :: S.Set Flow
    , fsAdded     :: S.Set Flow
    , fsRemoved   :: S.Set Flow
} deriving (Show)

flowsStInit = FlowsSt S.empty S.empty S.empty

-- add a flow
fsAddFlow :: FlowsSt -> Flow -> FlowsSt
fsAddFlow st fl = st { fsRemoved = removed'
                     , fsAdded   = added'}
    where removed   = fsRemoved st
          added     = fsAdded   st
          current   = fsCurrent st
          flRemoved = S.member fl removed
          added'    = S.insert fl added
          removed'  = case flRemoved of
                True  -> S.delete fl removed
                False -> removed

-- remove a flow
fsRemFlow :: FlowsSt -> Flow -> FlowsSt
fsRemFlow = error "fsRemFlow: NYI!"

-- reset the state
fsReset :: FlowsSt -> FlowsSt
fsReset st = st {  fsRemoved = S.empty
                 , fsAdded   = S.empty
                 , fsCurrent = next }

    where current = fsCurrent st
          removed = fsRemoved st
          added   = fsAdded st
          next = (current `S.difference` removed) `S.union` added

-- For endpoints, we use local/remote
-- Should we use the same for flows (instead of Rx/Tx)?
-- We typically use flows on the Rx side, so the mapping is:
--  local:  dst
--  remote: src
--
--  Note that endpoints are conceptually different than flows:
--   - Endpoints are LPG nodes that are (typically) configured by the
--     application and are "programming" the net stack to steer packets into
--     particular buffers (i.e., sockets)
--
--   - Flows, on the other hand, are packet classes that are used to evaluate
--     PRG configurations via cost functions. Given a set of flows, we use the
--     PRG to determine how these flows are mapped into the queues for a given
--     configuration. Then, we evaluate these mappings using a cost function.
--
epToFlow NS.EndpointUDPv4 {NS.epLocalIp = lIp,
                           NS.epLocalPort = lPort,
                           NS.epRemoteIp = rIp,
                           NS.epRemotePort = rPort }
   = FlowUDPv4 {
          flSrcIp = rIp,
          flDstIp = lIp,
          flDstPort  = lPort,
          flSrcPort  = rPort }


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

flowsStr flows = L.intercalate "\n" $ map flowStr flows
