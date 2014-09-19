module Dragonet.Flows (
    Flow(..)
) where

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
