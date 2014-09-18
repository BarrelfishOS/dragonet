module Dragonet.Flows (
    Flow(..)
) where

-- Flow definitions

import Data.Word

type IPv4Addr = Word32
type UDPPort  = Word16

data Flow =
  -- UDP connection
  FlowUDPv4Conn {
    flSrcIp    :: IPv4Addr,
    flDstIp    :: IPv4Addr,
    flDstPort  :: UDPPort,
    flSrcPort  :: UDPPort
  } |
  -- UDP listen socket
  FlowUDPv4List {
    flIp     :: Maybe IPv4Addr,
    flPort   :: UDPPort
  }
  deriving (Show, Eq, Ord)
