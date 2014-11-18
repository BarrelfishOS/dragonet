module Dragonet.Endpoint (
    SocketId,
    AppId,
    IPv4Addr,
    UDPPort,
    EndpointDesc(..)
) where


import Data.Word (Word16,Word32,Word64)

type SocketId = Word64
type AppId    = Word64
type IPv4Addr = Word32
type UDPPort  = Word16

-- Endpoint descriptor
data EndpointDesc = EndpointUDPIPv4 {
    -- an endpoint might have multiple application sockets
    edSockets :: [(SocketId, AppId)],
    edIP4Src :: Maybe IPv4Addr,
    edIP4Dst :: Maybe IPv4Addr,
    edUDPSrc :: Maybe UDPPort,
    edUDPDst :: Maybe UDPPort
} deriving (Show, Eq, Ord)
