module Dragonet.Endpoint (
    SocketId,
    AppId,
    IPv4Addr,
    UDPPort,
    EndpointDesc(..),
    epsDiff,
) where


import qualified Data.Set as S
import Data.Word (Word16,Word32,Word64)

type SocketId = Word64
type AppId    = Word64
type IPv4Addr = Word32
type UDPPort  = Word16

-- Endpoint descriptor
data EndpointDesc = EndpointUDPv4 {
    -- an endpoint might be connected to multiple application sockets
    epSockets    :: [(SocketId, AppId)],
    epLocalIp    :: Maybe IPv4Addr,
    epRemoteIp   :: Maybe IPv4Addr,
    epLocalPort  :: Maybe UDPPort,
    epRemotePort :: Maybe UDPPort
} deriving (Show, Eq, Ord)


epsDiff :: [EndpointDesc] -> [EndpointDesc] -> [EndpointDesc]
epsDiff es1 es2 = S.toList $ S.difference (S.fromList es1) (S.fromList es2)

