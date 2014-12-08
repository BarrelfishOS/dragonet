{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonet.NetState (
    EndpointId, SocketId, AppId,
    IPv4Addr,
    UDPPort, UDPEndpoint, LocalUDPEndpoint, RemoteUDPEndpoint,
    EndpointDesc(..),
    epAddSocket,
    epsDiff,
    allocAppId,
    runState, runState0,
    NetState(..), NetStateM, initNetSt,
    udpListen, udpBind, socketSpan, mkUdpEndpoint,
) where

-- basic network state logic:
--  - applications
--  - endpoints
--  - sockets
-- This is the part that does not depend on the stack implementation

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST

import Data.Word (Word16,Word32,Word64)

type EndpointId = Int
type SocketId   = Word64
type AppId      = Word64

type IPv4Addr   = Word32
type UDPPort    = Word16

type UDPEndpoint = (IPv4Addr,UDPPort)
type LocalUDPEndpoint = UDPEndpoint
type RemoteUDPEndpoint = UDPEndpoint

-- Endpoint descriptor
data EndpointDesc = EndpointUDPv4 {
    -- an endpoint might be connected to multiple application sockets
    epSockets    :: [(SocketId, AppId)],
    epLocalIp    :: Maybe IPv4Addr,
    epRemoteIp   :: Maybe IPv4Addr,
    epLocalPort  :: Maybe UDPPort,
    epRemotePort :: Maybe UDPPort
} deriving (Show, Eq, Ord)

data NetState = NetState {
      nsNextAppId      :: AppId  -- id for the next app that tries to connect
    , nsNextSocketId   :: SocketId -- id for next socket
    , nsNextEndpointId :: EndpointId -- id for next endpoint
    , nsEndpoints      :: M.Map EndpointId EndpointDesc
    , nsSockets        :: M.Map SocketId EndpointId
}

newtype NetStateM a = NetStateM { unNetStateM :: ST.State NetState a }
    deriving (Monad, ST.MonadState NetState, Functor)

runState :: NetStateM a -> NetState -> (a, NetState)
runState m ns = ST.runState (unNetStateM m) ns

runState0 :: NetStateM a -> (a, NetState)
runState0 m = runState m initNetSt

epAddSocket :: (SocketId,AppId) -> EndpointDesc -> EndpointDesc
epAddSocket (sid,aid) ep = ep {epSockets = (sid,aid):(epSockets ep)}

epsDiff :: [EndpointDesc] -> [EndpointDesc] -> [EndpointDesc]
epsDiff es1 es2 = S.toList $ S.difference (S.fromList es1) (S.fromList es2)


initNetSt = NetState {
    nsNextAppId      = 1,
    nsNextSocketId   = 1,
    nsNextEndpointId = 1,
    nsEndpoints      = M.empty,
    nsSockets        = M.empty
}

allocAppId :: NetStateM AppId
allocAppId = do
    aid <- ST.gets nsNextAppId
    ST.modify $ \s -> s { nsNextAppId = aid + 1 }
    return aid

allocSocketId :: NetStateM SocketId
allocSocketId = do
    sid <- ST.gets nsNextSocketId
    ST.modify $ \s -> s { nsNextSocketId = sid + 1 }
    return sid

allocEndpointId :: NetStateM EndpointId
allocEndpointId = do
    eid <- ST.gets nsNextEndpointId
    ST.modify $ \s -> s { nsNextEndpointId = eid + 1 }
    return eid

-- TODO: check for existing conflicting endpoints
addEndpoint :: EndpointDesc -> NetStateM EndpointId
addEndpoint ep = do
    eid <- allocEndpointId
    ST.modify $ \s -> s { nsEndpoints = M.insert eid ep $ nsEndpoints s }
    return eid

zeroAsNothing a = if a == 0 then Nothing else Just a

mkUdpEndpoint :: LocalUDPEndpoint -> RemoteUDPEndpoint -> EndpointDesc
mkUdpEndpoint (lip,lport) (rip,rport)=
    EndpointUDPv4 {
          epSockets    = []
        , epLocalIp    = zeroAsNothing lip
        , epRemoteIp   = zeroAsNothing rip
        , epLocalPort  = zeroAsNothing lport
        , epRemotePort = zeroAsNothing rport
    }

udpBind :: AppId
        -> (LocalUDPEndpoint, RemoteUDPEndpoint)
        -> NetStateM (SocketId, EndpointId)
udpBind aid ((lip,lport),(rip,rport)) = do
    sid <- allocSocketId
    let ep = (mkUdpEndpoint (lip,lport) (rip,rport))
             {epSockets = [(sid,aid)]}
    eid <- addEndpoint ep
    ST.modify $ \s -> s { nsSockets = M.insert sid eid $ nsSockets s }
    return (sid,eid)

udpListen :: AppId
           -> LocalUDPEndpoint
           -> NetStateM (SocketId, EndpointId)
udpListen aid x = udpBind aid (x,(0,0))

socketSpan :: AppId -> SocketId -> NetStateM (SocketId)
socketSpan aid sid = do
    -- NB: we should check if spanning is allowed (e.g., same application)
    Just eid <- M.lookup sid <$> ST.gets nsSockets
    sid'     <- allocSocketId
    -- add new socket to the endpoint
    let updF = epAddSocket (sid',aid)
    ST.modify $ \s -> s { nsEndpoints = M.adjust updF eid (nsEndpoints s) }
    return sid'
