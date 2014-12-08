{-# LANGUAGE ForeignFunctionInterface #-}
module Dragonet.Pipelines.Applications (
    ChanHandle,

    Event(..),
    TxMessage(..),

    interfaceThread,
    sendMessage
) where

import Dragonet.NetState (SocketId,AppId,IPv4Addr,UDPPort, UDPEndpoint)

import Dragonet.Pipelines.Implementation (GraphHandle(..))

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Data.Word
import Data.Int

type ChanHandle = Int

instance Show GraphHandle where
    show _ = "GraphHandle ()"

data Event =
    EvAppConnected GraphHandle |
    EvAppRegister String |
    EvAppStopped Bool | -- The boolean indicates regular termination
    EvSocketUDPBind UDPEndpoint UDPEndpoint |
    EvSocketUDPFlow UDPEndpoint UDPEndpoint |
    EvSocketSpan SocketId |
    EvSocketClose SocketId
    deriving (Show)

data TxMessage =
    MsgWelcome AppId |
    MsgStatus Bool |
    MsgSocketInfo SocketId
    deriving (Show,Eq,Ord)


type OpNewApp = ChanHandle -> GraphHandle -> IO ()
type OpRegister = ChanHandle -> CString -> IO ()
type OpStopApp = ChanHandle -> Bool -> IO ()
-- (bind: local ip, local port, remote ip, remote port)
type OpUDPBind = ChanHandle -> IPv4Addr -> UDPPort
                            -> IPv4Addr -> UDPPort
                            -> IO ()
-- (flow: local ip, local port, remote ip, remote port)
type OpUDPFlow = ChanHandle -> IPv4Addr -> UDPPort -> IPv4Addr ->
                    UDPPort -> IO ()
type OpSocketSpan = ChanHandle -> SocketId -> IO ()
type OpSocketClose = ChanHandle -> SocketId -> IO ()

foreign import ccall "app_control_init"
    c_app_control_init ::
        CString -> FunPtr OpNewApp -> FunPtr OpRegister -> FunPtr OpStopApp ->
            FunPtr OpUDPBind -> FunPtr OpUDPFlow -> FunPtr OpSocketSpan ->
            FunPtr OpSocketClose -> IO ()
foreign import ccall "app_control_send_welcome"
    c_app_control_send_welcome ::
        ChanHandle -> AppId -> IO ()
foreign import ccall "app_control_send_status"
    c_app_control_send_status ::
        ChanHandle -> Bool -> IO ()
foreign import ccall "app_control_send_socket_info"
    c_app_control_send_socket_info ::
        ChanHandle -> SocketId -> IO ()


foreign import ccall "wrapper"
    mkOpNewApp :: OpNewApp -> IO (FunPtr OpNewApp)
foreign import ccall "wrapper"
    mkOpRegister :: OpRegister -> IO (FunPtr OpRegister)
foreign import ccall "wrapper"
    mkOpStopApp :: OpStopApp -> IO (FunPtr OpStopApp)
foreign import ccall "wrapper"
    mkOpUDPBind :: OpUDPBind -> IO (FunPtr OpUDPBind)
foreign import ccall "wrapper"
    mkOpUDPFlow :: OpUDPFlow -> IO (FunPtr OpUDPFlow)
foreign import ccall "wrapper"
    mkOpSocketSpan :: OpSocketSpan -> IO (FunPtr OpSocketSpan)
foreign import ccall "wrapper"
    mkOpSocketClose :: OpSocketClose -> IO (FunPtr OpSocketClose)

{-|
 - Calls the c function "app_control_init" with proper agruments.
 - NOTE: keep in mind that the C function has an infinite loop
 - handling event.  So, this function will never return.
 -}
appControlInit :: String -> OpNewApp -> OpRegister -> OpStopApp -> OpUDPBind
        -> OpUDPFlow -> OpSocketSpan -> OpSocketClose -> IO ()
appControlInit sn a b c d e f g = do
    a' <- mkOpNewApp a
    b' <- mkOpRegister b
    c' <- mkOpStopApp c
    d' <- mkOpUDPBind d
    e' <- mkOpUDPFlow e
    f' <- mkOpSocketSpan f
    g' <- mkOpSocketClose g
    withCString sn $ \sn' ->
        c_app_control_init sn' a' b' c' d' e' f' g'


hOpNewApp :: (ChanHandle -> Event -> IO ()) -> OpNewApp
hOpNewApp eh ch gh = eh ch $ EvAppConnected gh

hOpRegister :: (ChanHandle -> Event -> IO ()) -> OpRegister
hOpRegister eh ch cs = do
    s <- peekCString cs
    eh ch $ EvAppRegister s

hOpStopApp :: (ChanHandle -> Event -> IO ()) -> OpStopApp
hOpStopApp eh ch reg = eh ch $ EvAppStopped reg

hOpUDPBind :: (ChanHandle -> Event -> IO ()) -> OpUDPBind
hOpUDPBind eh ch l_ip l_port r_ip r_port =
    eh ch $ EvSocketUDPBind (l_ip, l_port) (r_ip, r_port)

hOpUDPFlow :: (ChanHandle -> Event -> IO ()) -> OpUDPFlow
hOpUDPFlow eh ch l_ip l_port r_ip r_port =
    eh ch $ EvSocketUDPFlow (l_ip, l_port) (r_ip, r_port)

hOpSocketSpan :: (ChanHandle -> Event -> IO ()) -> OpSocketClose
hOpSocketSpan eh ch si = eh ch $ EvSocketSpan si

hOpSocketClose :: (ChanHandle -> Event -> IO ()) -> OpSocketClose
hOpSocketClose eh ch si = eh ch $ EvSocketClose si


{-|
 - function 'interfaceThread' runs the **infinite** event loop using the
 -  event handlers provided.  Note that this just calls the C equivalant
 -  function which will never return.
 -}
interfaceThread :: String -> (ChanHandle -> Event -> IO ()) -> IO ()
interfaceThread stackname eh = do
    appControlInit stackname (hOpNewApp eh) (hOpRegister eh) (hOpStopApp eh)
        (hOpUDPBind eh) (hOpUDPFlow eh) (hOpSocketSpan eh) (hOpSocketClose eh)

sendMessage :: ChanHandle -> TxMessage -> IO ()
sendMessage ch (MsgWelcome appid) = c_app_control_send_welcome ch appid
sendMessage ch (MsgStatus status) = c_app_control_send_status ch status
sendMessage ch (MsgSocketInfo sid) =
    c_app_control_send_socket_info ch sid

