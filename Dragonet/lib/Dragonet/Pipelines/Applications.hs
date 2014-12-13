{-# LANGUAGE ForeignFunctionInterface #-}
module Dragonet.Pipelines.Applications (
    ChanHandle,

    Event(..),
    TxMessage(..),
    appFlagsMore,

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


-- This should match the C version in app_control.h
type AppFlags = Word32
appFlagsMore  = 0 :: Int -- bit zero

type ChanHandle = Int

instance Show GraphHandle where
    show _ = "GraphHandle ()"

data Event =
    EvAppConnected GraphHandle |
    EvAppRegister String AppFlags|
    EvAppStopped Bool | -- The boolean indicates regular termination
    EvSocketUDPBind UDPEndpoint UDPEndpoint AppFlags |
    EvSocketUDPFlow SocketId UDPEndpoint UDPEndpoint AppFlags |
    EvSocketSpan SocketId AppFlags |
    EvSocketClose SocketId AppFlags |
    EvNop AppFlags
    deriving (Show)

data TxMessage =
    MsgWelcome AppId |
    MsgStatus Bool |
    MsgSocketInfo SocketId
    deriving (Show,Eq,Ord)


-- These should match the matching functions in app_control.h
type OpNewApp = ChanHandle -> GraphHandle -> IO ()
type OpRegister = ChanHandle -> CString -> AppFlags -> IO ()
type OpStopApp = ChanHandle -> Bool -> IO ()
-- (bind: local ip, local port, remote ip, remote port)
type OpUDPBind = ChanHandle -> IPv4Addr -> UDPPort
                            -> IPv4Addr -> UDPPort
                            -> AppFlags
                            -> IO ()
-- (flow: socket id, local ip, local port, remote ip, remote port)
type OpUDPFlow = ChanHandle -> SocketId
                            -> IPv4Addr -> UDPPort
                            -> IPv4Addr -> UDPPort
                            -> AppFlags
                            -> IO ()
type OpSocketSpan  = ChanHandle -> SocketId -> AppFlags -> IO ()
type OpSocketClose = ChanHandle -> SocketId -> AppFlags -> IO ()
type OpNop = ChanHandle -> AppFlags -> IO ()

foreign import ccall "app_control_init"
    c_app_control_init :: CString
                       -> FunPtr OpNewApp
                       -> FunPtr OpRegister
                       -> FunPtr OpStopApp
                       -> FunPtr OpUDPBind
                       -> FunPtr OpUDPFlow
                       -> FunPtr OpSocketSpan
                       -> FunPtr OpSocketClose
                       -> FunPtr OpNop
                       -> IO ()

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
foreign import ccall "wrapper"
    mkOpNop :: OpNop -> IO (FunPtr OpNop)

{-|
 - Calls the c function "app_control_init" with proper agruments.
 - NOTE: keep in mind that the C function has an infinite loop
 - handling event.  So, this function will never return.
 -}
appControlInit :: String
               -> OpNewApp
               -> OpRegister
               -> OpStopApp
               -> OpUDPBind
               -> OpUDPFlow
               -> OpSocketSpan
               -> OpSocketClose
               -> OpNop
               -> IO ()
appControlInit sn a b c d e f g h = do
    a' <- mkOpNewApp a
    b' <- mkOpRegister b
    c' <- mkOpStopApp c
    d' <- mkOpUDPBind d
    e' <- mkOpUDPFlow e
    f' <- mkOpSocketSpan f
    g' <- mkOpSocketClose g
    h' <- mkOpNop h
    withCString sn $ \sn' ->
        c_app_control_init sn' a' b' c' d' e' f' g' h'


hOpNewApp :: (ChanHandle -> Event -> IO ()) -> OpNewApp
hOpNewApp eh ch gh = eh ch $ EvAppConnected gh

hOpRegister :: (ChanHandle -> Event -> IO ()) -> OpRegister
hOpRegister eh ch cs fl = do
    s <- peekCString cs
    eh ch $ EvAppRegister s fl

hOpStopApp :: (ChanHandle -> Event -> IO ()) -> OpStopApp
hOpStopApp eh ch reg = eh ch $ EvAppStopped reg

hOpUDPBind :: (ChanHandle -> Event -> IO ()) -> OpUDPBind
hOpUDPBind eh ch l_ip l_port r_ip r_port fl =
    eh ch $ EvSocketUDPBind (l_ip, l_port) (r_ip, r_port) fl

hOpUDPFlow :: (ChanHandle -> Event -> IO ()) -> OpUDPFlow
hOpUDPFlow eh ch sid l_ip l_port r_ip r_port fl =
    eh ch $ EvSocketUDPFlow sid (l_ip, l_port) (r_ip, r_port) fl

hOpSocketSpan :: (ChanHandle -> Event -> IO ()) -> OpSocketSpan
hOpSocketSpan eh ch si fl = eh ch $ EvSocketSpan si fl

hOpSocketClose :: (ChanHandle -> Event -> IO ()) -> OpSocketClose
hOpSocketClose eh ch si fl = eh ch $ EvSocketClose si fl

hOpNop :: (ChanHandle -> Event -> IO ()) ->  OpNop
hOpNop eh ch fl = eh ch $ EvNop fl

{-|
 - function 'interfaceThread' runs the **infinite** event loop using the
 -  event handlers provided.  Note that this just calls the C equivalant
 -  function which will never return.
 -}
interfaceThread :: String -> (ChanHandle -> Event -> IO ()) -> IO ()
interfaceThread stackname eh = do
    appControlInit stackname
                   (hOpNewApp eh)
                   (hOpRegister eh)
                   (hOpStopApp eh)
                   (hOpUDPBind eh)
                   (hOpUDPFlow eh)
                   (hOpSocketSpan eh)
                   (hOpSocketClose eh)
                   (hOpNop eh)

sendMessage :: ChanHandle -> TxMessage -> IO ()
sendMessage ch (MsgWelcome appid) = c_app_control_send_welcome ch appid
sendMessage ch (MsgStatus status) = c_app_control_send_status ch status
sendMessage ch (MsgSocketInfo sid) =
    c_app_control_send_socket_info ch sid

