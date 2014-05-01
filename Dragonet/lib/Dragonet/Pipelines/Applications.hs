{-# LANGUAGE ForeignFunctionInterface #-}
module Dragonet.Pipelines.Applications (
    ChanHandle,
    SocketId,
    AppId,
    IPv4Addr,
    UDPPort,
    UDPEndpoint,

    Event(..),
    TxMessage(..),

    interfaceThread,
    sendMessage
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Data.Word
import Data.Int

type ChanHandle = Int
type SocketId = Word64
type AppId = Word64
type IPv4Addr = Word32
type UDPPort = Word16
type UDPEndpoint = (IPv4Addr,UDPPort)

data Event =
    EvAppRegister |
    EvAppStopped Bool | -- The boolean indicates regular termination
    EvSocketUDPListen SocketId UDPEndpoint |
    EvSocketUDPFlow SocketId UDPEndpoint UDPEndpoint |
    EvSocketClose SocketId
    deriving (Show,Eq,Ord)

data TxMessage =
    MsgWelcome AppId Int Int |
    MsgInQueue String |
    MsgOutQueue String |
    MsgStatus Bool |
    MsgSocketInfo Word8 Int32
    deriving (Show,Eq,Ord)


type OpNewApp = ChanHandle -> IO ()
type OpStopApp = ChanHandle -> Bool -> IO ()
type OpUDPListen = ChanHandle -> SocketId -> IPv4Addr -> UDPPort -> IO ()
type OpUDPFlow = ChanHandle -> SocketId -> IPv4Addr -> UDPPort -> IPv4Addr ->
                    UDPPort -> IO ()
type OpSocketClose = ChanHandle -> SocketId -> IO ()

foreign import ccall "app_control_init"
    c_app_control_init ::
        CString -> FunPtr OpNewApp -> FunPtr OpStopApp -> FunPtr OpUDPListen ->
            FunPtr OpUDPFlow -> FunPtr OpSocketClose -> IO ()
foreign import ccall "app_control_send_welcome"
    c_app_control_send_welcome ::
        ChanHandle -> AppId -> Word8 -> Word8 -> IO ()
foreign import ccall "app_control_send_status"
    c_app_control_send_status ::
        ChanHandle -> Bool -> IO ()
foreign import ccall "app_control_send_queue"
    c_app_control_send_queue ::
        ChanHandle -> Bool -> CString -> IO ()
foreign import ccall "app_control_send_socket_info"
    c_app_control_send_socket_info ::
        ChanHandle -> Word8 -> Int32 -> IO ()


foreign import ccall "wrapper"
    mkOpNewApp :: OpNewApp -> IO (FunPtr OpNewApp)
foreign import ccall "wrapper"
    mkOpStopApp :: OpStopApp -> IO (FunPtr OpStopApp)
foreign import ccall "wrapper"
    mkOpUDPListen :: OpUDPListen -> IO (FunPtr OpUDPListen)
foreign import ccall "wrapper"
    mkOpUDPFlow :: OpUDPFlow -> IO (FunPtr OpUDPFlow)
foreign import ccall "wrapper"
    mkOpSocketClose :: OpSocketClose -> IO (FunPtr OpSocketClose)

appControlInit :: String -> OpNewApp -> OpStopApp -> OpUDPListen -> OpUDPFlow ->
                        OpSocketClose -> IO ()
appControlInit sn a b c d e = do
    a' <- mkOpNewApp a
    b' <- mkOpStopApp b
    c' <- mkOpUDPListen c
    d' <- mkOpUDPFlow d
    e' <- mkOpSocketClose e
    withCString sn $ \sn' ->
        c_app_control_init sn' a' b' c' d' e'


hOpNewApp :: (ChanHandle -> Event -> IO ()) -> OpNewApp
hOpNewApp eh ch = eh ch EvAppRegister

hOpStopApp :: (ChanHandle -> Event -> IO ()) -> OpStopApp
hOpStopApp eh ch reg = eh ch $ EvAppStopped reg

hOpUDPListen :: (ChanHandle -> Event -> IO ()) -> OpUDPListen
hOpUDPListen eh ch si ip port = eh ch $ EvSocketUDPListen si (ip,port)

hOpUDPFlow :: (ChanHandle -> Event -> IO ()) -> OpUDPFlow
hOpUDPFlow eh ch si s_ip s_port d_ip d_port =
    eh ch $ EvSocketUDPFlow si (s_ip, s_port) (d_ip, d_port)

hOpSocketClose :: (ChanHandle -> Event -> IO ()) -> OpSocketClose
hOpSocketClose eh ch si = eh ch $ EvSocketClose si


interfaceThread :: String -> (ChanHandle -> Event -> IO ()) -> IO ()
interfaceThread stackname eh = do
    appControlInit stackname (hOpNewApp eh) (hOpStopApp eh) (hOpUDPListen eh)
        (hOpUDPFlow eh) (hOpSocketClose eh)

sendMessage :: ChanHandle -> TxMessage -> IO ()
sendMessage ch (MsgWelcome appid i o) =
    c_app_control_send_welcome ch appid i' o'
    where (i',o') = (fromIntegral i, fromIntegral o)
sendMessage ch (MsgOutQueue l) =
    withCString l $ \l' -> c_app_control_send_queue ch True l'
sendMessage ch (MsgInQueue l) =
    withCString l $ \l' -> c_app_control_send_queue ch False l'
sendMessage ch (MsgStatus status) = c_app_control_send_status ch status
sendMessage ch (MsgSocketInfo oq mux) = c_app_control_send_socket_info ch oq mux

