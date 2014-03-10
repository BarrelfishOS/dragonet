{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Util.Openonload (
        init_openonload_setup
        , alloc_queue
        , alloc_filter_default
        , alloc_filter_full_ipv4
        , alloc_filter_listen_ipv4
        , getPacket
        , sendPacket
) where

import Foreign (Ptr)
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (mallocForeignPtrBytes,
                           withForeignPtr)

import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CInt(..) )

import Text.Printf

--import Text.Format

import qualified Data.Word as DW
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

-- ##########################################################################


type Vif = Ptr ()
type Netif = Ptr ()

-- C function bindings for device setup
foreign import ccall "init_openonload_setup"
	c_init_openonload_setup ::  CString -> IO (Netif)

-- C function bindings for allocating queue
foreign import ccall "alloc_queue"
	c_alloc_queue ::  Netif -> IO (Vif)

-- C function bindings binding default destination with a queue
foreign import ccall "alloc_filter_default"
	c_alloc_filter_default :: Vif -> IO(CInt)

-- C function bindings to allocate listen port
foreign import ccall "alloc_filter_listen_ipv4"
	c_alloc_filter_listen_ipv4 :: Vif -> CInt ->
                    DW.Word32 -> DW.Word16 -> IO(CInt)

{-
int alloc_filter_listen_ipv4(struct vi *vis, int protocol,
uint32_t localip, uint16_t localport);
-}

-- C function bindings to allocate full port
foreign import ccall "alloc_filter_full_ipv4"
	c_alloc_filter_full_ipv4 :: Vif -> CInt
                    -> DW.Word32 -> DW.Word16
                    -> DW.Word32 -> DW.Word16
                    -> IO(CInt)
{-
int alloc_filter_full_ipv4(struct vi *vis, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport);
-}

-- C function bindings for sending and receiving packets
foreign import ccall "get_packet"
	c_get_packet :: Vif -> Ptr DW.Word8 -> CInt -> IO (CInt)

foreign import ccall "send_packet"
	c_send_packet :: Vif -> Ptr DW.Word8 -> CInt -> IO ()

-- ##########################################################################


-- Create a openonload device (from given interface)
--  eg  ``init_openonload_setup  "eth7"``
init_openonload_setup :: String -> IO (Netif)
init_openonload_setup ifname = do
        ifnamec <- newCString ifname
        c_init_openonload_setup ifnamec

alloc_queue :: Netif -> IO (Vif)
alloc_queue ifptr = do
        c_alloc_queue ifptr

alloc_filter_default :: Vif -> IO()
alloc_filter_default vptr = do
    ret <- c_alloc_filter_default vptr

    let msg = case (ret >= 0) of
            True -> show ("Command executioned successfully [" ++ (show ret)
                ++ "] : alloc_filter_default")
            False -> show ("Command execution failed [" ++ (show ret)
                ++ "] : alloc_filter_default")
    putStrLn (msg)


alloc_filter_listen_ipv4 :: Vif -> CInt -> DW.Word32 -> DW.Word16 -> IO()
alloc_filter_listen_ipv4 vptr proto localIp localPort = do
    ret <- c_alloc_filter_listen_ipv4 vptr proto localIp localPort
    let msg = case (ret >= 0) of
            True -> show ("Command executioned successfully [" ++ (show ret)
                ++ "] :alloc_filter_listen_ipv4")
            False -> show ("Command execution failed [" ++ (show ret)
                ++ "]alloc_filter_listen_ipv4")
    putStrLn (msg)



alloc_filter_full_ipv4 :: Vif -> CInt
            -> DW.Word32 -> DW.Word16
            -> DW.Word32 -> DW.Word16
            -> IO()
alloc_filter_full_ipv4 vptr proto localIp localPort remoteIp remotePort = do
    ret <- c_alloc_filter_full_ipv4 vptr proto localIp localPort remoteIp remotePort
    let msg = case (ret >= 0) of
            True -> show ("Command executioned successfully [" ++ (show ret)
                ++ "] : alloc_filter_full_ipv4")
            False -> show ("Command execution failed [" ++ (show ret)
                ++ "] : alloc_filter_full_ipv4")
    putStrLn (msg)


getPacket :: Vif -> IO BS.ByteString
getPacket vi = do
	buf <- mallocForeignPtrBytes blen
	len <- withForeignPtr buf $ \b -> c_get_packet vi b c_blen
	case len of
		0 -> error "c_get_packet() should not return an empty buffer"
		_ -> let len'    = fromIntegral len
		         bytestr = BI.fromForeignPtr buf 0 len'
		     in return bytestr
	where
		blen = 4096
		c_blen = fromIntegral blen


-- write a bytestring
sendPacket:: Vif -> BS.ByteString -> IO ()
sendPacket vi bstr = do
	withForeignPtr fptr $ \ptr -> c_send_packet vi (ptr `plusPtr` off) c_len
	where
		(fptr, off, len) = BI.toForeignPtr bstr
		c_len = fromIntegral len

-- ###############################################################

