{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Util.Dpdk (
        init_dpdk_setup,
        getPacket,
        sendPacket
) where

import Foreign (Ptr)
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (mallocForeignPtrBytes,
                           withForeignPtr)

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..) )

import Data.Word (Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

-- opaque tap handler
data TapHandle
newtype Tap = Tap (Ptr TapHandle)

foreign import ccall "tap_create"
	c_tap_create :: CString -> IO Tap

foreign import ccall "tap_up"
	c_tap_up :: Tap -> IO ()

foreign import ccall "tap_set_ip"
	c_tap_set_ip :: Tap -> CString -> IO ()

foreign import ccall "tap_set_mask"
	c_tap_set_mask :: Tap -> CString -> IO ()

foreign import ccall "tap_read"
	c_tap_read :: Tap -> Ptr Word8 -> CInt -> IO (CInt)

foreign import ccall "tap_write"
	c_tap_write :: Tap -> Ptr Word8 -> CInt -> IO ()

create :: String -> IO Tap
create name = withCString name c_tap_create

set_ip :: Tap -> String -> IO ()
set_ip tap ip = withCString ip (\x -> c_tap_set_ip tap x)

set_mask :: Tap -> String -> IO ()
set_mask tap mask = withCString mask (\x -> c_tap_set_mask tap x)

up :: Tap -> IO ()
up = c_tap_up

-- read a bytestring
readbs :: Tap -> IO BS.ByteString
readbs tap = do
	buf <- mallocForeignPtrBytes blen
	len <- withForeignPtr buf $ \b -> c_tap_read tap b c_blen
	case len of
		0 -> error "c_tap_read() should not return an empty buffer"
		_ -> let len'    = fromIntegral len
		         bytestr = BI.fromForeignPtr buf 0 len'
		     in return bytestr
	where
		blen = 4096
		c_blen = fromIntegral blen

-- write a bytestring
writebs :: Tap -> BS.ByteString -> IO ()
writebs tap bstr = do
	withForeignPtr fptr $ \ptr -> c_tap_write tap (ptr `plusPtr` off) c_len
	where
		(fptr, off, len) = BI.toForeignPtr bstr
		c_len = fromIntegral len

-- ##########################################################################

-- opaque tap handler
data DpdkHandle
newtype Dpdk = Dpdk (Ptr DpdkHandle)

foreign import ccall "init_dpdk_setup"
	c_init_dpdk_setup :: CString -> IO Dpdk

foreign import ccall "get_packet"
	c_get_packet :: Dpdk -> Ptr Word8 -> CInt -> IO (CInt)

foreign import ccall "send_packet"
	c_send_packet :: Dpdk -> Ptr Word8 -> CInt -> IO ()

-- Create a DPDK device
init_dpdk_setup :: String -> IO Dpdk
init_dpdk_setup name = withCString name c_init_dpdk_setup

--
getPacket :: Dpdk -> IO BS.ByteString
getPacket dpdk = do
	buf <- mallocForeignPtrBytes blen
	len <- withForeignPtr buf $ \b -> c_get_packet dpdk b c_blen
	case len of
		0 -> error "c_get_packet() should not return an empty buffer"
		_ -> let len'    = fromIntegral len
		         bytestr = BI.fromForeignPtr buf 0 len'
		     in return bytestr
	where
		blen = 4096
		c_blen = fromIntegral blen

-- write a bytestring
sendPacket:: Dpdk -> BS.ByteString -> IO ()
sendPacket dpdk bstr = do
	withForeignPtr fptr $ \ptr -> c_send_packet dpdk (ptr `plusPtr` off) c_len
	where
		(fptr, off, len) = BI.toForeignPtr bstr
		c_len = fromIntegral len


