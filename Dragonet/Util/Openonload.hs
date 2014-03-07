{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Util.Openonload (
        init_openonload_setup
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

import Data.Word (Word8)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

-- ##########################################################################


type Vif = Ptr ()

-- C function bindings for device setup
foreign import ccall "init_openonload_setup"
	c_init_openonload_setup ::  CString -> IO (Vif)

-- C function bindings for sending and receiving packets
foreign import ccall "get_packet"
	c_get_packet :: Vif -> Ptr Word8 -> CInt -> IO (CInt)

foreign import ccall "send_packet"
	c_send_packet :: Vif -> Ptr Word8 -> CInt -> IO ()

-- ##########################################################################


-- Create a openonload device (from given interface)
--  eg  ``init_openonload_setup  "eth7"``
init_openonload_setup :: String -> IO (Vif)
init_openonload_setup ifname = do
        ifnamec <- newCString ifname
        c_init_openonload_setup ifnamec

--
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

