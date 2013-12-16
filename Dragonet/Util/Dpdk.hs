{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Util.Dpdk (
        init_dpdk_setup
        , getPacket
        , sendPacket

        , e10k5TAdd
        , e10k5TDel
        , e10kFDirAdd
        , e10kFDirDel
) where

import Foreign (Ptr)
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (mallocForeignPtrBytes,
                           withForeignPtr)

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..) )

import Data.Word (Word8)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

-- ##########################################################################

-- opaque tap handler
data DpdkHandle
newtype Dpdk = Dpdk (Ptr DpdkHandle)

-- C function bindings for device setup
foreign import ccall "init_dpdk_setup"
	c_init_dpdk_setup :: CString -> IO Dpdk

-- C function bindings for sending and receiving packets
foreign import ccall "get_packet"
	c_get_packet :: Dpdk -> Ptr Word8 -> CInt -> IO (CInt)

foreign import ccall "send_packet"
	c_send_packet :: Dpdk -> Ptr Word8 -> CInt -> IO ()

-- C function bindings for managing hardware queues
foreign import ccall "fdir_add_perfect_filter_wrapper"
	c_fdir_add_perfect_filter_wrapper :: CInt -- qid
        -> Ptr Word8  -- srcIP
        -> CInt -- srcPort
        -> Ptr Word8 -- dstIP
        -> CInt -- dstPort
        -> CInt -- type
        -> IO (CInt) -- Return value: sucess/failure

foreign import ccall "fdir_add_perfect_filter2_wrapper_dummy"
	c_fdir_add_perfect_filter2_wrapper :: CInt -- qid
        -> IO (CInt) -- Return value: sucess/failure

foreign import ccall "fdir_del_perfect_filter_wrapper_dummy"
	c_fdir_del_perfect_filter_wrapper :: CInt -- qid
        -> IO (CInt) -- Return value: sucess/failure

foreign import ccall "fdir_add_flow_filter_wrapper_dummy"
	c_fdir_add_flow_filter_wrapper :: CInt -- qid
        -> IO (CInt) -- Return value: sucess/failure

foreign import ccall "fdir_del_flow_filter_wrapper_dummy"
	c_fdir_del_flow_filter_wrapper :: CInt -- qid
        -> IO (CInt) -- Return value: sucess/failure


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

-- ###############################################################

e10k5TAdd ::  Int -> String -> Word16 -> String -> Word16 -> Int -> IO ()
e10k5TAdd qid srcIP srcPort dstIP dstPort tp = do
    let
        c_qid = fromIntegral qid
    len <- c_fdir_add_perfect_filter2_wrapper c_qid
    putStrLn ("e10k5TAdd2: qid: " ++ (show qid)
            ++ ", srcIP: " ++ (show srcIP)
            ++ ", srcPort: " ++ (show srcPort)
            ++ ", dstIP: " ++ (show dstIP)
            ++ ", dstPort: " ++ (show dstPort)
            ++ ", type: " ++ (show tp))

e10k5TAdd2 :: Int -> IO()
e10k5TAdd2 tid = do
    let
        c_tid = fromIntegral tid
    len <- c_fdir_add_perfect_filter2_wrapper c_tid
    putStrLn ("e10k5TAdd: tid " ++ (show tid))


e10k5TDel :: Int -> IO()
e10k5TDel tid = do
    let
        c_tid = fromIntegral tid
    len <- c_fdir_del_perfect_filter_wrapper c_tid
    putStrLn ("e10k5TDel: tid " ++ (show tid))

e10kFDirAdd :: Int -> IO()
e10kFDirAdd tid = do
    let
        c_tid = fromIntegral tid
    len <- c_fdir_add_flow_filter_wrapper c_tid
    putStrLn ("e10kFDirAdd : tid " ++ (show tid))

e10kFDirDel :: Int -> IO()
e10kFDirDel tid = do
    let
        c_tid = fromIntegral tid
    len <- c_fdir_del_flow_filter_wrapper c_tid
    putStrLn ("e10kFDirDel : tid " ++ (show tid))


