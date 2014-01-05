{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Util.Dpdk (
        init_dpdk_setup_v2
        , getPacket_v2
        , sendPacket_v2
        , exec_control_command

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

import Text.Printf

--import Text.Format

import Data.Word (Word8)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

-- ##########################################################################

-- C function bindings for device setup
foreign import ccall "init_dpdk_setupV2"
	c_init_dpdk_setup_v2 :: IO (CInt)

-- C function bindings for sending and receiving packets
foreign import ccall "get_packetV2"
	c_get_packet_v2 :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO (CInt)

foreign import ccall "send_packetV2"
	c_send_packet_v2 :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO ()

-- ##########################################################################

-- C function bindings for sending and receiving packets
foreign import ccall "exec_control_command"
	c_exec_control_command :: CString -> IO (CInt)

-- ##########################################################################



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

-- ##########################################################################

-- Create a DPDK device
init_dpdk_setup_v2 :: IO CInt
init_dpdk_setup_v2 =  c_init_dpdk_setup_v2

--
getPacket_v2 :: Int -> Int -> Int -> IO BS.ByteString
getPacket_v2 coreid portid queueid = do
	buf <- mallocForeignPtrBytes blen
	len <- withForeignPtr buf $ \b -> c_get_packet_v2 c_coreid c_portid c_queueid b c_blen
	case len of
		0 -> error "c_get_packet() should not return an empty buffer"
		_ -> let len'    = fromIntegral len
		         bytestr = BI.fromForeignPtr buf 0 len'
		     in return bytestr
	where
		blen = 4096
		c_blen = fromIntegral blen
		c_coreid = fromIntegral coreid
		c_portid = fromIntegral portid
		c_queueid = fromIntegral queueid

-- write a bytestring
sendPacket_v2:: Int -> Int -> Int -> BS.ByteString -> IO ()
sendPacket_v2 coreid portid queueid bstr = do
	withForeignPtr fptr $ \ptr -> c_send_packet_v2 c_coreid c_portid c_queueid (ptr `plusPtr` off) c_len
	where
		(fptr, off, len) = BI.toForeignPtr bstr
		c_len = fromIntegral len
		c_coreid = fromIntegral coreid
		c_portid = fromIntegral portid
		c_queueid = fromIntegral queueid

-- ###############################################################

--exec_control_command :: String -> IO Int
exec_control_command :: String -> IO ()
exec_control_command cmd = do
    ret <- withCString cmd (\x -> c_exec_control_command x)
    let msg = case (ret >= 0) of
            True -> show ("Command executioned successfully [" ++ (show ret)
                ++ "] : " ++ (show cmd))
            False -> show ("Command execution failed [" ++ (show ret)
                ++ "] : " ++ (show cmd))
    putStrLn (msg)

--    msg
--        | ret > 0 = show ("Command executioned successfully [" ++ (show ret)
--                ++ "] : " ++ (show cmd))
--        | otherwise = show ("Command execution failed [" ++ (show ret)
--                ++ "] : " ++ (show cmd))
--    in putStrLn (msg)
--


--    case ret of
--        > 0 -> putStrLn ("Command executioned successfully [" ++ (show ret)
--                ++ "] : " ++ (show cmd))
--        _ -> putStrLn ("Command execution failed [" ++ (show ret)
--                ++ "] : " ++ (show cmd))

--    let ret' = fromIntegral ret
--    in return ret'


-- ###############################################################


e10k5TAdd ::  Int -> String -> Word16 -> String -> Word16 -> Int -> IO ()
e10k5TAdd qid srcIP srcPort dstIP dstPort tp = do
    let
        ethPort = 0
        proto = case tp of
            1 -> "tcp"
            otherwise -> "udp"
        cmd2 = ("add_perfect_filter " ++ (show ethPort) ++ " " ++ proto
            ++ " src " ++ srcIP ++ " " ++ (show srcPort)
            ++ " dst " ++ dstIP ++ " " ++ (show dstPort)
            ++ " flexbytes 0 "
            ++ " vlan 0 "
            ++ " queue " ++ (show qid)
            ++ " soft 0"
            ++ "\r\n" )
    exec_control_command cmd2
    putStrLn ("e10k5TAdd: cmd: " ++ (show cmd2))

e10k5TAdd2 :: Int -> IO()
e10k5TAdd2 tid = do
    let
        c_tid = fromIntegral tid
    len <- c_fdir_add_perfect_filter2_wrapper c_tid
    putStrLn ("e10k5TAdd2: tid " ++ (show tid))


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

-- ##########################################################################

