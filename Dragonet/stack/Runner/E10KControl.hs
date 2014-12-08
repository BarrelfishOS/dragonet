module Runner.E10KControl (
    FTuple(..),
    FDirTuple(..),
    L3Proto(..),
    L4Proto(..),

    waitReady,
    -- f5 filters
    ftUnset,
    ftSet,
    ftCount,
    -- fdir filters
    fdirUnset,
    fdirSet,
    fdirCount
) where

import qualified Dragonet.Pipelines.Implementation as PLI

import Data.Word
import Data.Bits
import Data.Maybe

data L3Proto = L3IPv4 | L3IPv6
    deriving (Eq,Ord)
instance Show L3Proto where
    show L3IPv4 = "IPv4"
    show L3IPv6 = "IPv6"

data L4Proto = L4TCP | L4UDP
    deriving (Eq,Ord)
instance Show L4Proto where
    show L4TCP = "TCP"
    show L4UDP = "UDP"

data FTuple = FTuple {
    ftPriority :: Word8,
    ftQueue    :: Word8,
    ftL3Proto  :: Maybe L3Proto,
    ftL4Proto  :: Maybe L4Proto,
    ftL3Src    :: Maybe Word32,
    ftL3Dst    :: Maybe Word32,
    ftL4Src    :: Maybe Word16,
    ftL4Dst    :: Maybe Word16
} deriving (Eq,Show,Ord)

data FDirTuple = FDirTuple {
    fdtQueue   :: Word8,
    fdtL3Proto :: Maybe L3Proto,
    fdtL4Proto :: Maybe L4Proto,
    fdtL3Src   :: Maybe Word32,
    fdtL3Dst   :: Maybe Word32,
    fdtL4Src   :: Maybe Word16,
    fdtL4Dst   :: Maybe Word16
} deriving (Eq,Show,Ord)


-- From e10k.h
l4tOther = 0
l4tUDP   = 1
l4tTCP   = 2
l4tSCTP  = 3

maskL4Proto = 1
maskSrcIP   = 2
maskDstIP   = 4
maskSrcPort = 8
maskDstPort = 16

ftCount = 128

foreign import ccall "e10k_ctrl_waitready"
    waitReady :: PLI.StateHandle -> IO ()

foreign import ccall "e10k_ctrl_5tuple_unset"
    c_ftfUnset :: PLI.StateHandle -> Word8 -> IO Bool

foreign import ccall "e10k_ctrl_5tuple_set"
    c_ftfSet :: PLI.StateHandle -> Word8 -> Word8 -> Word8 -> Word32
        -> Word32 -> Word16 -> Word16 -> Word16 -> Word16 -> IO Bool

ftUnset :: PLI.StateHandle -> Word8 -> IO ()
ftUnset st idx = do
    waitReady st
    res <- c_ftfUnset st idx
    if not res
        then error "e10k_ctrl_5tuple_unset failed"
        else return ()

ftSet :: PLI.StateHandle -> Word8 -> FTuple -> IO ()
ftSet st idx (FTuple p q l3 l4 sIP dIP sP dP) = do
    if idx >= ftCount
        then error "FTQF index too high"
        else return ()
    if p < 1 || p > 7
        then error "Invalid priority"
        else return ()
    waitReady st
    res <- c_ftfSet st idx p q
        (fromMaybe 0 sIP) (fromMaybe 0 dIP)
        (fromMaybe 0 sP) (fromMaybe 0 dP)
        l4' mask
    if not res
        then error "e10k_ctrl_5tuple_set failed"
        else return ()
    where
        mbFlag Nothing n _ = n
        mbFlag (Just _) _ j = j
        mask = (mbFlag l4 maskL4Proto 0) .|.
               (mbFlag sIP maskSrcIP 0) .|.
               (mbFlag dIP maskDstIP 0) .|.
               (mbFlag sP maskSrcPort 0) .|.
               (mbFlag dP maskDstPort 0)
        l4' = case l4 of
            Nothing -> 0
            Just t -> case t of
                L4TCP -> l4tTCP
                L4UDP -> l4tUDP

-- ##################### fdir flow management ###################

-- NOTE: fdirCount is based on a value in prgE10kImpl for CFDirFilter
-- TODO: Get this value by parsing NIC prg instead of hardcoding it
fdirCount = 2048

foreign import ccall "e10k_ctrl_fdir_unset"
    c_fdirUnset :: PLI.StateHandle -> Word8 -> IO Bool

foreign import ccall "e10k_ctrl_fdir_set"
    c_fdirSet :: PLI.StateHandle -> Word32 -> Word8 -> Word32
        -> Word32 -> Word16 -> Word16 -> Word16 -> Word16 -> IO Bool

fdirUnset :: PLI.StateHandle -> Int -> IO ()
fdirUnset st idx = do
    waitReady st
    res <- c_fdirUnset st (fromIntegral idx)
    if not res
        then error "e10k_ctrl_fdir_unset failed"
        else return ()

fdirSet :: PLI.StateHandle -> Int -> FDirTuple -> IO ()
fdirSet st idx (FDirTuple q l3 l4 sIP dIP sP dP) = do
    if (fromIntegral idx) >= fdirCount
        then error ("################## ERROR: FdirQF index " ++ (show idx) ++ " too high " ++ (show fdirCount) ++ " ############")
        else return ()
    waitReady st
    res <- c_fdirSet st (fromIntegral idx ) q
        (fromMaybe 0 sIP) (fromMaybe 0 dIP)
        (fromMaybe 0 sP) (fromMaybe 0 dP)
        l4' mask
    if not res
        then error "e10k_ctrl_fdir_set failed"
        else return ()
    where
        mbFlag Nothing n _ = n
        mbFlag (Just _) _ j = j
        mask = (mbFlag l4 maskL4Proto 0) .|.
               (mbFlag sIP maskSrcIP 0) .|.
               (mbFlag dIP maskDstIP 0) .|.
               (mbFlag sP maskSrcPort 0) .|.
               (mbFlag dP maskDstPort 0)
        l4' = case l4 of
            Nothing -> 0
            Just t -> case t of
                L4TCP -> l4tTCP
                L4UDP -> l4tUDP

