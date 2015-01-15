module Runner.SFControl (
    FTuple(..),
    FDirTuple(..),
    L3Proto(..),
    L4Proto(..),

    waitReady,
    ftUnset,
    ftSet,
    ftCount
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
    ftPriority :: Int,
    ftQueue    :: Int,
    ftL3Proto  :: Maybe L3Proto,
    ftL4Proto  :: Maybe L4Proto,
    ftL3Src    :: Maybe Word32,
    ftL3Dst    :: Maybe Word32,
    ftL4Src    :: Maybe Word16,
    ftL4Dst    :: Maybe Word16
} deriving (Eq,Show,Ord)

data FDirTuple = FDirTuple {
    fdtQueue   :: Int,
    fdtL3Proto :: L3Proto,
    fdtL4Proto :: L4Proto,
    fdtL3Src   :: Word32,
    fdtL3Dst   :: Word32,
    fdtL4Src   :: Word16,
    fdtL4Dst   :: Word16
} deriving (Eq,Show,Ord)


-- From sf.h
-- #define IPROTO_IP       0
-- #define IPROTO_ICMP     1
-- #define IPROTO_IGMP     2
-- #define IPROTO_TCP      6
-- #define IPROTO_UDP      17


l4tOther = 0
l4tUDP   = 17
l4tTCP   = 6
l4tSCTP  = 3

maskL4Proto = 1
maskSrcIP   = 2
maskDstIP   = 4
maskSrcPort = 8
maskDstPort = 16

ftCount = 2048

foreign import ccall "sf_ctrl_waitready"
    waitReady :: PLI.StateHandle -> IO ()

foreign import ccall "sf_ctrl_5tuple_unset"
    c_ftfUnset :: PLI.StateHandle -> Int -> IO Bool

foreign import ccall "sf_ctrl_5tuple_set"
    c_ftfSet :: PLI.StateHandle -> Int -> Int -> Int -> Word32
        -> Word32 -> Word16 -> Word16 -> Word16 -> Word16 -> IO Bool

ftUnset :: PLI.StateHandle -> Int -> IO ()
ftUnset st idx = do
    waitReady st
    res <- c_ftfUnset st idx
    if not res
        then error "sf_ctrl_5tuple_unset failed"
        else return ()

ftSet :: PLI.StateHandle -> Int -> FTuple -> IO ()
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
        then error "sf_ctrl_5tuple_set failed"
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

