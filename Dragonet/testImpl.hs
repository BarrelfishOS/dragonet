#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.DotGenerator

import Debug.Trace as T
import Control.Monad.State
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as BS




type Packet = BS.ByteString

data AttrValue = AttrS String | AttrI Int
    deriving Show
data Context = Context {
    ctxPacket :: Packet,
    ctxAttrs  :: M.Map String AttrValue
}
    deriving Show
data BoolPort = Ptrue | Pfalse
    deriving Show




packetLen :: State Context Int
packetLen = do
    ctx <- get
    return $ BS.length $ ctxPacket ctx

setAttr :: String -> AttrValue -> State Context ()
setAttr n v = do
    (Context p a) <- get
    put $ Context p $ M.insert n v a

getAttr :: String -> State Context AttrValue
getAttr n = do
    ctx <- get
    return $ attr ctx
    where
        attr c = fromJust $ M.lookup n $ ctxAttrs c


convert16BE w1 w2 =
    (shiftL (fromIntegral w1 :: Word16) 8) .|.
        (fromIntegral w2 :: Word16)

unpack16BE :: Word16 -> [Word8]
unpack16BE w =
    [(fromIntegral (shiftR w 8) :: Word8) , (fromIntegral w :: Word8)]


convert32BE w1 w2 w3 w4 =
    (shiftL (fromIntegral w1 :: Word32) 24) .|.
        (shiftL (fromIntegral w2 :: Word32) 16) .|.
        (shiftL (fromIntegral w3 :: Word32) 8) .|.
        (fromIntegral w4 :: Word32)



readPsafe :: Int -> Int -> State Context [Word8]
readPsafe offset len = do
    ctx <- get
    return $ BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8safe :: Int -> State Context (Maybe Word8)
readP8safe offset = do
    ctx <- get
    ws <- readPsafe offset 1
    case ws of
        w:[] -> return (Just w)
        _ -> return Nothing

readP16BEsafe :: Int -> State Context (Maybe Word16)
readP16BEsafe offset = do
    ctx <- get
    ws <- readPsafe offset 2
    case ws of
        w1:w2:[] -> return (Just $ convert16BE w1 w2)
        _ -> return Nothing

readP32BEsafe :: Int -> State Context (Maybe Word32)
readP32BEsafe offset = do
    ctx <- get
    ws <- readPsafe offset 4
    case ws of
        w1:w2:w3:w4:[] -> return (Just $ convert32BE w1 w2 w3 w4)
        _ -> return Nothing




readP :: Int -> Int -> State Context [Word8]
readP offset len = do
    ctx <- get
    if (length $ l ctx) /= len then
        error "Invalid read"
    else
        return (l ctx)
    where
        l ctx = BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8 :: Int -> State Context Word8
readP8 offset = do
    ctx <- get
    w:[] <- readP offset 1
    return w

readP16BE :: Int -> State Context Word16
readP16BE offset = do
    ctx <- get
    w1:w2:[] <- readP offset 2
    return (convert16BE w1 w2)

readP32BE :: Int -> State Context Word32
readP32BE offset = do
    ctx <- get
    w1:w2:w3:w4:[] <- readP offset 4
    return (convert32BE w1 w2 w3 w4)









pbool b = if b then "true" else "false"

toPort = return



lpgSoftwareRXImpl = toPort "out"

-----------------------------------------------------------------------------
-- Ethernet

lpgRxL2EtherClassifiedImpl = do
    return "true"

lpgRxL2EtherValidLengthImpl = do
    len <- packetLen
    --toPort $ pbool (len >= 56)
    toPort $ pbool (len >= 14) -- Actually an ethernet frame needs to be
                               -- at least 56 bytes, in our packets the padding
                               -- is removed

lpgRxL2EtherValidTypeImpl = do
    etype <- readP16BE 12
    toPort $ pbool (etype >= 0x0800)

lpgRxL2EtherValidMulticastImpl = do
    dmac <- readP 0 6
    toPort $ pbool
        ((dmac /= ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) &&
        (((head dmac) .&. 1) == 1)))
   

lpgRxL2EtherValidBroadcastImpl = do
    dmac <- readP 0 6
    toPort $ pbool (dmac == ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]))

lpgRxL2EtherValidUnicastImpl = do
    dmac <- readP8 0
    toPort $ pbool $ ((dmac .&. 1) == 0)

lpgRxL2EtherValidSrcImpl = do
    toPort "true"

lpgRxL2EtherClassifyL3Impl = do
    etype <- readP16BE 12
    setAttr "L3Offset" (AttrI 14)
    toPort $ case etype of
        0x0800 -> "ipv4"
        0x86DD -> "ipv6"
        0x0806 -> "arp"
        _ -> "drop"


-----------------------------------------------------------------------------
-- ARP

lpgRxL3ARPValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    -- hardcoded for Ethernet/IPv4
    if len - off == 28 then do
        hlen <- readP8 (off + 4)
        plen <- readP8 (off + 5)
        return $ pbool $ ((hlen == 6) && (plen == 4))
    else
        return "false"
        

lpgRxL3ARPClassifyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    oper <- readP16BE (off + 6)
    return $ case oper of
        1 -> "request"
        2 -> "reply"
        _ -> "drop"


-----------------------------------------------------------------------------
-- IPv4

ipHeaderLen = do
    (AttrI off) <- getAttr "L3Offset"
    ihl <- readP8 off
    return $ 4 * ((fromIntegral ihl :: Int) .&. 0xf)

-- Kind of ugly: convert pairs of bytes to 16bit integers, but use 32bit
-- arithmetic when summing up, in the end combine the higher and lower 16 bits
ipChecksum :: [Word8] -> Word16
ipChecksum p = cxsm
    where
        padded = if (length p) `mod` 2 /= 0 then p ++ [0] else p
        convertSingle a b = fromIntegral (convert16BE a b) :: Word32
        convert [] = []
        convert (a:b:rest) = (convertSingle a b):(convert rest)
        s32 = sum $ convert padded
        foldInt i = ((i .&. 0xffff) + (shiftR i 16))
        cxsm32 = xor 0xffff $ foldInt $ foldInt s32
        cxsm = fromIntegral cxsm32 :: Word16

lpgRxL3IPv4ValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    if ((len - off) < 20) then
        toPort "false"
    else do
        hlen <- ipHeaderLen
        toPort $ pbool $ (hlen >= 20 && (len - off) >= hlen)

-- For now we just make sure the packet is not fragmented
lpgRxL3IPv4ValidReassemblyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    fragOff <- readP16BE $ off + 6
    toPort $ pbool $ ((fragOff .&. 0x2000) == 0 && (fragOff .&. 0x1fff) == 0)


lpgRxL3IPv4ValidVersionImpl = do
    (AttrI off) <- getAttr "L3Offset"
    ver <- readP8 off
    toPort $ pbool ((shiftR ver 4) == 4)

lpgRxL3IPv4ValidLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    ipLen <- readP16BE (off + 2)    
    toPort $ pbool ((fromIntegral ipLen :: Int) + off <= len)

lpgRxL3IPv4ValidTTLImpl = toPort "true"

lpgRxL3IPv4ValidChecksumImpl = do
    (AttrI off) <- getAttr "L3Offset"
    hlen <- ipHeaderLen
    pkt <- readP off hlen
    toPort $ pbool $ (ipChecksum pkt) == 0

lpgRxL3IPv4ClassifyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    hlen <- ipHeaderLen
    setAttr "L4Offset" $ AttrI $ off + hlen
    proto <- readP8 (off + 9)
    toPort $ case proto of
        0x01 -> "icmp"
        0x06 -> "tcp"
        0x11 -> "udp"
        _ -> "drop"


-----------------------------------------------------------------------------
-- IPv6

lpgRxL3IPv6ValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- ICMP
lpgRxL3ICMPValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- UDP

ipv4Pseudoheader p len = do
    (AttrI off) <- getAttr "L3Offset"
    sIP <- readP (off + 12) 4
    dIP <- readP (off + 16) 4
    return (sIP ++ dIP ++ [0] ++ [p] ++ (unpack16BE len))

lpgRxL4UDPValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    toPort $ pbool ((len - off) >= 8)

lpgRxL4UDPValidLengthImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    udpLen <- readP16BE (off + 4)
    toPort $ pbool (len >= (fromIntegral udpLen) + off)

lpgRxL4UDPValidChecksumImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    cxsm <- readP16BE (off + 6)
    pkt <- readP off (len - off)
    pheader <- ipv4Pseudoheader 0x11 (fromIntegral (len - off))
    toPort $ pbool (cxsm == 0 || (ipChecksum (pheader ++ pkt)) == 0)


-----------------------------------------------------------------------------
-- TCP

lpgRxL4TCPValidHeaderLengthImpl = toPort "true"   

-----------------------------------------------------------------------------
-- Application RX

lpgRxToIPv4LocalImpl = do
    (AttrI off) <- getAttr "L3Offset"
    dIP <- readP32BE (off + 16)
    toPort $ pbool $ dIP == 0x8184666f

lpgRxToUDPPortDNSImpl = do
    (AttrI off) <- getAttr "L4Offset"
    dPort <- readP16BE (off + 2)
    toPort $ pbool $ dPort == 51098

    


-- Sinks
lpgPacketDropImpl = toPort "Packet dropped!"
lpgRxL3ARPRequestImpl = toPort "Got ARP request!"
lpgRxL3ARPResponseImpl = toPort "Got ARP response!"
lpgRxL3ICMPOutImpl = toPort "Got ICMP packet!"
lpgRxL4TCPOutImpl = toPort "Got TCP packet!"
lpgRxL4UDPOutImpl = toPort "Got UDP packet!"
lpgRxDnsRXImpl = toPort "Got DNS packet!"

-- Nodes for tx side
lpgTxSourceImpl = toPort "true"
lpgSoftwareTXImpl = toPort "true"
lpgTxARPTXImpl = toPort "true"
lpgTxExampleDnsTXImpl = toPort "true"
lpgTxExampleDns6TXImpl = toPort "true"
lpgTxL4UDPAddHeaderImpl = toPort "true"
lpgTxL4UDPAddHdrDPortImpl = toPort "true"
lpgTxL4UDPAddHdrSPortImpl = toPort "true"
lpgTxL4UDPAddHdrChecksumImpl = toPort "true"
lpgTxL4UDPAddHdrLengthImpl = toPort "true"
lpgTxL3IPv4AddHeaderImpl = toPort "true"
lpgTxL3IPv4AddHdrProtoImpl = toPort "true"
lpgTxL3IPv4AddHdrProtoUDP_Impl = toPort "true"
lpgTxL3IPv4AddHdrVersionImpl = toPort "true"
lpgTxL3IPv4AddHdrIHLImpl = toPort "true"
lpgTxL3IPv4AddHdrTotLenImpl = toPort "true"
lpgTxL3IPv4AddHdrTTLImpl = toPort "true"
lpgTxL3IPv4AddHdrSAddrImpl = toPort "true"
lpgTxL3IPv4AddHdrDAddrImpl = toPort "true"
lpgTxL3IPv6AddHeaderImpl = toPort "true"
lpgTxL3IPv6AddHdrVersionImpl = toPort "true"
lpgTxL3IPv6AddHdrSAddrImpl = toPort "true"
lpgTxL3IPv6AddHdrDAddrImpl = toPort "true"
lpgTxL2EtherAddHeaderImpl = toPort "true"
lpgTxL2EtherAddHdrTypeImpl = toPort "true"
lpgTxL2EtherAddHdrTypeIPv4_Impl = toPort "true"
lpgTxL2EtherAddHdrTypeIPv6_Impl = toPort "true"
lpgTxL2EtherAddHdrTypeARP_Impl = toPort "true"
lpgTxL2EtherAddHdrSAddrImpl = toPort "true"
lpgTxL2EtherAddHdrDAddrImpl = toPort "true"







-- The protocol graph
[unicornImpl_f|lpgImpl.unicorn|]

main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    writeFile "lpg.dot" $ toDotClustered lpgT lpgClusters
    where
        lpgT = pgSetType GTLpg lpg
