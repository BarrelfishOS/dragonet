{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LPGImpl (
    lpg, lpgClusters, pbool
) where

import qualified Dragonet.ProtocolGraph as PG
import Dragonet.Unicorn
import Dragonet.Implementation

import qualified Dragonet.Implementation.Ethernet as ETH
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Implementation.ICMP as ICMP
import qualified Dragonet.Implementation.UDP as UDP
import qualified Dragonet.Implementation.ARP as ARP

import Data.Maybe
import Data.Bits
import Data.Word
import qualified Data.Map as M
import qualified Data.List as L
import qualified Debug.Trace as T


pbool b = if b then "true" else "false"

toPort = return

cfgLocalMAC = fromJust $ ETH.macFromString "00:1b:22:54:69:f8"
cfgLocalIP = fromJust $ IP4.ipFromString "192.168.123.1"
--cfgLocalIP = fromJust $ IP4.ipFromString "129.132.102.111"




lpgSoftwareRXImpl = toPort "out"

-----------------------------------------------------------------------------
-- Ethernet
lpgRxL2EtherClassifiedImpl = do
    setAttr "L2Offset" $ AttrI 0
    return "true"

lpgRxL2EtherValidLengthImpl = do
    len <- packetLen
    --toPort $ pbool (len >= 56)
    toPort $ pbool (len >= 14) -- Actually an ethernet frame needs to be
                               -- at least 56 bytes, in our packets the padding
                               -- is removed

lpgRxL2EtherValidTypeImpl = do
    etype <- ETH.etypeRd
    toPort $ pbool (etype >= 0x0800)

lpgRxL2EtherValidMulticastImpl = do
    dmac <- ETH.sourceRd
    toPort $ pbool
        ((dmac /= ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) &&
        (((head dmac) .&. 1) == 1)))
   

lpgRxL2EtherValidBroadcastImpl = do
    dmac <- ETH.sourceRd
    toPort $ pbool (dmac == ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]))

lpgRxL2EtherValidUnicastImpl = do
    dmac <- ETH.sourceRd
    toPort $ pbool $ (((head dmac) .&. 1) == 0)

lpgRxL2EtherValidSrcImpl = do
    toPort "true"

lpgRxL2EtherClassifyL3Impl = do
    etype <- ETH.etypeRd
    setAttr "L3Offset" (AttrI 14)
    toPort $ case etype of
        0x0800 -> "ipv4"
        0x86DD -> "ipv6"
        0x0806 -> "arp"
        _ -> "drop"


-----------------------------------------------------------------------------
-- ARP

lpgRxL3ARPValidHeaderLengthImpl = do
    debug "RxL3ARPValidHeaderLength"
    off <- ARP.headerOff
    len <- packetLen
    let plen = len - off
    if plen >= ARP.headerMinLen then do
        hlen <- ARP.headerLen
        toPort $ pbool (plen == hlen)
    else
        toPort "false"
        
lpgRxL3ARPClassifyImpl = do
    debug "RxL3ARPClassify"
    oper <- ARP.operRd
    debug ("  " ++ show oper)
    toPort $
        if oper == ARP.operRequest then "request"
        else if oper == ARP.operReply then "response"
        else "drop"

-- Make sure the request is for Ethernet/IPv4
lpgRxL3ARPValidRequestImpl = do
    htype <- ARP.htypeRd
    ptype <- ARP.ptypeRd
    toPort $ pbool (htype == ARP.htypeEthernet && ptype == ARP.ptypeIPV4)

lpgRxL3ARPLocalIPDestImpl = do
    tpa <- ARP.tpaRd
    toPort $ pbool (pack32BE tpa == cfgLocalIP)

lpgRxL3ARPValidResponseImpl = do
    debug "RxL3ARPValidResponse"
    htype <- ARP.htypeRd
    ptype <- ARP.ptypeRd
    toPort $ pbool (htype == ARP.htypeEthernet && ptype == ARP.ptypeIPV4)

lpgRxL3ARPIsPendingImpl = do
    debug "RxL3ARPIsPending"
    gs <- getGS
    ip <- fmap pack32BE ARP.spaRd
    let pending = gsARPPending gs
    toPort $ pbool $ not $ null $ filter ((== ip) . fst) pending

lpgRxL3ARPProcessPendingResponseImpl = do
    debug "RxL3ARPProcessPendingResponse"
    ip <- fmap pack32BE ARP.spaRd
    mac <- ARP.shaRd

    -- Look for pending contexts and update state
    gs <- getGS
    let (curP,newP) = L.partition ((== ip) . fst) $ gsARPPending gs
    let cache = M.insert ip mac $ gsARPCache gs
    let gs' = gs { gsARPCache = cache, gsARPPending = newP }
    putGS gs'

    -- Reenable pending contexts for this IP
    let restart (_,ctx) = do { putCtx ctx ; debug ("  " ++ show (ctxAttrs ctx)) ; toPort "true" }
    mapM_ (forkPkt . restart) curP
    toPort "drop"
   



-----------------------------------------------------------------------------
-- IPv4

lpgRxL3IPv4ValidHeaderLengthImpl = do
    off <- IP4.headerOff
    len <- packetLen
    if ((len - off) < IP4.headerMinLen) then
        toPort "false"
    else do
        hlen <- IP4.headerLen
        toPort $ pbool $ (hlen >= IP4.headerMinLen && (len - off) >= hlen)

-- For now we just make sure the packet is not fragmented
lpgRxL3IPv4ValidReassemblyImpl = do
    flags <- IP4.flagsRd
    frag <- IP4.fragmentRd
    toPort $ pbool $ (flags .&. IP4.flagsMF) == 0 && (frag == 0)


lpgRxL3IPv4ValidVersionImpl = do
    ver <- IP4.versionRd
    toPort $ pbool $ ver == 4
    
lpgRxL3IPv4ValidLengthImpl = do
    len <- packetLen
    off <- IP4.headerOff
    ipLen <- IP4.lengthRd
    toPort $ pbool ((fromIntegral ipLen :: Int) + off <= len)

lpgRxL3IPv4ValidTTLImpl = toPort "true"

lpgRxL3IPv4ValidChecksumImpl = do
    off <- IP4.headerOff
    hlen <- IP4.headerLen
    pkt <- readP hlen off
    toPort $ pbool $ (IP4.checksum pkt) == 0

lpgRxL3IPv4ClassifyImpl = do
    l4off <- IP4.payloadOff
    setAttr "L4Offset" $ AttrI $ l4off
    proto <- IP4.protocolRd
    toPort $
        -- FIXME: Can't this be turned into a case?
        if proto == IP4.protocolICMP then "icmp"
        else if proto == IP4.protocolTCP then "tcp"
        else if proto == IP4.protocolUDP then "udp"
        else "drop"


-----------------------------------------------------------------------------
-- IPv6

lpgRxL3IPv6ValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- ICMP

lpgRxL3ICMPValidHeaderLengthImpl = do
    debug "RxL3ICMPValidHeaderLength"
    len <- packetLen
    off <- ICMP.headerOff
    toPort $ pbool (len - off >= ICMP.headerLen)

lpgRxL3ICMPValidChecksumImpl = do
    debug "RxL3ICMPValidChecksum"
    len <- packetLen
    off <- ICMP.headerOff
    pkt <- readP (len - off) off
    debug ("   "  ++ show (IP4.checksum pkt == 0))
    toPort $ pbool (IP4.checksum pkt == 0)

lpgRxL3ICMPIsTypeRequestImpl = do
    debug "RxL3ICMPIsTypeRequest"
    t <- ICMP.typeRd
    c <- ICMP.codeRd
    toPort $ pbool (t == ICMP.typeEchoRequest && c == 0x0)


-----------------------------------------------------------------------------
-- UDP

lpgRxL4UDPValidHeaderLengthImpl = do
    off <- UDP.headerOff
    len <- packetLen
    toPort $ pbool ((len - off) >= UDP.headerLen)

lpgRxL4UDPValidLengthImpl = do
    off <- UDP.headerOff
    len <- packetLen
    udpLen <- UDP.lengthRd
    toPort $ pbool (len >= (fromIntegral udpLen) + off)

lpgRxL4UDPValidChecksumImpl = do
    off <- UDP.headerOff
    len <- packetLen
    cxsm <- UDP.checksumRd
    pkt <- readP (len - off) off
    ipPH <- IP4.pseudoheader
    toPort $ pbool (cxsm == 0 || (IP4.checksum (ipPH ++ pkt)) == 0)


-----------------------------------------------------------------------------
-- TCP

lpgRxL4TCPValidHeaderLengthImpl = toPort "true"   

-----------------------------------------------------------------------------
-- Application RX

lpgRxToIPv4LocalImpl = do
    dIP <- IP4.destIPRd
    toPort $ pbool $ dIP == cfgLocalIP

lpgRxToUDPPortDNSImpl = do
    dPort <- UDP.destPortRd
    toPort $ pbool $ dPort == 51098

    


-- Sinks
lpgPacketDropImpl = do { debug "Packet dropped!" ; toPort "" }
lpgRxL4TCPOutImpl = do { debug "Got TCP packet!" ; toPort "" }
lpgRxL4UDPOutImpl = do { debug "Got UDP packet!" ; toPort "" }
lpgRxDnsRXImpl = do { debug "Got DNS packet!" ; toPort "" }





-----------------------------------------------------------------------------
-- Transmit Side 

lpgTxQueueImpl = do
    debug "TxQueue"
    pkt <- getPacket
    gs <- getGS
    putGS (gs { gsTXQueue = gsTXQueue gs ++ [pkt] })
    toPort ""


-----------------------------------------------------------------------------
-- Ethernet TX

lpgTxL2EtherAllocateHeaderImpl = do
    debug "TxL2EtherAllocateHeader"
    insertP ETH.headerLen 0
    shiftOffset "L4" ETH.headerLen
    shiftOffset "L3" ETH.headerLen
    setAttr "L2Offset" $ AttrI 0
    toPort "out"

lpgTxL2EtherFillHeaderImpl = do
    debug "TxL2EtherFillHeader"
    (AttrD smac) <- getAttr "ETHSrcMAC"
    (AttrD dmac) <- getAttr "ETHDstMAC"
    (AttrW16 etype) <- getAttr "ETHType"
    ETH.destWr dmac
    ETH.sourceWr smac
    ETH.etypeWr etype
    toPort "out"


-----------------------------------------------------------------------------
-- ARP TX

lpgTxL3ARPInitiateResponseImpl = do
    debug "TxL3ARPInitiateResponse"
    srcMAC <- ARP.shaRd
    srcIP <- ARP.spaRd
    dstIP <- ARP.tpaRd
    forkPkt $ (do
        setAttr "ARPDstMAC" $ AttrD srcMAC
        setAttr "ARPDstIP" $ AttrD srcIP
        setAttr "ARPSrcMAC" $ AttrD cfgLocalMAC
        setAttr "ARPSrcIP" $ AttrD dstIP
        setAttr "ARPOper" $ AttrW16 ARP.operReply
        setAttr "ETHDstMAC" $ AttrD srcMAC
        toPort "true")
    toPort "drop"

lpgTxL3ARPAllocateHeaderImpl = do
    debug "TxL3ARPAllocateHeader"
    insertP (ARP.allocLen 6 4) 0
    setAttr "L3Offset" $ AttrI 0
    toPort "out"

lpgTxL3ARPFillHeaderImpl = do
    debug "TxL3ARPFillHeader"
    (AttrD dstMAC) <- getAttr "ARPDstMAC"
    (AttrD dstIP) <- getAttr "ARPDstIP"
    (AttrD srcMAC) <- getAttr "ARPSrcMAC"
    (AttrD srcIP) <- getAttr "ARPSrcIP"
    (AttrW16 oper) <- getAttr "ARPOper"

    ARP.htypeWr ARP.htypeEthernet
    ARP.ptypeWr ARP.ptypeIPV4
    ARP.hlenWr 6
    ARP.plenWr 4
    ARP.operWr oper
    ARP.thaWr dstMAC
    ARP.tpaWr dstIP
    ARP.shaWr srcMAC
    ARP.spaWr srcIP

    setAttr "ETHSrcMAC" $ AttrD srcMAC
    setAttr "ETHType" $ AttrW16 ETH.etypeARP

    toPort "true"

lpgTxL3ARPLookup_Impl = do
    debug "TxL3ARPLookup"
    cache <- fmap gsARPCache getGS
    (AttrW32 ip) <- getAttr "IP4Dest"
    case M.lookup ip cache of
        (Just mac) -> do { debug "  hit" ; setAttr "ETHDstMAC" $ AttrD mac ; toPort "true" }
        _ -> do { debug "  miss" ; toPort "miss" }

lpgTxL3ARPSendRequestImpl = do
    debug "TxL3ARPSendRequest"
    ctx <- getCtx
    debug ("  " ++ show (ctxAttrs ctx))
    (AttrW32 srcIP) <- getAttr "IP4Source"
    (AttrW32 dstIP) <- getAttr "IP4Dest"
    (AttrD srcMAC) <- getAttr "ETHSrcMAC"
    forkPkt $ do
        setAttr "ARPDstMAC" $ AttrD [0,0,0,0,0,0]
        setAttr "ARPDstIP" $ AttrD $ unpack32BE dstIP
        setAttr "ARPSrcMAC" $ AttrD srcMAC
        setAttr "ARPSrcIP" $ AttrD $ unpack32BE srcIP
        setAttr "ARPOper" $ AttrW16 ARP.operRequest
        setAttr "ETHDstMAC" $ AttrD ETH.macBroadcast
        gs <- getGS
        putGS (gs { gsARPPending = gsARPPending gs ++ [(dstIP,ctx)] })
        toPort "true"
    toPort "drop"

-----------------------------------------------------------------------------
-- IPv4 TX

lpgTxL3IPv4AllocateHeaderImpl = do
    debug "TxL3IPv4AllocateHeader"
    let len = IP4.headerMinLen
    insertP len 0
    shiftOffset "L4" len
    setAttr "L3Offset" $ AttrI 0
    IP4.ihlWr (fromIntegral (len `quot` 4) :: Word8)
    toPort "out"

lpgTxL3IPv4FillHeaderImpl = do
    (AttrW32 dstIP) <- getAttr "IP4Dest"
    (AttrW32 srcIP) <- getAttr "IP4Source"
    (AttrW8 proto) <- getAttr "IP4Proto"
    hlen <- IP4.headerLen
    hoff <- IP4.headerOff
    plen <- IP4.payloadLen

    IP4.versionWr 4
    IP4.lengthWr (fromIntegral (hlen + plen) :: Word16)
    IP4.identificationWr 0
    IP4.flagsWr 0
    IP4.fragmentWr 0
    IP4.ttlWr 64
    IP4.protocolWr proto
    IP4.sourceIPWr srcIP
    IP4.destIPWr dstIP

    -- calculate checksum
    cs <- fmap IP4.checksum $ readP hlen hoff
    IP4.checksumWr cs

    setAttr "ETHType" $ AttrW16 ETH.etypeIPV4
    toPort "out"

lpgTxL3IPv4RoutingImpl = do
    setAttr "ETHSrcMAC" $ AttrD cfgLocalMAC
    toPort "true"



-----------------------------------------------------------------------------
-- ICMP TX

lpgTxL3ICMPInitiateResponseImpl = do
    debug "TxL3ICMPInitiateResponse"
    i <- ICMP.miscRd
    debug ("  i=" ++ show i)
    poff <- ICMP.payloadOff
    plen <- ICMP.payloadLen
    payload <- readP plen poff
    dstIP <- IP4.sourceIPRd
    srcIP <- IP4.destIPRd

    forkPkt $ (do
        setAttr "IP4Dest" $ AttrW32 dstIP
        setAttr "IP4Source" $ AttrW32 srcIP
        setAttr "ICMPId" $ AttrW32 i
        insertP plen 0
        writeP payload 0
        toPort "out")
    toPort "drop"

lpgTxL3ICMPAllocateHeaderImpl = do
    debug "TxL3ICMPAllocateHeader"
    insertP ICMP.headerLen 0
    setAttr "L4Offset" $ AttrI 0
    toPort "out"

lpgTxL3ICMPFillHeaderImpl = do
    debug "TxL3ICMPFillHeader"
    (AttrW32 i) <- getAttr "ICMPId"
    ICMP.typeWr ICMP.typeEchoReply
    ICMP.codeWr 0
    debug ("  i=" ++ show i)
    ICMP.miscWr i
    -- calculate checksum
    len <- packetLen
    off <- ICMP.headerOff
    pkt <- readP (len - off) off
    ICMP.checksumWr $ IP4.checksum pkt

    setAttr "IP4Proto" $ AttrW8 IP4.protocolICMP
    toPort "true"


        





shiftOffset :: String -> Int -> ImplM ()
shiftOffset layer off = do
    let l = layer ++ "Offset"
    a <- getAttrM l
    case a of
        (Just (AttrI i)) -> setAttr l $ AttrI (i + off)
        _ -> return ()




{-lpgSoftwareTXImpl = do { debug "SoftwareTX" ; toPort "true" }
lpgTxL3ARPBuildResponseImpl = do { debug "ARPBuildResponse" ; toPort "true" }
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
lpgTxL2EtherAddHdrDAddrImpl = toPort "true"-}


lpgClusters :: [(Int, [String])]
lpgNodes :: [(Int, PG.Node Implementation)]
lpgEdges :: [PG.PGEdge]
lpg :: PG.PGraph Implementation

-- The protocol graph
[unicornImpl_f|lpgImpl.unicorn|]

