{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGImpl.LPGImplTx (

    -- UDP TX functions
    lpgTxL4UDPInitiateResponseImpl,
    lpgTxL4UDPAllocateHeaderImpl,
    lpgTxL4UDPFillHeaderImpl,

    lpgTxDemuxImpl, -- Important node

    lpgTxQueueImpl, lpgTxL2EtherAllocateHeaderImpl, lpgTxL2EtherFillHeaderImpl,
    lpgTxL3ARPInitiateResponseImpl, lpgTxL3ARPAllocateHeaderImpl,
    lpgTxL3ARPFillHeaderImpl, lpgTxL3ARPLookup_Impl, lpgTxL3ARPSendRequestImpl,
    lpgTxL3ARPLookupRequestInImpl,
    lpgTxL3IPv4AllocateHeaderImpl, lpgTxL3IPv4FillHeaderImpl,
    lpgTxL3IPv4RoutingImpl, lpgTxL3ICMPInitiateResponseImpl,
    lpgTxL3ICMPAllocateHeaderImpl, lpgTxL3ICMPFillHeaderImpl,

) where

import Dragonet.DotGenerator


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

import LPGImpl.LPGImplBase

toPort = return
-----------------------------------------------------------------------------
--

-----------------------------------------------------------------------------
-- Transmit Side

lpgTxQueueImpl = do
    debug "TxQueue"
    pkt <- getPacket
    gs <- getGS
    putGS (gs { gsTXQueue = gsTXQueue gs ++ [pkt] })
    toPort ""

lpgTxDemuxImpl = do
    a <- getAttrM "mux"
    let nextPort = case a of
            Just (AttrS p) -> p
            _ -> "drop"

    debug ("lpgTxDemuxImpl " ++ (show nextPort))
    toPort nextPort

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
    forkPkt $ (return $
        (setAttr' "ARPDstMAC" $ AttrD srcMAC) $
        (setAttr' "ARPDstIP" $ AttrD srcIP) $
        (setAttr' "ARPSrcMAC" $ AttrD cfgLocalMAC) $
        (setAttr' "ARPSrcIP" $ AttrD dstIP) $
        (setAttr' "ARPOper" $ AttrW16 ARP.operReply) $
        (setAttr' "ETHDstMAC" $ AttrD srcMAC) $
        initContext emptyPacket)

    adm <- getAttrM "ARPDstMAC"
    if isJust adm then do
        debug "forked"
        toPort "true"
    else do
        debug "unforked"
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

-- Dummy node to interface with ARP lookup OR operator
lpgTxL3ARPLookupRequestInImpl = do
    debug "TxL3ARPLookupRequestIn"
    toPort "true"



lpgTxL3ARPLookup_Impl = do
    debug "TxL3ARPLookup"
    cache <- fmap gsARPCache getGS
    (AttrW32 ip) <- getAttr "IP4Dest"
    case M.lookup ip cache of
        (Just mac) -> do { setAttr "ETHDstMAC" $ AttrD mac ; toPort "true" }
        _ -> toPort "miss"

lpgTxL3ARPSendRequestImpl = do
    debug "TxL3ARPSendRequest"
    ctx <- getCtx
    debug ("  " ++ show (ctxAttrs ctx))
    (AttrW32 srcIP) <- getAttr "IP4Source"
    (AttrW32 dstIP) <- getAttr "IP4Dest"
    (AttrD srcMAC) <- getAttr "ETHSrcMAC"
    gs <- getGS
    putGS (gs { gsARPPending = gsARPPending gs ++ [(dstIP,ctx)] })
    forkPkt $ return $
        (setAttr' "ARPDstMAC" $ AttrD [0,0,0,0,0,0]) $
        (setAttr' "ARPDstIP" $ AttrD $ unpack32BE dstIP) $
        (setAttr' "ARPSrcMAC" $ AttrD srcMAC) $
        (setAttr' "ARPSrcIP" $ AttrD $ unpack32BE srcIP) $
        (setAttr' "ARPOper" $ AttrW16 ARP.operRequest) $
        (setAttr' "ETHDstMAC" $ AttrD ETH.macBroadcast) $
        initContext emptyPacket
    adm <- getAttrM "ARPDstMAC"
    if isJust adm then
        toPort "true"
    else
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
    pktLen <- packetLen

    IP4.versionWr 4
    IP4.lengthWr (fromIntegral (pktLen - hoff) :: Word16)
    IP4.identificationWr 0
    IP4.flagsWr IP4.flagsDF
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
    poff <- ICMP.payloadOff
    plen <- ICMP.payloadLen
    payload <- readP plen poff
    dstIP <- IP4.sourceIPRd
    srcIP <- IP4.destIPRd

    forkPkt $ return $
        (setAttr' "IP4Dest" $ AttrW32 dstIP) $
        (setAttr' "IP4Source" $ AttrW32 srcIP) $
        (setAttr' "ICMPId" $ AttrW32 i) $
        (setAttr' "forked" $ AttrI 1) $
        initContext emptyPacket
    f <- getAttrM "forked"
    if isJust f then do
        dropAttr "forked"
        insertP plen 0
        writeP payload 0
        toPort "out"
    else
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
    ICMP.miscWr i
    -- calculate checksum
    len <- packetLen
    off <- ICMP.headerOff
    pkt <- readP (len - off) off
    ICMP.checksumWr $ IP4.checksum pkt

    setAttr "IP4Proto" $ AttrW8 IP4.protocolICMP
    toPort "true"

-----------------------------------------------------------------------------
-- UDP TX



lpgTxL4UDPInitiateResponseImpl = do
    debug "lpgTxL4UDPInitiateResponseImpl "
    sport <- UDP.destPortRd
    dport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    plen' <- UDP.lengthRd
    payload <- readPX plen poff
    dstIP <- IP4.sourceIPRd
    srcIP <- IP4.destIPRd

    forkPkt $ return $
        (setAttr' "IP4Dest" $ AttrW32 dstIP) $
        (setAttr' "IP4Source" $ AttrW32 srcIP) $
        (setAttr' "UDPSrcPort" $ AttrW16 sport) $
        (setAttr' "UDPDstPort" $ AttrW16 dport) $
        (setAttr' "UDPDstPort" $ AttrW16 dport) $
        (setAttr' "UDPLen" $ AttrW16 plen') $
        (setAttr' "UDPPayload" $ AttrD payload) $
        (setAttr' "forked" $ AttrI 1) $
        initContext emptyPacket
    f <- getAttrM "forked"
    if isJust f then do
        dropAttr "forked"
        insertP plen 0
        writeP payload 0
        toPort "out"
    else
        toPort "drop"

lpgTxL4UDPAllocateHeaderImpl = do
    debug "lpgTxL4UDPAllocateHeaderImpl "
    insertP UDP.headerLen 0
    setAttr "L4Offset" $ AttrI 0
    toPort "out"


lpgTxL4UDPFillHeaderImpl = do
    debug "lpgTxL4UDPFillHeaderImpl "
    (AttrW16 sport) <- getAttr "UDPSrcPort"
    (AttrW16 dport) <- getAttr "UDPDstPort"
    (AttrW16 l) <- getAttr "UDPLen"
    UDP.sourcePortWr sport
    UDP.destPortWr dport
    UDP.lengthWr l

    -- set checksum
    UDP.checksumWr 0

    setAttr "IP4Proto" $ AttrW8 IP4.protocolUDP
    toPort "true"

-----------------------------------------------------------------------------

shiftOffset :: String -> Int -> ImplM ()
shiftOffset layer off = do
    let l = layer ++ "Offset"
    a <- getAttrM l
    case a of
        (Just (AttrI i)) -> setAttr l $ AttrI (i + off)
        _ -> return ()


-----------------------------------------------------------------------------
