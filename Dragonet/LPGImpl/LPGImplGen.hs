{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGImpl.LPGImplGen (

--    lpg, lpgClusters, graphGen,

    pbool,
    lpgQueueImpl,
    lpgSoftwareRXImpl, lpgRxL2EtherClassifiedImpl, lpgRxL2EtherValidLengthImpl,
    lpgRxL2EtherValidTypeImpl, lpgRxL2EtherValidMulticastImpl,
    lpgRxL2EtherValidBroadcastImpl, lpgRxL2EtherValidUnicastImpl,
    lpgRxL2EtherValidSrcImpl, lpgRxL2EtherClassifyL3Impl,
    lpgRxL2EtherValidLocalMACImpl,
    lpgRxL3ARPValidHeaderLengthImpl, lpgRxL3ARPClassifyImpl,
    lpgRxL3ARPValidRequestImpl, lpgRxL3ARPLocalIPDestImpl,
    lpgRxL3ARPValidResponseImpl, lpgRxL3ARPIsPendingImpl,
    lpgRxL3ARPProcessPendingResponseImpl, lpgRxL3IPv4ValidHeaderLengthImpl,
    lpgRxL3IPv4ValidReassemblyImpl, lpgRxL3IPv4ValidVersionImpl,
    lpgRxL3IPv4ValidLocalIPImpl,
    lpgRxL3IPv4ValidLengthImpl, lpgRxL3IPv4ValidTTLImpl,
    lpgRxL3IPv4ValidChecksumImpl, lpgRxL3IPv4ClassifyImpl,
    lpgRxL3IPv6ValidHeaderLengthImpl, lpgRxL3ICMPValidHeaderLengthImpl,
    lpgRxL3ICMPValidChecksumImpl, lpgRxL3ICMPIsTypeRequestImpl,

    -- UDP functions
    lpgRxL4UDPValidHeaderLengthImpl,
    lpgRxL4UDPValidLengthImpl,
    lpgRxL4UDPValidChecksumImpl,
    lpgRxL4UDPPortClassifyTypeImpl,
    lpgRxL4UDPPortClassifyStaticImpl,
    lpgRxL4UDPPortClassifyDynamicImpl,
    lpgRxL4UDPClosedPortActionImpl,

--    lpgRxL4UDPOutImpl,
    lpgRxTagTxARPIRImpl, lpgRxTagTxARPLuImpl, lpgRxTagTxICMPIRImpl,

    lpgRxTagTxUDPIRImpl, -- classify Tx

    -- Applications
    lpgRxDnsAPPImpl,
    lpgRxEchoAPPImpl,
    lpgPacketDropImpl,

    -- Port management functions
    lpgRxL4UDPAddPort,

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

-----------------------------------------------------------------------------

toPort = return

lpgQueueImpl = toPort "out"
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
--    debug ("etype is " ++ show(etype))
    toPort $ pbool (etype >= 0x0800)

lpgRxL2EtherValidMulticastImpl = do
    dmac <- ETH.sourceRd
    toPort $ pbool
        ((dmac /= ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) &&
        (((head dmac) .&. 1) == 1)))


lpgRxL2EtherValidBroadcastImpl = do
    smac <- ETH.destRd
    dmac <- ETH.sourceRd
--    debug ("llpgRxL2EtherValidBroadcastImpl destMac " ++ show (dmac)
--            ++ " , srcMac " ++ show(smac) )
    toPort $ pbool (dmac == ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]))

lpgRxL2EtherValidUnicastImpl = do
    smac <- ETH.destRd
    dmac <- ETH.sourceRd

--    debug ("lpgRxL2EtherValidUnicastImpl  destMac " ++ show (dmac)
--            ++ " , srcMac " ++ show(smac) )
    toPort $ pbool $ (((head dmac) .&. 1) == 0)


lpgRxL2EtherValidLocalMACImpl = do
    smac <- ETH.destRd
    let endPort = (smac == cfgLocalMAC)


--    debug ("lpgRxL2EtherValidLocalMACImpl: "
--        ++ " smac (" ++ (show smac)
--        ++ ") == local mac (" ++ (show cfgLocalMAC)
--        ++ ") ==> "
--        ++ " port: " ++ (show endPort)
--        )
    toPort $ pbool $  endPort


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
    off <- ARP.headerOff
    len <- packetLen
    let plen = len - off
--    debug ("RxL3ARPValidHeaderLength: "
--        ++ (" len is " ++ show(len))
--        ++ (" off is " ++ show(off))
--        ++ (" plen is "  ++ show (plen))
--        )

    if plen >= ARP.headerMinLen then do
        hlen <- ARP.headerLen
--        debug ("toport is "  ++ show (plen) ++ " == " ++ show(hlen))
        toPort $ pbool (plen >= hlen)
    else
        toPort "false"

lpgRxL3ARPClassifyImpl = do
--    debug "RxL3ARPClassify"
    oper <- ARP.operRd
--    debug ("RxL3ARPClassify oper is " ++ show(oper) ++ " Req is "
--            ++ show(ARP.operRequest) ++ " and res is " ++ show(ARP.operReply) )

    toPort $
        if oper == ARP.operRequest then "request"
        else if oper == ARP.operReply then "response"
        else
            "drop"
            --debug "RxL3ARPClassify its not request or response, so dropping"

-- Make sure the request is for Ethernet/IPv4
lpgRxL3ARPValidRequestImpl = do
--    debug "L3ARPValidRequest"
    htype <- ARP.htypeRd
    ptype <- ARP.ptypeRd
    toPort $ pbool (htype == ARP.htypeEthernet && ptype == ARP.ptypeIPV4)

lpgRxL3ARPLocalIPDestImpl = do
    tpa <- ARP.tpaRd
--    debug ("lpgRxL3ARPLocalIPDestImpl " ++ show (tpa) ++
        -- " "++ show (pack32BE tpa) ++ " " ++ show (cfgLocalIP)  ++
        -- " selectedPort " ++
        -- show (pack32BE tpa == cfgLocalIP))
    toPort $ pbool (pack32BE tpa == cfgLocalIP)

lpgRxL3ARPValidResponseImpl = do
--    debug "RxL3ARPValidResponse"
    htype <- ARP.htypeRd
    ptype <- ARP.ptypeRd
    toPort $ pbool (htype == ARP.htypeEthernet && ptype == ARP.ptypeIPV4)

lpgRxL3ARPIsPendingImpl = do
--    debug "RxL3ARPIsPending"
    gs <- getGS
    ip <- fmap pack32BE ARP.spaRd
    let pending = gsARPPending gs
    toPort $ pbool $ not $ null $ filter ((== ip) . fst) pending

lpgRxL3ARPProcessPendingResponseImpl = do
--    debug "RxL3ARPProcessPendingResponse"
    ip <- fmap pack32BE ARP.spaRd
    mac <- ARP.shaRd

    -- Look for pending contexts and update state
    gs <- getGS
    let (curP,newP) = L.partition ((== ip) . fst) $ gsARPPending gs
    let cache = M.insert ip mac $ gsARPCache gs
    let gs' = gs { gsARPCache = cache, gsARPPending = newP }
    putGS gs'

    -- Reenable pending contexts for this IP
    let restart (_,ctx) = do { return $ setAttr' "reenabled" (AttrI 1) ctx }
    mapM_ (forkPkt . restart) curP

    re <- getAttrM "reenabled"
    if isJust re then do
        dropAttr "reenabled"
        toPort "true"
    else
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

lpgRxL3IPv4ValidLocalIPImpl = do
    dIP <- IP4.destIPRd

    -- FIXME: THIS is hack to avoid unnecessary ARP requests going out.
    -- Updating the ARP cache with MAC from current packet
    smac <- ETH.sourceRd
    sip <- IP4.sourceIPRd

    gs <- getGS
    let cache = M.insert sip smac $ gsARPCache gs
    let gs' = gs { gsARPCache = cache }
    putGS gs'


    let nextPort = dIP == cfgLocalIP
--    debug ("lpgRxL3IPv4ValidLocalIPImpl:  " ++ (show nextPort))
    toPort $ pbool $ nextPort

lpgRxL3IPv4ClassifyImpl = do
    l4off <- IP4.payloadOff
    l4payloadLen <- IP4.payloadLen
    setAttr "L4Offset" $ AttrI $ l4off
    setAttr "L4PayloadLen" $ AttrI $ l4payloadLen
    proto <- IP4.protocolRd
    let nextPort = if proto == IP4.protocolICMP then "icmp"
        else if proto == IP4.protocolTCP then "tcp"
        else if proto == IP4.protocolUDP then "udp"
        else "drop"
--    debug("lpgRxL3IPv4ClassifyImpl l4Proto val, (ICMP, TCP, UDP), dest_port: "
--        ++ show(proto) ++ ", ("
--        ++ show IP4.protocolICMP ++ ", "
--        ++ show IP4.protocolTCP ++ ", "
--        ++ show IP4.protocolUDP ++ ") "
--        ++ nextPort)
    toPort $ nextPort

-----------------------------------------------------------------------------
-- IPv6

lpgRxL3IPv6ValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- ICMP

lpgRxL3ICMPValidHeaderLengthImpl = do
--    debug "RxL3ICMPValidHeaderLength"
    len <- packetLen
    off <- ICMP.headerOff
    toPort $ pbool (len - off >= ICMP.headerLen)

lpgRxL3ICMPValidChecksumImpl = do
--    debug "RxL3ICMPValidChecksum"
    len <- packetLen
    off <- ICMP.headerOff
    pkt <- readP (len - off) off
--    debug ("   "  ++ show (IP4.checksum pkt == 0))
    toPort $ pbool (IP4.checksum pkt == 0)

lpgRxL3ICMPIsTypeRequestImpl = do
--    debug "RxL3ICMPIsTypeRequest"
    t <- ICMP.typeRd
    c <- ICMP.codeRd
    toPort $ pbool (t == ICMP.typeEchoRequest && c == 0x0)


-----------------------------------------------------------------------------
-- UDP

lpgRxL4UDPValidHeaderLengthImpl = do
--    debug "RxL4UDPValidHeaderLengthImpl"
    off <- UDP.headerOff
    len <- packetLen
    let nextPort = ((len - off) >= UDP.headerLen)
--    debug ("RxL4UDPValidHeaderLengthImpl " ++ (show nextPort))
    toPort $ pbool nextPort

lpgRxL4UDPValidLengthImpl = do
    off <- UDP.headerOff
    len <- packetLen
    udpLen <- UDP.lengthRd
    let nextPort = (len >= (fromIntegral udpLen) + off)
--   debug ("lpgRxL4UDPValidLengthImpl " ++ (show nextPort))
    toPort $ pbool nextPort

lpgRxL4UDPValidChecksumImpl = do
    off <- UDP.headerOff
    len <- packetLen
    cxsm <- UDP.checksumRd
    pkt <- readP (len - off) off
    ipPH <- IP4.pseudoheader
    let nextPort = (cxsm == 0 || (IP4.checksum (ipPH ++ pkt)) == 0)
--    debug ("lpgRxL4UDPValidChecksumImpl " ++ (show nextPort))
    toPort $ pbool nextPort


lpgRxL4UDPAddPort portNo socktype  appName app = do
    addPortMappingUDP portNo socktype appName app

lpgRxL4UDPPortClassifyTypeImpl = do
--    toPort $ "dynamic"  -- Use this when using modifying port allocation
    toPort $ "static" -- Use this when using hardcoded port allocation

-- Classifies incomming connections in static way based on following values
lpgRxL4UDPPortClassifyStaticImpl = do
    dport <- UDP.destPortRd
    sport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    payload <- readPX plen poff
    let outPort = if dport == 51098 then "appDNS"
        else if dport == 5556 then "appEcho"
        else "closedPort"
    debug ("Hardcoded Classify: UDP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", poff " ++ (show poff)
        ++ ", len " ++ (show plen)
        ++ ", payload " ++ (show payload)
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

-- Classifies incomming connections in dynamic way based on port table context
lpgRxL4UDPPortClassifyDynamicImpl = do
    dport <- UDP.destPortRd
    sport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    payload <- readPX plen poff

    portHandler <- findPortMappingUDP1 $ fromIntegral dport
    let
        outPortFun = case portHandler of
            Nothing -> lpgRxL4UDPClosedPortActionImpl
            Just x -> x

        outPort = case portHandler of
            Nothing -> "closedPort"
            Just _ -> ("SomeApp_" ++ (show dport))
                -- FIXME: need a way to get application name

    debug ("Adaptive Classify: UDP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", poff " ++ (show poff)
        ++ ", len " ++ (show plen)
        ++ ", payload " ++ (show payload)
        ++ " ====> to ==> " ++ (show outPort)
        )
    outPortFun

lpgRxL4UDPClosedPortActionImpl = do
    dport <- UDP.destPortRd
    sport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    payload <- readPX plen poff
    debug ("UDP packet on closed port with data:"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", poff " ++ (show poff)
        ++ ", len " ++ (show plen)
        ++ ", payload " ++ (show payload)
        )
    toPort $ "out"

-----------------------------------------------------------------------------
-- Application RX


-- Sinks
lpgPacketDropImpl = do { debug "Packet dropped!!!!" ; toPort "" }


lpgRxDnsAPPImpl = do
    dport <- UDP.destPortRd
    sport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    payload <- readPX plen poff
    debug ("DNS Packet with data:"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", poff " ++ (show poff)
        ++ ", len " ++ (show plen)
        ++ ", payload " ++ (show payload)
        )
    toPort "out"

lpgRxEchoAPPImpl = do
    dport <- UDP.destPortRd
    sport <- UDP.sourcePortRd
    poff <- UDP.payloadOff
    plen <- UDP.payloadLen
    payload <- readPX plen poff
    debug ("Echo Packet with data:"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", poff " ++ (show poff)
        ++ ", len " ++ (show plen)
        ++ ", payload " ++ (show payload)
        )
    toPort "out"

-----------------------------------------------------------------------------
-- Multiplexing for TX queue

lpgRxTagTxARPIRImpl = do
    setAttr "mux" $ AttrS "ARPIR"
    toPort "true"

lpgRxTagTxARPLuImpl = do
    setAttr "mux" $ AttrS "ARPLu"
    toPort "true"

lpgRxTagTxICMPIRImpl = do
    setAttr "mux" $ AttrS "ICMPIR"
    toPort "true"

lpgRxTagTxUDPIRImpl = do
    debug "lpgRxTagTxUDPIRImpl --> "
    setAttr "mux" $ AttrS "UDPIR"
    toPort "true"


-----------------------------------------------------------------------------


{-
lpgClusters :: [(Int, [String])]
lpgNodes :: [(Int, PG.Node Implementation)]
lpgEdges :: [PG.PGEdge]
lpg :: PG.PGraph Implementation

-- The protocol graph
[unicornImpl_f|lpgImpl.unicorn|]


graphGen :: IO ()
graphGen = do
    putStrLn "Generating .dot files for Dragonet implemented lpg"
    writeFile "lpgImpl2.dot" $ toDotClustered lpgT lpgClusters
    where
        lpgT = PG.pgSetType PG.GTLpg lpg
-}

