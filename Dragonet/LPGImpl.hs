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
import qualified Dragonet.Implementation.UDP as UDP

import Data.Maybe
import Data.Bits


pbool b = if b then "true" else "false"

toPort = return

cfgLocalMAC = fromJust $ ETH.macFromString "00:1b:22:54:69:f8"
cfgLocalIP = fromJust $ IP4.ipFromString "192.168.123.1"




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
lpgRxL3ICMPValidHeaderLengthImpl = toPort "true"


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
lpgRxL3ARPRequestImpl = do { debug "Got ARP request!" ; toPort "" }
lpgRxL3ARPResponseImpl = do { debug "Got ARP response!" ; toPort "" }
lpgRxL3ICMPOutImpl = do { debug "Got ICMP packet!" ; toPort "" }
lpgRxL4TCPOutImpl = do { debug "Got TCP packet!" ; toPort "" }
lpgRxL4UDPOutImpl = do { debug "Got UDP packet!" ; toPort "" }
lpgRxDnsRXImpl = do { debug "Got DNS packet!" ; toPort "" }

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


lpgClusters :: [(Int, [String])]
lpgNodes :: [(Int, PG.Node Implementation)]
lpgEdges :: [PG.PGEdge]
lpg :: PG.PGraph Implementation

-- The protocol graph
[unicornImpl_f|lpgImpl.unicorn|]

