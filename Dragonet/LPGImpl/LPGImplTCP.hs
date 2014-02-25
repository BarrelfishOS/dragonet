{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPGImpl.LPGImplTCP (
    -- TCP functions
    lpgRxL4TCPValidHeaderLengthImpl,
    lpgRxL4TCPPortClassifyTypeImpl,
    lpgRxL4TCPPortClassifyStaticImpl,
    lpgRxL4TCPPortClassifyDynamicImpl,
    lpgRxL4TCPClosedPortActionImpl,

    -- TCP socket related functions
    lpgRxL4TCPSocketClassifyImpl,
    lpgRxL4TCPSocketServerSideImpl, lpgRxL4TCPSocketClientSideImpl,
    lpgRxL4TCPSocketIsValidSynImpl, lpgRxL4TCPSocketSendSynImpl,

    lpgRxL4TCPSocketAddHalfOpenConnImpl, -- importanat function, sets the connection state

    -- TCP dummy function placeholder
    lpgRxTCPdynamicPortUsed,

    lpgRxTagTxTCPIRImpl,

    lpgTxL4TCPInitiateResponseImpl,
    lpgTxL4TCPAllocateHeaderImpl,
    lpgTxL4TCPFillHeaderImpl,

) where

import Dragonet.DotGenerator

import qualified Dragonet.ProtocolGraph as PG
import Dragonet.Unicorn
import Dragonet.Implementation

import qualified Dragonet.Implementation.Ethernet as ETH
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Implementation.ICMP as ICMP
import qualified Dragonet.Implementation.UDP as UDP
import qualified Dragonet.Implementation.TCP as TCP
import qualified Dragonet.Implementation.ARP as ARP



import qualified Data.ByteString as BS
import Data.Maybe
import Data.Bits
import Data.Word
import qualified Data.Map as M
import qualified Data.List as L
import qualified Debug.Trace as T

import LPGImpl.LPGImplBase

toPort = return

-----------------------------------------------------------------------------
-- TCP


lpgRxL4TCPValidHeaderLengthImpl = do
    toPort "true"


lpgRxL4TCPPortClassifyTypeImpl = do
--    toPort $ "dynamic"  -- Use this when using modifying port allocation
    toPort $ "static" -- Use this when using hardcoded port allocation

lpgRxL4TCPClosedPortActionImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    debug ("TCP packet on closed port with data:"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        )
    toPort $ "out"


-- Classifies incomming connections in static way based on following values
lpgRxL4TCPPortClassifyStaticImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    ackNo <- TCP.ackNoRd
    urgent <- TCP.urgentRd
    win <- TCP.windowRd
    dord <- TCP.dORd
    headerLen <- TCP.headerLen
    plen <- TCP.payloadLen
    poff <- UDP.payloadOff
    payload <- readPX plen poff
    flags <- TCP.flagsRd
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    portDetails <- findPortMappingTCPSocket $ fromIntegral dport
    let outPort =  case portDetails of
            Nothing -> "noSocket"
            Just x -> "toSocket"
    debug ("Static Classify: TCP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", seqNo " ++ (show seqNo)
        ++ ", ackNo " ++ (show ackNo)
        ++ ", win " ++ (show win)
        ++ ", urgent " ++ (show urgent)
        ++ "\n, flags " ++ (show flags)
        ++ ", isSyn " ++ (show isSyn)
        ++ ", isAck " ++ (show isAck)
        ++ ", isRst " ++ (show isRst)
        ++ ", isFin " ++ (show isFin)
        ++ ", dord " ++ (show dord)
        ++ ", dord Int " ++ (show (fromIntegral dord))
        ++ ", headerLen " ++ (show headerLen)
        ++ ", plen " ++ (show plen)
        ++ ", poff " ++ (show poff)
        ++ "\n, payload: " ++ (show payload)
        ++ " ====> " ++ (show portDetails) ++ "" ++ " ==> "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

-- figures out if the socket is server socket, or client socket
--      based on the socket descriptor

lpgRxL4TCPSocketClassifyImpl = do
    -- FIXME: this should do a lookup and then decide
    --      if this is client or server socket

    dport <- TCP.destPortRd
    Just portDetails <- findPortMappingTCPSocket $ fromIntegral dport
    debug ("showing matched application... " ++ show portDetails)

    let outPort = if (ptcontextTCP portDetails) == [] then
            "closedSocket"
        else if (socketStateTCP (head (ptcontextTCP portDetails))) == ListenSocket then
            "srvSocket"
        else if (socketStateTCP (head (ptcontextTCP portDetails))) == ConnectSocket then
            "cliSocket"
        else
            "closedSocket"
    debug ("socket Classify : "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort


lpgRxL4TCPSocketServerSideImpl = do

    dport <- TCP.destPortRd
    Just portDetails <- findPortMappingTCPSocket $ fromIntegral dport
    let state = tcpState (head (ptcontextTCP portDetails))
        outPort = case state of
            TCPListen -> "isListen"
--          TCPSynSent ->
            TCPSynRecved -> "isSynRecv"
            TCPEstablished -> "isEstablished"
            TCPClosed -> "isClosed"
            _ -> "isClosed"

    debug ("TCPSocketServerSide: state classify: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

lpgRxL4TCPSocketClientSideImpl = do
    let outPort = "out"
    debug ("TCPSocketClientSide: state classify (Hardcoded): "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

lpgRxL4TCPSocketIsValidSynImpl = do
    -- FIXME: check if packet is actually syn packet or not.
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    let outPort = if (isSyn && ( (not isAck) && (not isRst) && (not isFin)))
            then "true"
            else "false"
    debug ("IsValidSyn: action selection: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

-- Add connection details to socket state
lpgRxL4TCPSocketAddHalfOpenConnImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    urgent <- TCP.urgentRd
    win <- TCP.windowRd

    Just portDetails <- findPortMappingTCPSocket $ fromIntegral dport
    let outPort = "success"
        urgentNo = 0
        ackNo = seqNo + 1
        mySeqNo = 85
        myWindow = 13
        payload = emptyPacket
        payloadLen = 0
        newcon = TCPContext AcceptSocket sport dport mySeqNo ackNo
                    urgentNo myWindow payload payloadLen  TCPSynRecved

    addSocketTCP sport dport newcon
    debug ("AddHalfOpenConn: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort


lpgRxL4TCPSocketSendSynImpl = do
    let outPort = "out"
    debug ("TCPSocketSendSyn: No action (Hardcoded): "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort


-----------------------------
-- TCP dynamic port management

-- Classifies incomming connections in dynamic way based on port table context
lpgRxL4TCPPortClassifyDynamicImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd

    portHandler <- findPortMappingTCP1 $ fromIntegral dport
    let
        (outAppName, outPortFun) = case portHandler of
            Nothing -> ("closedPort", lpgRxL4TCPClosedPortActionImpl)
            Just x -> x
{-        outPort = case portHandler of
            Nothing -> "closedPort"
            Just _ -> ("SomeApp_" ++ (show dport))
                -- FIXME: need a way to get application name
-}
    debug ("Adaptive Classify: TCP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ " ====> to ==> " ++ (show outAppName)
        )
    outPortFun

lpgRxTCPdynamicPortUsed = do
    let outPort = "out"
    debug ("TCP Socket, dynamic port used: This shouldn't happen (NYI)!"
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

-----------------------------------------------------------------------------
-- TCP TX

lpgRxTagTxTCPIRImpl = do
    debug "lpgRxTagTxTCPIRImpl --> "
    setAttr "mux" $ AttrS "TCPIR"
    toPort "true"


lpgTxL4TCPInitiateResponseImpl = do
    debug "lpgTxL4TCPInitiateResponseImpl "
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    sock' <- getSpecificSocketTCP sport dport
    tt <- getSocketsTCP dport
    let sock = case sock' of
                Nothing -> error ("TCPInitiateResp: S["++ (show sport)
                        ++ "]->D[" ++ (show dport) ++ "] ==> " ++ (show tt) )
                Just x -> x

    dstIP <- IP4.sourceIPRd
    srcIP <- IP4.destIPRd

    let checksum = 0
        seqNo = seqNoTCP sock
        ackNo = ackNoTCP sock
        window = windowTCP sock
        urgent = urgentNoTCP sock
        doof = 5 -- minimum size in 32 bits unit (4 bytes unit)

        poff = doof * 4 -- Minimum header size in bytes
        plen = payloadLenTCP sock
--        plen' <- TCP.getLength
        payload = payloadTCP sock -- readPX plen poff

--        synset = True
--        ackset = True
        flags = 0x12    -- 0b010010

    forkPkt $ return $
        (setAttr' "IP4Dest" $ AttrW32 dstIP) $
        (setAttr' "IP4Source" $ AttrW32 srcIP) $
        (setAttr' "TCPSrcPort" $ AttrW16 dport) $ -- switching src and dst port
        (setAttr' "TCPDstPort" $ AttrW16 sport) $
        (setAttr' "TCPSeqNo" $ AttrW32 seqNo) $
        (setAttr' "TCPAckNo" $ AttrW32 ackNo) $
        (setAttr' "TCPWindow" $ AttrW16 window) $
        (setAttr' "TCPUrgent" $ AttrW16 urgent) $
        (setAttr' "TCPdataoff" $ AttrW8 doof) $
        (setAttr' "TCPFlags" $ AttrW8 flags) $
--        (setAttr' "TCPSyn" $ synset) $
--        (setAttr' "TCPAck" $ ackset) $
        (setAttr' "TCPChecksum" $ AttrW16 checksum) $

--        (setAttr' "TCPLen" $ AttrW16 (fromIntegral plen')) $
        (setAttr' "TCPPayload" $ AttrD (BS.unpack payload)) $
        (setAttr' "forked" $ AttrI 1) $
        initContext emptyPacket
    f <- getAttrM "forked"
    if isJust f then do
        dropAttr "forked"
        insertP plen 0
        writeP (BS.unpack payload) 0
        toPort "out"
    else
        toPort "drop"

lpgTxL4TCPAllocateHeaderImpl = do
    debug "lpgTxL4TCPAllocateHeaderImpl "
    insertP TCP.headerLenMin 0
    setAttr "L4Offset" $ AttrI 0
    toPort "out"

lpgTxL4TCPFillHeaderImpl = do
    debug "lpgTxL4TCPFillHeaderImpl "
    (AttrW16 sport) <- getAttr "TCPSrcPort"
    (AttrW16 dport) <- getAttr "TCPDstPort"
    (AttrW32 seqNo) <- getAttr "TCPSeqNo"
    (AttrW32 ackNo) <- getAttr "TCPAckNo"
    (AttrW16 window) <- getAttr "TCPWindow"
    (AttrW16 urgent) <- getAttr "TCPUrgent"
    (AttrW8 doof) <- getAttr "TCPdataoff"
    (AttrW8 flags) <- getAttr "TCPFlags"
    (AttrW16 checksum) <- getAttr "TCPChecksum"
--        (setAttr' "TCPSyn" $ synset) $
--        (setAttr' "TCPAck" $ ackset) $

    TCP.sourcePortWr sport
    TCP.destPortWr dport
    TCP.seqNoWr seqNo
    TCP.ackNoWr ackNo
    TCP.windowWr window
    TCP.urgentWr urgent
    TCP.flagsWr flags
    TCP.dOWr doof

    -- set checksum
    TCP.checksumWr checksum

    setAttr "IP4Proto" $ AttrW8 IP4.protocolTCP
    toPort "true"

