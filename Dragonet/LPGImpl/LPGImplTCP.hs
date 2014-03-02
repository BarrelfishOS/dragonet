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

    lpgRxL4TCPSocketCopyDataImpl,

    -- state change nodes
    lpgRxL4TCPSocketSChangeEstablishedImpl,
    lpgRxL4TCPSocketSChangeCloseWaitImpl,
    lpgRxL4TCPSocketSChangeToClosedImpl,
    lpgRxL4TCPSocketSChangeToClosed2Impl,
    lpgRxL4TCPSocketSChangeLastAckImpl,

    -- condition check nodes
    lpgRxL4TCPSocketIsValidSynAckSImpl,
    lpgRxL4TCPSocketIsDataPacketImpl,
    lpgRxL4TCPSocketIsFinSetImpl,
    lpgRxL4TCPSocketIsValidFinAckImpl,
    lpgRxL4TCPSocketIsValidFinAck2Impl,


    -- Error condition handling
    lpgRxL4TCPSocketInClosedImpl,

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
import qualified Dragonet.Implementation as IMPL

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
    sport <- TCP.sourcePortRd
    listenports <-  findPortMappingTCPSocket (fromIntegral dport)
    pdtails <- getSpecificSocketTCP sport dport
    let portDetails = case pdtails of
            Just x -> x
            Nothing -> case (listenports) of
                            Just xx -> (head (ptcontextTCP xx))
                            Nothing -> error "No socket is in listen or accepted mode"

--    Just portDetails <- findPortMappingTCPSocket $ fromIntegral dport
    --let state = tcpState (head (ptcontextTCP portDetails))
    let state = tcpState portDetails
        outPort = case state of
            TCPListen -> "isListen"
--          TCPSynSent ->
            TCPSynRecved -> "isSynRecv"
            TCPEstablished -> "isEstablished"
            TCPCloseWait -> "isCloseWait"
            TCPLastAck -> "isLastAck"
            TCPClosed -> "isClosed"
            _ -> "isClosed"

    debug ("TCPSocketServerSide: state classify: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort


lpgRxL4TCPSocketInClosedImpl = do
    let outPort = "out"
    debug ("TCPSocketInClosedState: incomming packet on closed socket "
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
        newcon = defaultTCPContext dport AcceptSocket TCPSynRecved
        newcon' = newcon {
                        srcPortTCP      = sport,
                        seqNoTCP        = mySeqNo,
                        ackNoTCP        = ackNo,
                        windowTCP       = myWindow,
                        snd_iss_TCP     = 85,
                        snd_una_TCP     = mySeqNo,
                        snd_nxt_TCP     = mySeqNo,
                        snd_wnd_TCP     = win,
                        rcv_iss_TCP     = seqNo,
                        rcv_nxt_TCP     = ackNo,
                        rcv_wnd_TCP     = myWindow
                     }
    addSocketTCP sport dport newcon'
    debug ("AddHalfOpenConn: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort


lpgRxL4TCPSocketSChangeEstablishedImpl = do
    let outPort = "true"
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    payloadlen <- TCP.payloadLen
    win <- TCP.windowRd

    updateSocketTCPState sport dport TCPEstablished
    Just sock <- getSpecificSocketTCP sport dport

    --let newSeqNo = (fromIntegral (seqNoTCP sock)) + 1
    let newSeqNo = (fromIntegral (snd_nxt_TCP sock)) + 1
    updateSocketTCPSynNo sport dport newSeqNo
    let newAckNo = (fromIntegral seqNo) + 1
    updateSocketTCPAckNo sport dport newAckNo
    let usock = sock {
                        snd_una_TCP     = newSeqNo,
                        snd_nxt_TCP     = newSeqNo,
                        rcv_nxt_TCP     = newAckNo,
                        snd_wnd_TCP     = win
        }
    updateSocketTCP sport dport usock
    debug ("SChangeEstablished: with payload-len " ++  (show payloadlen)
        ++ " ====> to ==> " ++ (show outPort)
        )
    -- FIXME: connect with IsDataPacket or with CopyData when there is data in this packet
    toPort $ outPort



lpgRxL4TCPSocketIsValidSynAckSImpl = do
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    seqNo <- TCP.seqNoRd
    ackNo <- TCP.ackNoRd
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    flags <- TCP.flagsRd

    Just sock <- getSpecificSocketTCP sport dport
    --let myseqno = seqNoTCP sock
    let myseqno = snd_nxt_TCP sock
        outPort = if (isAck && (ackNo == myseqno + 1) &&
                     (not isSyn) &&  (not isRst) && (not isFin))
                    then "true"
                    else "false"
    debug ("IsValidSynAck: action selection: [ " ++ " flags " ++ (show flags)
            ++ " ackNo " ++ (show ackNo) ++ " == myseqno " ++ (show myseqno)
            ++ "] ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

lpgRxL4TCPSocketIsDataPacketImpl = do
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    seqNo <- TCP.seqNoRd
    ackNo <- TCP.ackNoRd
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd

    -- FIXME: Valid seq number

    Just sock <- getSpecificSocketTCP sport dport
--    let myseqno = seqNoTCP sock
    let myseqno = snd_nxt_TCP sock
        -- FIXME: figure out a proper way to validate ack numbers
        --outPort = if (isAck && (ackNo == myseqno + 1) &&
        outPort = if (isAck && -- (ackNo == myseqno + 1) &&
                     (not isSyn) &&  (not isRst)) --  && (not isFin)
                    then "true"
                    else "false"
    debug ("IsValidData: " ++ " ====> to ==> " ++ (show outPort))
    toPort $ outPort

-- Check if the fin flag is set
lpgRxL4TCPSocketIsFinSetImpl = do
    isFin <- TCP.isFIN
    let outPort = if (isFin) then "true" else "false"
    debug ("IsFin: " ++ " ====> to ==> " ++ (show outPort))
    toPort $ outPort

lpgRxL4TCPSocketIsValidFinAckImpl = do
    isAck <- TCP.isACK
    isSyn <- TCP.isSYN
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    seqNo <- TCP.seqNoRd
    ackNo <- TCP.ackNoRd
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd

    Just sock <- getSpecificSocketTCP sport dport
    --let myseqno = seqNoTCP sock
    let myseqno = snd_nxt_TCP sock
        outPort = if (isAck && (ackNo == myseqno + 1) &&
                     (not isSyn) &&  (not isRst)) -- && (not isFin))
                    then "true"
                    else "false"
    debug ("IsValidFinAck: action selection: [ "
            ++ " ackNo " ++ (show ackNo) ++ " == myseqno " ++ (show myseqno)
            ++ "] ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

lpgRxL4TCPSocketIsValidFinAck2Impl = lpgRxL4TCPSocketIsValidFinAckImpl

lpgRxL4TCPSocketSChangeCloseWaitImpl = do
    let outPort = "true"
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    payloadlen <- TCP.payloadLen
    win <- TCP.windowRd
    Just sock <- getSpecificSocketTCP sport dport
    updateSocketTCPState sport dport TCPCloseWait
    let newAckNo = (fromIntegral seqNo) + 1 -- (fromIntegral payloadlen)
    updateSocketTCPAckNo sport dport newAckNo
    let usock = sock {
--                        snd_una_TCP     = newSeqNo,
--                        snd_nxt_TCP     = newSeqNo,
                        rcv_nxt_TCP     = newAckNo,
                        snd_wnd_TCP     = win
        }
    updateSocketTCP sport dport usock
    debug ("SChangeCloseWait: state reached " ++ " ====> to ==> "
            ++ (show outPort))
    toPort $ outPort



lpgRxL4TCPSocketSChangeLastAckImpl = do
    let outPort = "true"
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    isAck <- TCP.isACK
    payloadlen <- TCP.payloadLen
    win <- TCP.windowRd
    updateSocketTCPState sport dport TCPLastAck

    Just sock <- getSpecificSocketTCP sport dport
--    let mysynno = seqNoTCP sock
--        newSynNo = (fromIntegral mysynno) + 1
--    updateSocketTCPSynNo sport dport newSynNo
    let newAckNo = (fromIntegral seqNo) + 1 -- (fromIntegral payloadlen)
    updateSocketTCPAckNo sport dport newAckNo
    let usock = sock {
--                        snd_una_TCP     = newSeqNo,
--                        snd_nxt_TCP     = newSeqNo,
                        rcv_nxt_TCP     = newAckNo,
                        snd_wnd_TCP     = win
        }
    updateSocketTCP sport dport usock
    debug ("SChangeLastAck: state reached " ++ " ====> to ==> "
            ++ (show outPort))
    toPort $ outPort


lpgRxL4TCPSocketSChangeToClosedImpl = do
    let outPort = "true"
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    updateSocketTCPState sport dport TCPClosed
    debug ("SChangeToClosed: state reached " ++ " ====> to ==> "
            ++ (show outPort))
    toPort $ outPort


lpgRxL4TCPSocketSChangeToClosed2Impl = lpgRxL4TCPSocketSChangeToClosedImpl


lpgRxL4TCPSocketCopyDataImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    payload <- TCP.getPayload
    seqNo <- TCP.seqNoRd
    seg_ack <- TCP.ackNoRd
    payloadlen <- TCP.payloadLen
    win <- TCP.windowRd
    Just sock <- getSpecificSocketTCP sport dport

    -- FIXME: Use the ACK no to determine which seq no. can be dropped.
    -- Is it valid ACK no? SND.UNA < SEG.ACK =< SND.NXT
    let snd_una = IMPL.snd_una_TCP sock
        snd_nxt = IMPL.snd_nxt_TCP sock
        snd_una_new = if ((snd_una) < seg_ack) && (seg_ack <= snd_nxt) then seg_ack
                        else snd_una

        -- Dropping the data which is successfully acked.
        remaining_send_data = BS.drop (fromIntegral (snd_una_new - snd_una))
                                    (IMPL.sendDataTCP sock)
        usock = sock {
                        sendDataTCP     = remaining_send_data,
                        snd_una_TCP     = snd_una_new
            }


--    let newAckNo = (fromIntegral (ackNoTCP sock) + (fromIntegral payloadlen))
    let newAckNo = (fromIntegral seqNo) + (fromIntegral payloadlen)
    updateSocketTCPAckNo sport dport newAckNo
    let usock = sock {
--                        snd_una_TCP     = newSeqNo,
--                        snd_nxt_TCP     = newSeqNo,
                        rcv_nxt_TCP     = newAckNo,
                        snd_wnd_TCP     = win
        }
    updateSocketTCP sport dport usock
    debug ("############## Data of size " ++ (show payloadlen)
            ++ " for application: " ++ (show payload))
    let outPort = if (payloadlen == 0) then "false" else "true"
    toPort $ outPort





lpgRxL4TCPSocketSendSynImpl = do
    let outPort = "true"
    debug ("TCPSocketSendSyn: "
        ++ " ====> to ==> " ++ (show outPort)
        )
    -- FIXME: make explicit operation for updating ACK no.
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
        --seqNo = IMPL.seqNoTCP sock
        seqNo = IMPL.snd_nxt_TCP sock
        --ackNo = IMPL.ackNoTCP sock
        ackNo = IMPL.rcv_nxt_TCP sock
        --window = IMPL.windowTCP sock
        window = IMPL.rcv_wnd_TCP sock
        urgent = IMPL.urgentNoTCP sock

        -- FIXME: Get the header size from socket option, and don't hardcode it.
        doof = 5 -- minimum size in 32 bits unit (4 bytes unit)
        poff = doof * 4 -- Minimum header size in bytes

        plen = IMPL.payloadLenTCP sock
        payload = IMPL.payloadTCP sock -- readPX plen poff

        sockState = IMPL.tcpState sock

        -- Get flags from socket state
--        synset = True
--        ackset = True
        -- flags = 0x12    -- 0b010010
        flags = decideFlags sock

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
        (setAttr' "TCPChecksum" $ AttrW16 checksum) $

        (setAttr' "TCPLen" $ AttrW16 (fromIntegral plen)) $
        (setAttr' "TCPPayload" $ AttrD (BS.unpack payload)) $
        (setAttr' "forked" $ AttrI 1) $
        initContext emptyPacket
    f <- getAttrM "forked"
    debug ("###### flags for state " ++ (show sockState) ++
            " are " ++ (show flags))
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

    -- Calculate checksum
    pkt <- getPacket
    pktlen <- packetLen

    -- Following values needs to be set for creating psudo ipv4 header
    setAttr "IP4Proto" $ AttrW8 IP4.protocolTCP
    setAttr "IP4PayloadLen" $ AttrW16 (fromIntegral pktlen)

    ipPH <- IP4.pseudoheaderTx
    let csum = IP4.checksum (ipPH ++ (BS.unpack pkt))

    -- set checksum
    TCP.checksumWr csum

    toPort "true"


