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

--    lpgRxEchoAppServSocketImpl, -- TCP application
--
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

-- Classifies incomming connections in dynamic way based on port table context
lpgRxL4TCPPortClassifyDynamicImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd

    portHandler <- findPortMapping1 $ fromIntegral dport
    let
        outPortFun = case portHandler of
            Nothing -> lpgRxL4TCPClosedPortActionImpl
            Just x -> x

        outPort = case portHandler of
            Nothing -> "closedPort"
            Just _ -> ("SomeApp_" ++ (show dport))
                -- FIXME: need a way to get application name

    debug ("Adaptive Classify: TCP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ " ====> to ==> " ++ (show outPort)
        )
    outPortFun

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
    doof <- TCP.dORd
    flags <- TCP.flagsRd
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    let outPort = if dport == 1234 then "toSocket"
        else "noSocket"
    debug ("Static Classify (Hardcoded) : TCP packet with :"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", seqNo " ++ (show seqNo)
        ++ ", ackNo " ++ (show ackNo)
        ++ ", win " ++ (show win)
        ++ ", urgent " ++ (show urgent)
        ++ ", flags " ++ (show flags)
        ++ ", dataoff " ++ (show doof)
        ++ ", isSyn " ++ (show isSyn)
        ++ ", isAck " ++ (show isAck)
        ++ ", isRst " ++ (show isRst)
        ++ ", isFin " ++ (show isFin)
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

-- figures out if the socket is server socket, or client socket
--      based on the socket descriptor
lpgRxL4TCPSocketClassifyImpl = do
    -- FIXME: this should do a lookup and then decide
    --      if this is client or server socket
    let outPort = "srvSocket"
    debug ("socket Classify (Hardcoded) : "
        ++ " ====> to ==> " ++ (show outPort)
        )
    toPort $ outPort

lpgRxL4TCPSocketServerSideImpl = do
    let outPort = "isListen"
    debug ("TCPSocketServerSide: state classify (Hardcoded): "
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
    let outPort = "true"
    debug ("TCPSocketIsValidSyn: action selection (Hardcoded): "
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
-- TCP application

lpgRxEchoAppServSocketImpl = do
    dport <- TCP.destPortRd
    sport <- TCP.sourcePortRd
    seqNo <- TCP.seqNoRd
    ackNo <- TCP.ackNoRd
    urgent <- TCP.urgentRd
    win <- TCP.windowRd
    doof <- TCP.dORd
    flags <- TCP.flagsRd
    isSyn <- TCP.isSYN
    isAck <- TCP.isACK
    isRst <- TCP.isRST
    isFin <- TCP.isFIN
    debug ("TCP Echo Packet with data:"
        ++ "  sport " ++ (show sport)
        ++ ", dport " ++ (show dport)
        ++ ", seqNo " ++ (show seqNo)
        ++ ", ackNo " ++ (show ackNo)
        ++ ", win " ++ (show win)
        ++ ", urgent " ++ (show urgent)
        ++ ", flags " ++ (show flags)
        ++ ", dataoff " ++ (show doof)
--        ++ ", flags " ++ (showIntAtBase 2 intToDigit flags)
        ++ ", isSyn " ++ (show isSyn)
        ++ ", isAck " ++ (show isAck)
        ++ ", isRst " ++ (show isRst)
        ++ ", isFin " ++ (show isFin)
        )
    toPort "out"


