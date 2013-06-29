#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - All the possible computations which can happen in the Network processing.
 -
 -
 -}

--module Main (
module NetBasics (
    DesLabel(..)
    , ConfLabel(..)
    , OpLabel(..)
    , DesAttribute(..)
    , ConfAttribute(..)
    , OpAttribute(..)
    , Attribute(..)
    , NetOperation(..)
    , NetOperator(..)
    , graphLabelStr
) where


type L2Address = Integer
type L3Address = Integer
type L4Address = Integer

-- for flow filtering
data Protocol = NONEProtocol
    | Ethernet
    | IEEE80211
    | IPv4
    | IPv6
    | ICMP
    | UDP
    | TCP
    | ANYProtocol
    | CustomProto String
    deriving (Show, Eq, Ord)

type Qid = Integer
type CoreID = Integer
type FilterID = Integer

data Layer = L1 -- hardware
        | L2 -- Ethernet
        | L3 -- IP layer
        | L4 -- TCP/UDP layer
        | L5 -- Application
        | CustomLayer String
        deriving (Show, Eq, Ord)

data Operation = Checksum
        | LengthCheck
        deriving (Show, Eq, Ord)

data DesLabel1 = DesLabel1 {
        dLayer :: Layer
        , dProtocol :: Protocol
        , dOperation :: Operation
    }
    deriving (Show, Eq)

data NetOperation = ClassifiedL2Ethernet -- Ethernet starter node
        | L2EtherValidLen
        | L2EtherValidCRC
        | L2EtherValidBroadcast
        | L2EtherValidMulticast
        | L2EtherValidUnicast
        | L2EtherValidDest
        | L2EtherValidSrc
        | L2EtherValidType -- clasifier
        | VerifiedL2Ethernet -- tag specifying that is it a valid Ethernet packet
        | VerifiedL2
        | ClassifiedL3IPv4 -- IPv4 starter node
        | L3IPv4ValidVersion
        | L3IPv4ValidLength
        | L3IPv4ValidTTL
        | L3IPv4ValidChecksum
        | L3IPv4ValidSrc
        | L3IPv4ValidDest
        | L3IPv4ValidReassembly
        | L3IPv4ValidProtocol -- clasifies the next level protocol
        | VerifiedL3IPv4 -- clasifies the next level protocol
        | ClassifiedL3IPv4Valid -- tag specifying that is it a valid IPv4 packet
        | ClassifiedL3IPv6 -- IPv6 starter node
        | L3IPv6ValidVersion
        | L3IPv6ValidLength
        | L3IPv6ValidHops
        | L3IPv6ValidSrc
        | L3IPv6ValidDest
        | L3IPv6ValidProtocol -- clasifies the next level protocol
        | VerifiedL3IPv6
        | VerifiedL3
        | ClassifiedL3
        | ClassifiedL4ICMP -- ICMP starter node
        | L4ICMPValidType
        | VerifiedL4ICMP -- ICMP verified node
        | ClassifiedL4UDP
        | L4UDPValidSrc
        | L4UDPValidDest
        | L4UDPValidLength
        | L4UDPValidChecksum
        | VerifiedL4UDP
        | ClassifiedL4TCP
        | L4TCPValidSrc
        | L4TCPValidDest
        | L4TCPValidLength
        | L4TCPValidChecksum
        | L4TCPChecksumAdjustment
        | L4TCPValidSequence
        | L4TCPValidAckNo
        | L4TCPValidAck
        | L4TCPValidSyn
        | L4TCPValidFin
        | L4TCPValidFlags
        | L4TCPValidWindow
        | L4TCPValidUrgent
        | L4TCPValidOffset
        | L4TCPValidState
        | L4TCPSegmentation
        | L4TCPUpdateProtoState -- Updates the protocol state in machine
        | VerifiedL4TCP
        | L4ReadyToClassify
        | UnclasifiedL4
        | ClasifiedL4
        | VerifiedL4
        | ReqBufDescregister -- BufDesc
        | VerifyBufDesc
        | PacketDrop
        deriving (Show, Eq, Ord)

data NetOperator = AND
        | XOR
        | OR
        deriving (Show, Eq, Ord)



class GraphLabel a where
    graphLabelStr :: a -> String


data DesLabel = DesLabel NetOperation
    deriving (Show, Eq)
instance GraphLabel DesLabel where
    graphLabelStr (DesLabel no) = show no

data ConfLabel = ConfLabel String
    deriving (Show, Eq)
instance GraphLabel ConfLabel where
    graphLabelStr (ConfLabel cl) = show cl

data OpLabel = OpLabel NetOperator
    deriving (Show, Eq)
instance GraphLabel OpLabel where
    graphLabelStr (OpLabel no) = show no


data DesAttribute = DesAttribute Attribute
    deriving (Show, Eq, Ord)

data OpAttribute = OpAttribute Attribute
    deriving (Show, Eq, Ord)

data ConfAttribute = ConfAttribute Attribute
    deriving (Show, Eq, Ord)

data Attribute = Security Bool
    | InHardware Bool
    deriving (Show, Eq, Ord)


