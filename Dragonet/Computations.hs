#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - All the possible computations which can happen in the Network processing.
 -
 -
 -}

--module Main (
module Computations (
    Computation(..)
    , Socket(..)
    , Module(..)
    , Application(..)
    , Filter(..)
    , AppName
    , SocketId
    , L2Address
    , L3Address
    , L4Address
    , getNetworkDependency
) where

import qualified MyGraph as MG
import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Data.Ix as Ix


type L2Address = String
type L3Address = String
type L4Address = String

-- for flow filtering
type Protocol = String

data Filter = Filter {
        protocol :: Protocol
        , srcIP :: L3Address
        , dstIP :: L3Address
        , srcPort :: L4Address
        , dstPort :: L4Address
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Filter where
    show (Filter proto sip dip sp dp) = show proto ++ "_" ++ show sip ++ "_"
            ++ dip  ++ "_" ++ sp  ++ "_" ++ dp

type Qid = String

type SocketId = Integer
type AppName = String

data Application = Application {
        appName :: AppName
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Application where
    show (Application aname) = show aname

data Socket = Socket {
      socketId :: SocketId
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Socket where
    show (Socket sid) = show sid

-- List of all the computations/tests which can happen on incoming packets
-- presence of these tags in any module will show that the module is capable of
-- performing this perticular computation
data Computation = ClassifiedL2Ethernet -- Ethernet starter node
        | L2ValidLen
        | L2ValidCRC
        | L2ValidBroadcast
        | L2ValidMulticast
        | L2ValidUnicast
        | L2ValidDest
        | L2ValidSrc
        | L2ValidType -- clasifier
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
        | L4TCPUpdateProtoState -- Updates the protocol state in machine
        | VerifiedL4TCP
        | L4ReadyToClassify
        | UnclasifiedL4
        | ClasifiedL4
        | VerifiedL4
        | ToKernelMemory -- packet copy to kernel memory
        | ToUserMemory -- packet copy to user memory
--        | IsFlow Protocol SrcIP DstIP SrcPort DstPort -- for flow filtering
        | IsFlow  Filter -- for flow filtering
        | CopyToQueue Qid
        | ToDefaultKernelProcessing
        | ToSocket Socket
        | ToApplication Application
        deriving (Show, Eq, Ord, DD.Typeable, DD.Data)
        --deriving (Show, Eq, Ord)
        --deriving (Eq, Ord)
{-
instance Show Computation where
    show (IsFlow Protocol sip dip sp dp) = "IsFlow_P_" ++ show Protocol ++ "_SIP_"
                ++ show sip  ++ "_DIP_" ++ show dip ++ "_SP_" ++ show sp
                ++ "_DP_" ++ show dp
    show (CopyToQueue1 qid) = "Queue_" ++ show qid
-}
--instance Show Computation where
--    show a = show a

{-
 - Gives list of all dependencies between various nodes in typical network graph
 - Rules: AND nodes should appear only once in the following node.
 -      AND nodes should have more than one dependency edges in single declaration.
 -      OR nodes should always have only one entry in every declaration
 -}
getNetworkDependency :: [MG.Gnode Computation]
getNetworkDependency = [
        (ClassifiedL2Ethernet, [])
        , (L2ValidLen, [ClassifiedL2Ethernet])
        , (L2ValidType, [ClassifiedL2Ethernet])
        , (L2ValidCRC, [ClassifiedL2Ethernet])
        , (L2ValidBroadcast, [ClassifiedL2Ethernet])
        , (L2ValidMulticast, [ClassifiedL2Ethernet])
        , (L2ValidUnicast, [ClassifiedL2Ethernet])
        , (L2ValidDest, [L2ValidBroadcast])
        , (L2ValidDest, [L2ValidMulticast])
        , (L2ValidDest, [L2ValidUnicast])
        , (L2ValidSrc, [ClassifiedL2Ethernet])
        , (VerifiedL2Ethernet, [L2ValidCRC, L2ValidLen, L2ValidType
                       , L2ValidDest, L2ValidSrc])

        -- IPv4 related tags
        , (ClassifiedL3IPv4, [L2ValidType])
        , (L3IPv4ValidVersion, [ClassifiedL3IPv4])
        , (L3IPv4ValidLength, [ClassifiedL3IPv4])
        , (L3IPv4ValidTTL, [ClassifiedL3IPv4])
        , (L3IPv4ValidChecksum, [ClassifiedL3IPv4])
        , (L3IPv4ValidSrc, [ClassifiedL3IPv4])
        , (L3IPv4ValidDest, [ClassifiedL3IPv4])
        , (L3IPv4ValidReassembly, [ClassifiedL3IPv4])
        , (L3IPv4ValidProtocol, [ClassifiedL3IPv4])
        , (VerifiedL3IPv4, [L3IPv4ValidVersion, L3IPv4ValidLength
            , L3IPv4ValidTTL, L3IPv4ValidChecksum, L3IPv4ValidSrc
            , L3IPv4ValidDest, L3IPv4ValidReassembly, VerifiedL2Ethernet])

        -- IPv6 related tags
        , (ClassifiedL3IPv6, [L2ValidType])
        , (L3IPv6ValidVersion, [ClassifiedL3IPv6])
        , (L3IPv6ValidLength, [ClassifiedL3IPv6])
        , (L3IPv6ValidHops, [ClassifiedL3IPv6])
        , (L3IPv6ValidSrc, [ClassifiedL3IPv6])
        , (L3IPv6ValidDest, [ClassifiedL3IPv6])
        , (L3IPv6ValidProtocol, [ClassifiedL3IPv6])
        , (VerifiedL3IPv6 , [L3IPv6ValidVersion, L3IPv6ValidLength
            , L3IPv6ValidHops, L3IPv6ValidSrc, L3IPv6ValidDest
            , L3IPv6ValidProtocol, VerifiedL2Ethernet])


        -- Generic L3 level tags
        , (VerifiedL3, [VerifiedL3IPv4])
        , (VerifiedL3, [VerifiedL3IPv6])

        , (ClassifiedL3, [L3IPv4ValidProtocol])
        , (ClassifiedL3, [L3IPv6ValidProtocol])

        -- ICMP related computations
        , (ClassifiedL4ICMP, [ClassifiedL3]) -- ICMP classification
        , (L4ICMPValidType, [ClassifiedL4ICMP])
        , (VerifiedL4ICMP, [L4ICMPValidType, VerifiedL3])

        -- UDP related computations
        , (ClassifiedL4UDP, [ClassifiedL3])
        , (L4UDPValidSrc, [ClassifiedL4UDP])
        , (L4UDPValidDest, [ClassifiedL4UDP])
        , (L4UDPValidLength, [ClassifiedL4UDP])
        , (L4UDPValidChecksum, [ClassifiedL4UDP])
        , (VerifiedL4UDP, [L4UDPValidSrc, L4UDPValidDest
            , L4UDPValidLength, L4UDPValidChecksum, VerifiedL3])

        -- TCP related computations
        , (ClassifiedL4TCP, [ClassifiedL3]) -- TCP classification
        , (L4TCPValidSrc, [ClassifiedL4TCP])
        , (L4TCPValidDest, [ClassifiedL4TCP])
        , (L4TCPValidLength, [ClassifiedL4TCP])
        , (L4TCPValidChecksum, [ClassifiedL4TCP])
        , (L4TCPValidSequence, [ClassifiedL4TCP])
        , (L4TCPValidAckNo, [ClassifiedL4TCP])
        , (L4TCPValidAck, [ClassifiedL4TCP])
        , (L4TCPValidSyn, [ClassifiedL4TCP])
        , (L4TCPValidFin, [ClassifiedL4TCP])
        , (L4TCPValidFlags, [ClassifiedL4TCP])
        , (L4TCPValidWindow, [ClassifiedL4TCP])
        , (L4TCPValidUrgent, [ClassifiedL4TCP])
        , (L4TCPValidOffset, [ClassifiedL4TCP])
        , (L4TCPValidState, [ClassifiedL4TCP])
        , (VerifiedL4TCP, [L4TCPValidSrc, L4TCPValidDest, L4TCPValidLength
            , L4TCPValidChecksum, L4TCPValidSequence, L4TCPValidAckNo
            , L4TCPValidAck, L4TCPValidSyn, L4TCPValidFin, L4TCPValidFlags
            , L4TCPValidWindow, L4TCPValidUrgent, L4TCPValidOffset
            , L4TCPValidState, VerifiedL3])
        -- TCP writeback State
        , (L4TCPUpdateProtoState, [VerifiedL4TCP])

        -- unsupported protocols.  Anything that was not treated specially
        -- by above nodes
        , (UnclasifiedL4, [ClassifiedL3])

        , (L4ReadyToClassify, [ClassifiedL4TCP])
        , (L4ReadyToClassify, [ClassifiedL4UDP])
        , (L4ReadyToClassify, [ClassifiedL4ICMP])
        , (L4ReadyToClassify, [UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [L4ReadyToClassify])

        -- Copying packets into the default queue
        , (default_queue, [generic_filter])

        , (VerifiedL4, [L4TCPUpdateProtoState])
        , (VerifiedL4, [VerifiedL4UDP])
        , (VerifiedL4, [VerifiedL4TCP])
        , (VerifiedL4, [VerifiedL4ICMP])

        , (ToDefaultKernelProcessing, [default_queue, VerifiedL4])
    ]
    where
       generic_filter = (IsFlow (Filter "ANY" "ANY" "ANY" "ANY" "ANY"))
       default_queue = (CopyToQueue "0:Default")


{-
 - Small sample dependency list for testing purposes
 -}
getNetworkDependencyDummy :: [MG.Gnode Computation]
getNetworkDependencyDummy = [
        (ClassifiedL2Ethernet, [])
        , (L2ValidLen, [ClassifiedL2Ethernet])
        , (L2ValidType, [ClassifiedL2Ethernet])
        , (VerifiedL2Ethernet, [L2ValidCRC, L2ValidLen, L2ValidType])
        , (ClassifiedL3IPv4, [L2ValidType])
    ]



data Module = Module {
        name :: String
        , computations :: [Computation]
    } deriving (Eq, Ord)
--    } deriving (Eq, Ord, DD.Typeable, DD.Data)

-- To support printing of module
instance Show Module where
    show (Module n comps) =
                "Module: { " ++ show n ++ " --> " ++ show comps ++ "}\n"


getNICModule :: Module
getNICModule = Module "NIC" []

getEthernetModule :: Module
getEthernetModule = Module "Ethernet" [L2ValidType, L2ValidCRC, L2ValidDest]

getIPv4Module :: Module
getIPv4Module = Module "IPv4" [L3IPv4ValidVersion, L3IPv4ValidLength
            , L3IPv4ValidTTL, VerifiedL3IPv4, L3IPv4ValidChecksum
            , L3IPv4ValidSrc, L3IPv4ValidDest, L3IPv4ValidReassembly]

getIPv6Module :: Module
getIPv6Module = Module "IPv6" [L3IPv6ValidVersion, L3IPv6ValidLength
            , L3IPv6ValidHops, L3IPv6ValidSrc, L3IPv6ValidDest
            , L3IPv6ValidProtocol]

getICMPModule :: Module
getICMPModule =  Module "ICMP" [L4ICMPValidType]

getUDPModule :: Module
getUDPModule =  Module "UDP" [L4UDPValidSrc, L4UDPValidDest, L4UDPValidLength
            , L4UDPValidChecksum]

getTCPModule :: Module
getTCPModule =  Module "TCP" [L4TCPValidSrc, L4TCPValidDest, L4TCPValidLength
        , L4TCPValidChecksum, L4TCPValidSequence, L4TCPValidAckNo
        , L4TCPValidAck, L4TCPValidSyn, L4TCPValidFin, L4TCPValidFlags
        , L4TCPValidWindow, L4TCPValidUrgent, L4TCPValidOffset
        , L4TCPValidState]

-- List of all modules available
getModLst :: [Module]
getModLst = [getNICModule, getEthernetModule, getIPv4Module, getIPv6Module
                , getICMPModule, getUDPModule, getTCPModule]

moduleRange :: (Module, Module) -> [Module]
moduleRange (m1, m2) = DL.takeWhile (<= m2) $ DL.dropWhile (< m1) $ getModLst

-- To support printing of module
instance Ix.Ix Module where
    range (m1, m2) = moduleRange (m1, m2)
    rangeSize (m1, m2) = DL.length $ moduleRange (m1, m2)
    index (m1, m2) m3 = DL.length $ DL.takeWhile (< m3) $ moduleRange (m1, m2)
    inRange (m1, m2) m3 = [] /= ( DL.takeWhile (== m3) $ moduleRange (m1, m2))

-- main function (just for testing purposes)
main_old :: IO()
main_old = do
        putStrLn out1
        putStrLn lineBreak
--        putStrLn $ show gr

    where
        lineBreak = "\n\n"
        -- m = L4TCPValidAckNo
        m = Module "Ethernet" [L2ValidLen, L2ValidCRC]
        out1 = show $ getModLst
--        out2 = show $ DD.typeOf (m)


main  :: IO()
main = do
        putStrLn outDot
    where
        outDotDummy = MG.showFlowGraph getNetworkDependencyDummy
        outDot = MG.showFlowGraph getNetworkDependency

