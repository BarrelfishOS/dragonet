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
    , BasicQueue(..)
    , Queue(..)
    , Filter(..)
    , graphLabelStr
    , getDefaultBasicQueue
    , getDefaultQueue
    , getDefaultFitlerForID
    , Qid
    , CoreID
    , FilterID
    , TupleValue
    , TupleSelector(..)
    , BitMaskSelecter(..)
    , ConfigCompare(..)
) where


import qualified Data.Maybe as MB
import qualified Data.Char as DC
import qualified Data.List as DL

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

{-
 - Very simple queue, which I may extend in future with list
 - of descriptors, but for time being, it is just to show that there is a queue.
 -}
data BasicQueue = BasicQueue {
        qSize :: Integer
    } deriving (Eq, Ord)

instance Show BasicQueue where
    show (BasicQueue s) = show s

data Queue = Queue {
        queueId :: Qid
        , coreId :: CoreID
        , bQueue :: BasicQueue
    } deriving (Eq, Ord)

instance Show Queue where
    show (Queue qid coreid _) = show qid ++ " core " ++ show coreid

data Filter = Filter {
        filterID :: FilterID
        , protocol :: Protocol
        , srcIP :: L3Address
        , dstIP :: L3Address
        , srcPort :: L4Address
        , dstPort :: L4Address
    } deriving (Eq, Ord)

instance Show Filter where
    show (Filter fid proto sip dip sp dp) = show fid ++ " " ++ show proto
        ++ "_" ++ show sip ++ "_" ++ show dip  ++ "_" ++ show sp  ++ "_"
        ++ show dp

getDefaultBasicQueue :: BasicQueue
getDefaultBasicQueue = BasicQueue 5

getDefaultQueue :: Queue
getDefaultQueue = Queue 0 0 $ getDefaultBasicQueue

getDefaultFitlerForID :: FilterID -> Filter
getDefaultFitlerForID x = (Filter x ANYProtocol 0 0 0 0)


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
        | ToDefaultKernelProcessing
        | ToQueue Queue
        | FiveTupleFilter TupleSelector Queue
        | HashFilter TupleSelector Queue
        | SyncFilter Queue
        | BitMaskFilter BitMaskSelecter Queue
        | IsFlow Filter Queue -- for flow filtering
        deriving (Show, Eq)


type TupleValue = Integer -- FIXME: should be byte
data TupleSelector = TupleSelector {
   ts1 :: TupleValue
   , ts2 :: TupleValue
   , ts3 :: TupleValue
   , ts4 :: TupleValue
   , ts5 :: TupleValue
   } deriving (Eq)

instance Show TupleSelector where
    show (TupleSelector t1 t2 t3 t4 t5) = show t1 ++ " " ++ show t2 ++ " "
        ++ show t3 ++ " " ++ show t4 ++ " " ++ show t5

data BitMaskSelecter = BitMaskSelecter {
   bitPosition :: [Integer] -- FIXME: This should be fixed size list
   , bitValue :: [Integer] -- FIXME: This should be a single bit
   } deriving (Show, Eq)

data NetOperator = AND String
        | XOR String
        | OR String
        deriving (Show, Eq)


class GraphLabel a where
    graphLabelStr :: a -> String

class ConfigCompare a where
    confCompare :: a -> a -> Bool



{-
 - Replaces blank spaces with Underscores in given string
 -}
replaceSpaces :: String -> String
replaceSpaces str = DL.map (\x-> (if DC.isAlphaNum x then x else '_' )) str

data DesLabel = DesLabel NetOperation
    deriving (Show, Eq)
instance GraphLabel DesLabel where
    graphLabelStr (DesLabel no) = replaceSpaces $ show no

data ConfLabel = ConfLabel (MB.Maybe NetOperation)
    deriving (Show, Eq)

instance ConfigCompare ConfLabel where
    confCompare (ConfLabel (MB.Just (FiveTupleFilter _ _)))
        (ConfLabel (MB.Just (FiveTupleFilter _ _))) = True
    confCompare (ConfLabel (MB.Just (HashFilter _ _)))
        (ConfLabel (MB.Just (HashFilter _ _))) = True
    confCompare (ConfLabel (MB.Just (SyncFilter _)))
        (ConfLabel (MB.Just (SyncFilter _))) = True
    confCompare (ConfLabel (MB.Just (BitMaskFilter _ _)))
        (ConfLabel (MB.Just (BitMaskFilter _ _))) = True
    confCompare (ConfLabel (MB.Just (ToQueue q1)))
        (ConfLabel (MB.Just (ToQueue q2))) = queueId q1 == queueId q2
    confCompare (ConfLabel no1) (ConfLabel no2) =  no1 == no2



instance GraphLabel ConfLabel where
    graphLabelStr (ConfLabel cl) = replaceSpaces $ "Conf" ++ (drop 4 $ show cl)

data OpLabel = OpLabel NetOperator
    deriving (Show, Eq)
instance GraphLabel OpLabel where
    graphLabelStr (OpLabel no) = replaceSpaces $ (show no)


data DesAttribute = DesAttribute Attribute
    deriving (Show, Eq, Ord)

data OpAttribute = OpAttribute Attribute
    deriving (Show, Eq, Ord)

data ConfAttribute = ConfAttribute Attribute
    deriving (Show, Eq, Ord)

data Attribute = Security Bool
    | InHardware Bool
    deriving (Show, Eq, Ord)


