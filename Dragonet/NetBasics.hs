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
    , EmbedCompare(..)
    , DesLabel1(..) -- added to avoid unused types warning
    , Application(..)
    , Socket(..)
    , SocketId
    , AppName
    , Flow(..)
    , L4Flow(..)
    , L3Flow(..)
    , L2Flow(..)
    , Address(..)
    , getFlow
    , Protocol(..)
    , toIP
    , getANYL2Address
    , getANYL3Address
    , getANYL4Address
) where


import qualified Data.Maybe as MB
import qualified Data.Char as DC
import qualified Data.List as DL

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

type SocketId = Integer
type AppName = String

data Application = Application {
        appName :: AppName
    } deriving (Eq, Ord)

instance Show Application where
    show (Application aname) = show aname

data Socket = Socket {
      socketId :: SocketId
    } deriving (Eq, Ord)

instance Show Socket where
    show (Socket sid) = show sid


data Queue = Queue {
        queueId :: Qid
        , coreId :: CoreID
        , bQueue :: BasicQueue
    } deriving (Eq, Ord)

instance Show Queue where
    show (Queue qid coreid _) = show qid ++ " core " ++ show coreid
--    show (Queue qid coreid _) = "" -- show qid ++ " core " ++ show coreid

instance ConfigCompare Queue where
    confCompare q1 q2 = queueId q1 == queueId q2

instance EmbedCompare Queue where
    embedCompare q1 q2 = queueId q1 == queueId q2

data Address = L2Address Integer
            | L3Address Integer
            | L4Address Integer
        deriving (Show, Eq, Ord)

data Flow = Flow {
        fProto        :: Protocol
        , fSRCAddr    :: Address
        , fDSTAddr    :: Address
    } deriving (Eq, Ord)

instance Show Flow where
    show (Flow proto src dest) = show proto ++ " " ++ show src ++ " "
            ++ show dest

data L4Flow = L4Flow Flow L3Flow -- Port numbers
    deriving (Eq, Ord)

instance Show L4Flow where
    show (L4Flow fl4 (L3Flow fl3 (L2Flow fl2))) = show (fProto fl4) ++ " "
            ++ show (fProto fl3) ++ " "
--            ++ show (L4Address (fSRCAddr fl4)) ++ " "
--            ++ show (L3Address (fSRCAddr fl3)) ++ " "
--            ++ show (L4Address (fDSTAddr fl4)) ++ " "
--            ++ show (L3Address (fDSTAddr fl3))



data L3Flow = L3Flow Flow L2Flow -- IP Addresses
    deriving (Show, Eq, Ord)

data L2Flow = L2Flow Flow -- MAC addresses
    deriving (Show, Eq, Ord)

getL2Flow :: Protocol -> Address -> Address -> L2Flow
getL2Flow p s d = L2Flow f
    where
    f = Flow {
        fProto = p
        , fSRCAddr = s
        , fDSTAddr = d
    }

getL3Flow :: (Protocol, Protocol) -> (Address, Address) -> (Address, Address)
    -> L3Flow
getL3Flow (p3, p2) (s3, s2) (d3, d2) = L3Flow f fl2
    where
    f = Flow {
        fProto = p3
        , fSRCAddr = s3
        , fDSTAddr = d3
    }
    fl2 = getL2Flow p2 s2 d2

getL4Flow :: (Protocol, Protocol, Protocol) -> (Address, Address, Address)
    -> (Address, Address, Address) -> L4Flow
getL4Flow (p4, p3, p2) (s4, s3, s2) (d4, d3, d2) = L4Flow f fl3
    where
    f = Flow {
        fProto = p4
        , fSRCAddr = s4
        , fDSTAddr = d4
    }
    fl3 = getL3Flow (p3, p2) (s3, s2) (d3, d2)

getANYL2Address :: Address
getANYL2Address = L2Address 0

getANYL3Address :: Address
getANYL3Address = L3Address 0

getANYL4Address :: Address
getANYL4Address = L4Address 0


toIP :: String -> Address
toIP address = L3Address val
    where
       val = (read octet4::Integer) + ((read octet3::Integer) * 1000) +
            ((read octet2::Integer) * 1000000) + ((read octet1::Integer) * 1000000000)
       (octet1, address')  = DL.break condition  address
       (octet2, address'') = DL.break condition $ DL.dropWhile condition address'
       (octet3, address''') = DL.break condition $ DL.dropWhile condition address''
       (octet4, _) = DL.break condition $ DL.dropWhile condition address'''
       condition = (\x -> x == '.')



getFlow :: (Protocol, Protocol) -> (Address, Address)
    -> (Address, Address) -> L4Flow
getFlow (p4, p3) (s4, s3) (d4, d3) = getL4Flow (p4, p3, p2)
        (s4, s3, s2) (d4, d3, d2)
    where
        p2 = Ethernet
        s2 = getANYL2Address
        d2 = getANYL2Address

type Addr = Integer

data Filter = Filter {
        filterID :: FilterID
        , protocol :: Protocol
        , srcIP :: Addr
        , dstIP :: Addr
        , srcPort :: Addr
        , dstPort :: Addr
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
        | UnclassifiedL4
        | ClassifiedL4
        | VerifiedL4
        | ReqBufDescregister -- BufDesc
        | VerifyBufDesc
        | PacketDrop
        | InSoftware
        | ToDefaultKernelProcessing
        | ToQueue Queue
        | FiveTupleFilter TupleSelector Queue
        | HashFilter TupleSelector Queue
        | SyncFilter Queue
        | BitMaskFilter BitMaskSelecter Queue
        | IsL5Flow L4Flow -- Filter -- for flow filtering
        | ToSocket Socket -- POSIX style sockets
        | CopyTo
        | ToApplication Application -- End applications
        deriving (Show, Eq)

instance ConfigCompare NetOperation where
    confCompare (FiveTupleFilter _ _) (FiveTupleFilter _  _) = True
    confCompare (HashFilter _ _ ) (HashFilter _ _ ) = True
    confCompare (SyncFilter _ ) (SyncFilter _ ) = True
    confCompare (BitMaskFilter _ _ ) (BitMaskFilter _ _ ) = True
    confCompare (ToQueue q1) (ToQueue q2) = queueId q1 == queueId q2
    confCompare no1  no2 =  no1 == no2


instance EmbedCompare NetOperation where
    embedCompare (FiveTupleFilter f1 q1) (FiveTupleFilter f2 q2) =
        f1 == f2 && embedCompare q1 q2
    embedCompare (HashFilter f1 q1) (HashFilter f2 q2) =
        f1 == f2 && embedCompare q1 q2
    embedCompare (BitMaskFilter ms1 q1) (BitMaskFilter ms2 q2) =
        ms1 == ms2 && embedCompare q1 q2
    embedCompare (SyncFilter q1) (SyncFilter q2) = embedCompare q1 q2
    embedCompare (ToQueue q1) (ToQueue q2) = embedCompare q1 q2
    embedCompare no1 no2 = no1 == no2


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
--    show (TupleSelector t1 t2 t3 t4 t5) = ""

data BitMaskSelecter = BitMaskSelecter {
   bitPosition :: [Integer] -- FIXME: This should be fixed size list
   , bitValue :: [Integer] -- FIXME: This should be a single bit
   } deriving (Show, Eq)

data NetOperator = AND | XOR | OR
        deriving (Show, Eq)


class GraphLabel a where
    graphLabelStr :: a -> String

class ConfigCompare a where
    confCompare :: a -> a -> Bool

class EmbedCompare a where
    embedCompare :: a -> a -> Bool




{-
 - Replaces blank spaces with Underscores in given string
 -}
replaceSpaces :: String -> String
replaceSpaces str = DL.map (\x-> (if DC.isAlphaNum x then x else '_' )) str

data DesLabel = DesLabel NetOperation
    deriving (Show, Eq)

instance EmbedCompare DesLabel where
    embedCompare (DesLabel no1) (DesLabel no2) = embedCompare no1 no2


instance ConfigCompare DesLabel where
    confCompare (DesLabel no1) (DesLabel no2) = confCompare no1 no2

instance GraphLabel DesLabel where
    graphLabelStr (DesLabel no) = replaceSpaces $ show no

data ConfLabel = ConfLabel (MB.Maybe NetOperation)
    deriving (Show, Eq)

instance ConfigCompare ConfLabel where
    confCompare (ConfLabel (MB.Just no1))
        (ConfLabel (MB.Just no2)) = confCompare no1 no2
    confCompare _ _ = error "confCompare: Incompatible nodes in confCompare"

{-
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
-}


instance GraphLabel ConfLabel where
    graphLabelStr (ConfLabel cl) = replaceSpaces $ "Conf" ++ (drop 4 $ show cl)

data OpLabel = OpLabel {
    netOP   :: NetOperator
    , labOP :: NetOperation
    } deriving (Show, Eq)

instance GraphLabel OpLabel where
    graphLabelStr (OpLabel no l) = replaceSpaces $ ((show no) ++ ":" ++ show l)


data DesAttribute = DesAttribute Attribute
    deriving (Show, Eq, Ord)

data OpAttribute = OpAttribute Attribute
    deriving (Show, Eq, Ord)

data ConfAttribute = ConfAttribute Attribute
    deriving (Show, Eq, Ord)

data Attribute = Security Bool -- assumed to be False
    | InHardware Bool -- assumed to be True
    | InSoft Bool -- assumed to be False
    | ResultSaved Bool -- assumed to be True
    | NeedAdaptor Bool -- Assumed to be False
    | IsAdaptor Bool -- Assumed to be False
    deriving (Show, Eq, Ord)

{-
canEmbed :: [Attribute] -> Bool
canEmbed desAttr
    | NeedAdaptor `DL.isElem` desAttr = False
    | ResultSaved `DL.isElem` desAttr = False
    where
    needAdapter

checkAttr :: (Attribute -> Bool) -> Attribute -> [Attribute] -> Bool
checkAttr fn attr attrList
    |
    where
    foundAttr =
-}

