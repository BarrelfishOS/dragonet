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
    , GraphNode(..)
    , Gnode
    , Edge
    , NodeCategory(..)
    , Socket(..)
    , Application(..)
    , Filter(..)
    , Queue(..)
    , Protocol(..)
    , Mode(..)
    , BasicQueue(..)
    , getDefaultBasicQueue
    , ModeType
    , genericModeTag
    , compareModeTags
    , addMode
    , addModeToAll
    , Qid
    , CoreID
    , AppName
    , SocketId
    , L2Address
    , L3Address
    , L4Address
    , anyValue
    , toIP
    , anyIP
    , anyPort
    , getNetworkDependency
    , getNetworkDependencyDummy
    , embeddGraphs
    , getNodesList
    , sortGraph
    , getDefaultQueue
    , getDefaultFitlerForID
    , ConfDecision(..)
    , ConfStatus(..)
    , EmulatedComp(..)
    , PartialComp(..)
    , getNodeCategory

) where

--import qualified MyGraph as MG
import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Data.Set as Set

--import qualified Data.Ix as Ix
--import qualified Debug.Trace as TR

type Gnode a = (a, [a])

type Edge a = (a, a) -- connects two vertices.

data NodeCategory =
    ANDNode
    | ORNode
    | CONFNode
    deriving (Show, Eq, Ord)


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
    deriving (Show, Eq, Ord, Enum, DD.Typeable, DD.Data)

{-
data Qid = NONEQid
    | Integer
    | ANYQid
    deriving (Show, Eq, Ord, Enum, DD.Typeable, DD.Data)
-}

type Qid = Integer
type CoreID = Integer
type FilterID = Integer

toIP :: String -> L3Address
toIP address = (read octet4::Integer) + ((read octet3::Integer) * 1000) +
    ((read octet2::Integer) * 1000000) + ((read octet1::Integer) * 1000000000)
    where
       (octet1, address')  = DL.break condition  address
       (octet2, address'') = DL.break condition $ DL.dropWhile condition address'
       (octet3, address''') = DL.break condition $ DL.dropWhile condition address''
       (octet4, _) = DL.break condition $ DL.dropWhile condition address'''
       condition = (\x -> x == '.')

anyIP ::  L3Address
anyIP = 0

anyPort :: L4Address
anyPort =  0

anyValue :: Integer
anyValue =  0


{-
 - Very simple queue, which I may extend in future with list
 - of descriptors, but for time being, it is just to show that there is a queue.
 -}
data BasicQueue = BasicQueue {
        qSize :: Integer
    } deriving (Eq, Ord,  DD.Typeable, DD.Data)

type BufID = Integer

data BufDesc = BufDesc {
    bufID :: BufID
    , owner :: Application
}

instance Show BasicQueue where
    show (BasicQueue s) = show s


data Queue = Queue {
        queueId :: Qid
        , coreId :: CoreID
        , bQueue :: BasicQueue
    } deriving (Eq, Ord,  DD.Typeable, DD.Data)

instance Show Queue where
    show (Queue qid coreid _) = show qid ++ " core " ++ show coreid

data Filter = Filter {
        filterID :: FilterID
        , protocol :: Protocol
        , srcIP :: L3Address
        , dstIP :: L3Address
        , srcPort :: L4Address
        , dstPort :: L4Address
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

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


showConfBounds :: Computation -> String
showConfBounds (ToQueue _) = "[(0, 0)..(MaxQueue, MaxCore)]"
showConfBounds (IsFlow _) = "[AllPossibleFilters]"
showConfBounds _ = "[False, True]"

data ConfStatus = ENABLE
                | SKIP
                | STOP
                | Undecided
                deriving (Show, Eq, Ord, DD.Typeable, DD.Data)

type ModeType = String



data Mode = Mode {
        mName :: ModeType
        , mComp :: Computation
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Mode where
    show (Mode name c) = show c ++ " " ++ show name


genericModeTag :: ModeType
genericModeTag = "ANY"

compareModeTags :: ModeType -> ModeType -> Bool
compareModeTags t1 t2
    | t1 == genericModeTag = True
    | t2 == genericModeTag = True
    | otherwise = t1 == t2



addMode :: ModeType -> Computation -> Computation
addMode tName (InMode _) = error "addMode: tag embedding is not allowed"
addMode tName c = InMode (Mode tName c)

{-
 - Adds specified tag as mode to all the nodes and their dependencies
 -}
addModeToAll :: ModeType -> [Gnode Computation] ->
    [Gnode Computation]
addModeToAll _ [] = []
addModeToAll tag (x:xs) = (currentNode:(addModeToAll tag xs))
    where
    currentNode = (c', deps')
    (c, deps) = x
    c' = addMode tag c
    deps' = DL.map (addMode tag) deps


data EmulatedComp = EmulatedComp  {
        eComp :: Computation
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show EmulatedComp where
    show (EmulatedComp c) = show c

data PartialComp = PartialComp {
        pComp :: Computation
        , pNeeds :: Computation
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show PartialComp where
    show (PartialComp c n) = show c ++  " " ++ show n

data ConfDecision = ConfDecision {
        dComp :: Computation
        , dStatus :: ConfStatus
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show ConfDecision where
    show (ConfDecision comp (Undecided)) =  show Undecided ++
        " " ++ show comp ++ " " ++ showConfBounds comp
    show (ConfDecision comp (SKIP)) =  show SKIP ++ " " ++ show comp
    show (ConfDecision comp (ENABLE)) =  show ENABLE ++ " " ++ show comp
    show (ConfDecision comp (STOP)) =  show STOP ++ " " ++ show comp

--        " " ++ show (DD.typeOf comp) ++ " " ++ showConfBounds comp



-- To be used in future to define Computation and Configuration
-- But for time being, I am ignoring them. (Layer and Protocol)
data Layer = L1 -- hardware
        | L2 -- Ethernet
        | L3 -- IP layer
        | L4 -- TCP/UDP layer
        | L5 -- Application
        deriving (Show, Eq, Ord, DD.Typeable, DD.Data)


class (Show gn) => GraphNode gn where
    toVertex :: gn -> String
    toVertex gn = show gn


-- List of all the computations/tests which can happen on incoming packets
-- presence of these tags in any module will show that the module is capable of
-- performing this perticular computation
data Computation =
        L0Tag
        | L2Virtualization
        | L2NOVirtualization
        | ClassifiedL2Ethernet -- Ethernet starter node
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
        | L4TCPSegmentation BasicQueue -- FIXME: when matching segmentation, size of queue should not matter
                -- Also add segmentation into LPG
        | L4TCPUpdateProtoState -- Updates the protocol state in machine
        | VerifiedL4TCP
        | L4ReadyToClassify
        | UnclasifiedL4
        | ClasifiedL4
        | VerifiedL4
        | ToKernelMemory -- packet copy to kernel memory
        | ToUserMemory -- packet copy to user memory
--        | IsFlow Protocol SrcIP DstIP SrcPort DstPort -- for flow filtering
        | IsFlow Filter -- for flow filtering
        | IsHashFilter Filter -- for hash based filtering
        | InSoftware
        | ToQueue Queue
        | ToDefaultKernelProcessing
        | ToSocket Socket
        | ToApplication Application
        | ReqBufDescregister -- BufDesc
        | VerifyBufDesc
        | AddBufDescToQueue Queue
        | IsConfSet ConfDecision
        | IsPartial PartialComp
        | IsEmulated EmulatedComp
        | InMode Mode
        deriving (Show, Eq, Ord, DD.Typeable, DD.Data)


instance GraphNode Computation where
    toVertex (InMode m) = show m
    toVertex c = show c

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
getNetworkDependency :: [Gnode Computation]
getNetworkDependency = [
        (ClassifiedL2Ethernet, [])
        , (L2EtherValidLen, [ClassifiedL2Ethernet])
        , (L2EtherValidType, [ClassifiedL2Ethernet])
        , (L2EtherValidCRC, [ClassifiedL2Ethernet])
        , (L2EtherValidBroadcast, [ClassifiedL2Ethernet])
        , (L2EtherValidMulticast, [ClassifiedL2Ethernet])
        , (L2EtherValidUnicast, [ClassifiedL2Ethernet])
        , (L2EtherValidDest, [L2EtherValidBroadcast])
        , (L2EtherValidDest, [L2EtherValidMulticast])
        , (L2EtherValidDest, [L2EtherValidUnicast])
        , (L2EtherValidSrc, [ClassifiedL2Ethernet])
        , (VerifiedL2Ethernet, [L2EtherValidCRC, L2EtherValidLen, L2EtherValidType
                       , L2EtherValidDest, L2EtherValidSrc])

        -- IPv4 related tags
        , (ClassifiedL3IPv4, [L2EtherValidType])
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
        , (ClassifiedL3IPv6, [L2EtherValidType])
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
       generic_filter = IsFlow (getDefaultFitlerForID 0)
       default_queue = ToQueue getDefaultQueue


{-
 - Small sample dependency list for testing purposes
 -}
getNetworkDependencyDummy :: [Gnode Computation]
getNetworkDependencyDummy = [
        (VerifiedL2Ethernet, [L2EtherValidLen, L2EtherValidType])
        , (L2EtherValidLen, [ClassifiedL2Ethernet])
        , (L2EtherValidType, [ClassifiedL2Ethernet])
        , (ClassifiedL3IPv4, [L2EtherValidType])
        , (ClassifiedL2Ethernet, [])
    ]


{-
 -
 -}
getDependencies :: (Eq a) => [Gnode a] -> a -> ([a], [a])
getDependencies gnodeList v
        | length listANDnodes > 1 = error "More than one list of AND dependencies"
        | length listANDnodes > 1 && length listORnodes > 1 =
                        error "Both AND and OR dependencies present"
        | otherwise = (listANDDeps, listORDeps)
    where
        depList = DL.filter (\x -> (fst x) == v) gnodeList
        filterddepList = DL.filter (\x ->  (snd x) /= [] ) depList
        listANDnodes = DL.filter (\x -> length (snd x) > 1) filterddepList
        listORnodes = DL.filter (\x -> length (snd x) == 1) filterddepList
        listANDDeps
                | listANDnodes == [] = []
                | otherwise = snd $ DL.head listANDnodes
        listORDeps
                | listORnodes == [] = []
                | otherwise = concatMap snd listANDnodes

getANDdependencies :: (Eq a) => [Gnode a] -> a -> [a]
getANDdependencies gnodeList v = fst $ getDependencies gnodeList v

getORdependencies :: (Eq a) => [Gnode a] -> a -> [a]
getORdependencies gnodeList v = snd $ getDependencies gnodeList v


getNodesList :: (Eq a) => [Gnode a] -> [a]
getNodesList gnodeList = DL.map fst gnodeList

getSinkNodesList :: (Eq a) => [Gnode a] -> [a]
getSinkNodesList gnodeList = DL.concatMap snd gnodeList

mySubset :: (Ord a) => (Eq a) => (Show a) => [a] -> [a] -> Bool
mySubset superSet subSet = Set.fromList subSet `Set.isSubsetOf`  Set.fromList superSet

-- find the first node which is not in sortedNodes list, but but not dependent
-- on any other element in unsorted list
findNextSortedNodes :: (Ord a) => (Eq a) => (Show a) => [Gnode a] ->
        [Gnode a] -> [Gnode a]
findNextSortedNodes sortedNodes unsortedNodes =
       DL.filter (\x -> mySubset  sinkNodes (snd x)) unsortedNodes
    where
       sinkNodes =  getNodesList sortedNodes


sortGraphStep :: (Ord a) => (Eq a) => (Show a) =>
        ([Gnode a], [Gnode a]) -> ([Gnode a], [Gnode a])
sortGraphStep (sortedNodes, unsortedNodes)
    | unsortedNodes == [] = (sortedNodes, [])
    | selectedNodes == [] = error (
        "No suitable nodes found even there are nodes in unsorted list " ++
        show unsortedNodes)
    | otherwise = sortGraphStep (newSorted, newUnsorted)
        where
            selectedNodes = findNextSortedNodes sortedNodes unsortedNodes
            newSorted = sortedNodes ++ selectedNodes
            newUnsorted =  unsortedNodes DL.\\ selectedNodes


sortGraph :: (Ord a) => (Eq a) => (Show a) => [Gnode a] -> [Gnode a]
sortGraph graph
    | unsorted /= [] = error ("Could not sort the graph because of: " ++
        show unsorted)
    | otherwise = sorted
    where
        (sorted, unsorted) = sortGraphStep ([], graph)

{-
f1 :: (Eq a) => [Gnode a] -> [Gnode a] -> a -> [Gnode a]
f1 prg rag v =
    | DL.notElem v getNodesList prg

    where
-}


{- Given node should go in the software, so add it to the software part
 - all previous nodes are in H/W --> just add single dep to InSoftware
 - some previous nodes in H/W --> add dep to all nodes which are not in h/w
 -              and to inSoftware
 - all previous nodes in S/W --> add dep to all those nodes
 -}
addToSoftPartAND :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
    -> [Gnode a] -> Gnode a -> Gnode a
addToSoftPartAND  prg swstartnode emblpg (vname, deps)
    | inSW == [] = (vname, [swstartnode])
    | inHW == [] = (vname, deps)
    | otherwise = (vname, [swstartnode] ++ inSW)
    where
        hwNodes = getNodesList prg
        inHW = filter (\x -> DL.elem x hwNodes)  deps
        inSW = filter (\x -> DL.notElem x hwNodes)  deps

addToSoftPartOR :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
    -> [Gnode a] -> Gnode a -> Gnode a
addToSoftPartOR  prg swstartnode  emblpg (vname, deps)
    | DL.elem (DL.head deps) (getNodesList prg) = (vname, [swstartnode])
    | otherwise = (vname, deps)


{- Given node can go in the hardware, so add it to hardware part
 - all previous nodes are in H/W --> just add single dep to InSoftware
 - some previous nodes in H/W --> add dep to all nodes which are not in
 -          h/w and to inSoftware
 - all previous nodes in S/W --> add dep to all those nodes
 -}
addToHWPartAND :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
    -> [Gnode a] -> Gnode a -> Gnode a
addToHWPartAND prg swstartnode emblpg (vname, deps)
    | inSW == [] = (vname, deps)
    | inHW == [] = (vname, deps)
    | otherwise = error ("2:previous node [ " ++ show (DL.head deps) ++
        " ]  is not in hardware, whereas this node [ " ++ show vname ++
        " ]is in h/w ")
    where
        hwNodes = getNodesList prg
        inHW = filter (\x -> DL.elem x hwNodes)  deps
        inSW = filter (\x -> DL.notElem x hwNodes)  deps


addToHWPartOR :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
    -> [Gnode a] -> Gnode a -> Gnode a
addToHWPartOR prg swstartnode emblpg (vname, deps)
    | DL.elem (DL.head deps) (getNodesList prg) = (vname, deps)
    | otherwise = error ("previous node [ " ++ show (DL.head deps) ++
        " ]  is not in hardware, whereas this node [ " ++ show vname ++
        " ]is in h/w ")



{-
 - Embedd the node v by finding out whether it can go in hardware
 - or in software
 -}
embedGivenNode :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
        -> [Gnode a] -> Gnode a -> Gnode a
embedGivenNode prg swstartnode emblpg (vname, deps)
    | DL.elem vname $ getNodesList prg = if length deps == 1 then
                        addToHWPartOR prg swstartnode  emblpg (vname, deps)
                    else
                        addToHWPartAND prg swstartnode emblpg (vname, deps)
    | otherwise = if length deps == 1 then
            addToSoftPartOR prg swstartnode  emblpg (vname, deps)
        else
            addToSoftPartAND prg swstartnode emblpg (vname, deps)


embeddGraphStep :: (Eq a) => (Ord a) => (Show a)  => [Gnode a] -> a
        -> ([Gnode a], [Gnode a]) -> ([Gnode a], [Gnode a])
embeddGraphStep prg  swstartnode (lpgEmbedded, lpgUnembedded)
        | lpgUnembedded == [] = (lpgEmbedded, lpgUnembedded)
        | otherwise = embeddGraphStep prg swstartnode (lpgEmbedded', lpgUnembedded')
        where
            v = head lpgUnembedded
            newV = embedGivenNode prg swstartnode lpgEmbedded v
            lpgEmbedded' = lpgEmbedded ++ [newV]
            lpgUnembedded' = DL.deleteBy (\x y -> fst x == fst y) newV lpgUnembedded

{-
 - Embed LPG onto PRG
 -}
embeddGraphs :: (Eq a) => (Ord a) => (Show a) => [Gnode a] -> [Gnode a]
                -> a -> a -> [Gnode a]
embeddGraphs lpg prg swstartnode defaultQueue
    | unEmbedded /= [] = error "Still some vertieces are not converted"
    | otherwise = embedded ++ [(swstartnode, [defaultQueue])]
    where
        (embedded, unEmbedded) = embeddGraphStep prg swstartnode ([], lpg)


getNodeCategory :: Gnode Computation -> NodeCategory
getNodeCategory ((IsConfSet _), deps)
    | DL.length deps > 1 = error "ERROR: given node is both AND and configuration"
    | otherwise = CONFNode
getNodeCategory (n, deps)
    | DL.length deps > 1 = ANDNode
    | otherwise = ORNode


-- main function (just for testing purposes)
main_old :: IO()
main_old = do
        putStrLn out2
        putStrLn lineBreak

    where
        lineBreak = "\n\n"
        m = L4TCPValidAckNo
        out2 = show $ DD.typeOf (m)


main  :: IO()
main = do
        putStrLn outDot
    where
        outDotDummy = show getNetworkDependencyDummy
        outDot = show getNetworkDependency

