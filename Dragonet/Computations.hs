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
    , Application(..)
    , Filter(..)
    , Queue(..)
    , Qid
    , CoreID
    , AppName
    , SocketId
    , L2Address
    , L3Address
    , L4Address
    , getNetworkDependency
    , getNetworkDependencyDummy
    , embeddGraphs
    , sortGraph
    , getDefaultQueue
    , getDefaultFilter
) where

import qualified MyGraph as MG
import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Data.Set as Set
--import qualified Data.Ix as Ix
--import qualified Debug.Trace as TR

type L2Address = String
type L3Address = String
type L4Address = String

-- for flow filtering
type Proto = String

data Queue = Queue {
        queueId :: Qid
        , coreId :: CoreID
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Queue where
    show (Queue qid coreid) = show qid ++ " core " ++ show coreid

data Filter = Filter {
        protocol :: Proto
        , srcIP :: L3Address
        , dstIP :: L3Address
        , srcPort :: L4Address
        , dstPort :: L4Address
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

instance Show Filter where
    show (Filter proto sip dip sp dp) = show proto ++ "_" ++ show sip ++ "_"
            ++ dip  ++ "_" ++ sp  ++ "_" ++ dp

type Qid = String
type CoreID = String

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




-- To be used in future to define Computation and Configuration
-- But for time being, I am ignoring them. (Layer and Protocol)
data Layer = L1 -- hardware
        | L2 -- Ethernet
        | L3 -- IP layer
        | L4 -- TCP/UDP layer
        | L5 -- Application
        deriving (Show, Eq, Ord, DD.Typeable, DD.Data)

data Protocol = Ethernet
        | IPv4
        | IPv6
        | ICMP
        | UDP
        | TCP
        | Socket1


-- List of all the computations/tests which can happen on incoming packets
-- presence of these tags in any module will show that the module is capable of
-- performing this perticular computation
data Computation = ClassifiedL2Ethernet -- Ethernet starter node
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
        | InSoftware
        | ToQueue Queue
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
       generic_filter = getDefaultFilter
       default_queue = ToQueue getDefaultQueue


getDefaultQueue :: Queue
getDefaultQueue = Queue "0:Default" "0"

getDefaultFilter :: Computation
getDefaultFilter = (IsFlow (Filter "ANY" "ANY" "ANY" "ANY" "ANY"))

{-
 - Small sample dependency list for testing purposes
 -}
getNetworkDependencyDummy :: [MG.Gnode Computation]
getNetworkDependencyDummy = [
        (VerifiedL2Ethernet, [L2EtherValidLen, L2EtherValidType])
        , (L2EtherValidLen, [ClassifiedL2Ethernet])
        , (L2EtherValidType, [ClassifiedL2Ethernet])
        , (ClassifiedL3IPv4, [L2EtherValidType])
        , (ClassifiedL2Ethernet, [])
    ]

-- FIXME: Implement getPreviousNodes

-- FIXME: implement matchDependencies

{-
 - Check if the second node satisfies all the dependencies of first node.
 -
 -}
--isDepSatisfied :: (Eq a) => [MG.Gnode a] -> [MG.Gnode a] -> a -> Bool

getDependencyPaths :: (Eq a) => [MG.Gnode a] -> a -> [[a]]
getDependencyPaths gnodeList v = [[]]


{-
 -
 -}
getDependencies :: (Eq a) => [MG.Gnode a] -> a -> ([a], [a])
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

getANDdependencies :: (Eq a) => [MG.Gnode a] -> a -> [a]
getANDdependencies gnodeList v = fst $ getDependencies gnodeList v

getORdependencies :: (Eq a) => [MG.Gnode a] -> a -> [a]
getORdependencies gnodeList v = snd $ getDependencies gnodeList v


getNodesList :: (Eq a) => [MG.Gnode a] -> [a]
getNodesList gnodeList = DL.map fst gnodeList

getSinkNodesList :: (Eq a) => [MG.Gnode a] -> [a]
getSinkNodesList gnodeList = DL.concatMap snd gnodeList

mySubset :: (Ord a) => (Eq a) => (Show a) => [a] -> [a] -> Bool
mySubset superSet subSet = Set.fromList subSet `Set.isSubsetOf`  Set.fromList superSet

-- find the first node which is not in sortedNodes list, but but not dependent
-- on any other element in unsorted list
findNextSortedNodes :: (Ord a) => (Eq a) => (Show a) => [MG.Gnode a] ->
        [MG.Gnode a] -> [MG.Gnode a]
findNextSortedNodes sortedNodes unsortedNodes =
       DL.filter (\x -> mySubset  sinkNodes (snd x)) unsortedNodes
    where
       sinkNodes =  getNodesList sortedNodes


sortGraphStep :: (Ord a) => (Eq a) => (Show a) =>
        ([MG.Gnode a], [MG.Gnode a]) -> ([MG.Gnode a], [MG.Gnode a])
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


sortGraph :: (Ord a) => (Eq a) => (Show a) => [MG.Gnode a] -> [MG.Gnode a]
sortGraph graph
    | unsorted /= [] = error ("Could not sort the graph because of: " ++
        show unsorted)
    | otherwise = sorted
    where
        (sorted, unsorted) = sortGraphStep ([], graph)

{-
f1 :: (Eq a) => [MG.Gnode a] -> [MG.Gnode a] -> a -> [MG.Gnode a]
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
addToSoftPartAND :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
    -> [MG.Gnode a] -> MG.Gnode a -> MG.Gnode a
addToSoftPartAND  prg swstartnode emblpg (vname, deps)
    | inSW == [] = (vname, [swstartnode])
    | inHW == [] = (vname, deps)
    | otherwise = (vname, [swstartnode] ++ inSW)
    where
        hwNodes = getNodesList prg
        inHW = filter (\x -> DL.elem x hwNodes)  deps
        inSW = filter (\x -> DL.notElem x hwNodes)  deps

addToSoftPartOR :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
    -> [MG.Gnode a] -> MG.Gnode a -> MG.Gnode a
addToSoftPartOR  prg swstartnode  emblpg (vname, deps)
    | DL.elem (DL.head deps) (getNodesList prg) = (vname, [swstartnode])
    | otherwise = (vname, deps)


{- Given node can go in the hardware, so add it to hardware part
 - all previous nodes are in H/W --> just add single dep to InSoftware
 - some previous nodes in H/W --> add dep to all nodes which are not in
 -          h/w and to inSoftware
 - all previous nodes in S/W --> add dep to all those nodes
 -}
addToHWPartAND :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
    -> [MG.Gnode a] -> MG.Gnode a -> MG.Gnode a
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


addToHWPartOR :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
    -> [MG.Gnode a] -> MG.Gnode a -> MG.Gnode a
addToHWPartOR prg swstartnode emblpg (vname, deps)
    | DL.elem (DL.head deps) (getNodesList prg) = (vname, deps)
    | otherwise = error ("previous node [ " ++ show (DL.head deps) ++
        " ]  is not in hardware, whereas this node [ " ++ show vname ++
        " ]is in h/w ")



{-
 - Embedd the node v by finding out whether it can go in hardware
 - or in software
 -}
embedGivenNode :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
        -> [MG.Gnode a] -> MG.Gnode a -> MG.Gnode a
embedGivenNode prg swstartnode emblpg (vname, deps)
    | DL.elem vname $ getNodesList prg = if length deps == 1 then
                        addToHWPartOR prg swstartnode  emblpg (vname, deps)
                    else
                        addToHWPartAND prg swstartnode emblpg (vname, deps)
    | otherwise = if length deps == 1 then
            addToSoftPartOR prg swstartnode  emblpg (vname, deps)
        else
            addToSoftPartAND prg swstartnode emblpg (vname, deps)


embeddGraphStep :: (Eq a) => (Ord a) => (Show a)  => [MG.Gnode a] -> a
        -> ([MG.Gnode a], [MG.Gnode a]) -> ([MG.Gnode a], [MG.Gnode a])
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
embeddGraphs :: (Eq a) => (Ord a) => (Show a) => [MG.Gnode a] -> [MG.Gnode a]
                -> a -> a -> [MG.Gnode a]
embeddGraphs lpg prg swstartnode defaultQueue
    | unEmbedded /= [] = error "Still some vertieces are not converted"
    | otherwise = embedded ++ [(swstartnode, [defaultQueue])]
    where
        (embedded, unEmbedded) = embeddGraphStep prg swstartnode ([], lpg)

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
        outDotDummy = MG.showFlowGraph getNetworkDependencyDummy
        outDot = MG.showFlowGraph getNetworkDependency

