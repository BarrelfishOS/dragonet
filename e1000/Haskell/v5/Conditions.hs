#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

--module Conditions (
module Main (
    Computation(..)
    , Module(..)
    , getModLst
    , moduleRange
    , main
) where

import qualified Data.Data as DD
import qualified Data.List as DL
import qualified Data.Ix as Ix


-- List of all the computations/tests which can happen on incoming packets
-- presence of these tags in any module will show that the module is capable of
-- performing this perticular computation
data Computation = ClassifiedL2Ethernet -- Ethernet starter node
        | L2ValidLen
        | L2ValidCRC
        | L2ValidDest
        | L2ValidSrc
        | L2ValidType -- clasifier
        | VerifiedL2Ethernet -- tag specifying that is it a valid Ethernet packet
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
--        | IsFlow Proto SrcIP DstIP SrcPort DstPort -- for flow filtering
        | ToKernelMemory -- packet copy to kernel memory
        | ToUserMemory -- packet copy to user memory
        deriving (Show, Eq, Ord, Ix.Ix, DD.Typeable, DD.Data)


type Dependency = (Computation, Computation)
-- data Dependency = Dependency (Computation, Computation)


getDependencyList :: [Dependency]
getDependencyList = [
        (L2ValidType, ClassifiedL2Ethernet)
        , (L2ValidCRC, ClassifiedL2Ethernet)
        , (L2ValidDest, ClassifiedL2Ethernet)
        , (L2ValidSrc, ClassifiedL2Ethernet)
        , (ClassifiedL2Ethernet, L2ValidType)
        , (L3IPv4ValidVersion, ClassifiedL3IPv4)
        , (L3IPv4ValidVersion, ClassifiedL3IPv4)
        , (L3IPv4ValidLength, ClassifiedL3IPv4)
        , (L3IPv4ValidTTL, ClassifiedL3IPv4)
        , (VerifiedL3IPv4, ClassifiedL3IPv4) -- clasify nxt lvl prto
        , (L3IPv4ValidChecksum, ClassifiedL3IPv4)
                ]

getValidityList :: [Dependency]
getValidityList = [
        (VerifiedL2Ethernet, L2ValidType)
        , (VerifiedL2Ethernet, L2ValidSrc)
        , (VerifiedL2Ethernet, L2ValidDest)
        , (VerifiedL2Ethernet, L2ValidCRC)

            ]


type Gnode a = (a, [a])
type Edge a = (a, a)

myShowList :: (Show a) => [a] -> String
myShowList [] = "\n"
myShowList (x:xs) = show x ++ "\n" ++ myShowList xs

{-
 - Remove the duplicates from the list
 -}
rmdups :: (Ord a) => [a] -> [a]
rmdups = DL.map DL.head . DL.group . DL.sort

{-
 - get list of valid vertices from given Gnode list.
 - This also includes the vertices which have only incoming edges
 - and no outgoing edges
-}
getVertices :: (Ord a) => [Gnode a] -> [a]
getVertices nlist = rmdups $ source_vertex ++ dest_vertex
    where
        source_vertex = DL.map fst nlist
        dest_vertex = DL.concat $ DL.map snd nlist


{-
 - Get list of all the edges from given list of Gnodes
 -}
makeEdgeList :: a -> [a] -> [Edge a]
makeEdgeList _ [] = []
makeEdgeList src (x:xs) = [(src, x)] ++ makeEdgeList src xs

getEdges :: (Ord a) => [Gnode a] -> [Edge a]
getEdges nlist = DL.concat $ DL.map
                    ( \ e -> makeEdgeList (fst e) (snd e)) nlist

{-
 - Prints the graph in dot format
 - Arguments are <list of vertices> <list of edges>
 -}
showGraphViz :: (Show a) => [a] -> [(a, a)] -> String
showGraphViz vertices edges =
    "digraph name {\n" ++
    "rankdir=LR;\n" ++
    (DL.concatMap showNode vertices) ++
    (DL.concatMap showEdge edges) ++
    "}\n"
    where showEdge (from, to) = show from ++ " -> " ++ show to ++
                   " [label = \"" ++ "\"];\n"
          showNode v = show v ++ " [label = " ++ (show  v) ++ "];\n"


{-
 - Small example  dependency list for testing
 - -}
getNetworkDependencyDummy :: [Gnode Computation]
getNetworkDependencyDummy = [
        (ClassifiedL2Ethernet, [])
        , (L2ValidLen, [ClassifiedL2Ethernet])
        , (L2ValidType, [ClassifiedL2Ethernet])
        , (VerifiedL2Ethernet, [L2ValidCRC, L2ValidLen, L2ValidType])
        , (ClassifiedL3IPv4, [L2ValidType])
    ]

{-
 - Gives list of all dependencies between various nodes in typical network
 - graph
 -}
getNetworkDependency :: [Gnode Computation]
getNetworkDependency = [
        (ClassifiedL2Ethernet, [])
        , (L2ValidLen, [ClassifiedL2Ethernet])
        , (L2ValidType, [ClassifiedL2Ethernet])
        , (L2ValidCRC, [ClassifiedL2Ethernet])
        , (L2ValidDest, [ClassifiedL2Ethernet])
        , (L2ValidSrc, [ClassifiedL2Ethernet])
        , (VerifiedL2Ethernet, [L2ValidCRC, L2ValidLen, L2ValidType
                       , L2ValidDest, L2ValidSrc])
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
            , L3IPv4ValidDest, L3IPv4ValidReassembly])
        , (VerifiedL3IPv4, [VerifiedL2Ethernet])

        , (ClassifiedL3IPv6, [L2ValidType])
        , (L3IPv6ValidVersion, [ClassifiedL3IPv6])
        , (L3IPv6ValidLength, [ClassifiedL3IPv6])
        , (L3IPv6ValidHops, [ClassifiedL3IPv6])
        , (L3IPv6ValidSrc, [ClassifiedL3IPv6])
        , (L3IPv6ValidDest, [ClassifiedL3IPv6])
        , (L3IPv6ValidProtocol, [ClassifiedL3IPv6])
        , (VerifiedL3IPv6 , [L3IPv6ValidVersion, L3IPv6ValidLength
            , L3IPv6ValidHops, L3IPv6ValidSrc, L3IPv6ValidDest
            , L3IPv6ValidProtocol])
        , (VerifiedL3IPv6, [VerifiedL2Ethernet])


        , (ClassifiedL4ICMP, [L3IPv4ValidProtocol]) -- ICMP classification
        , (ClassifiedL4ICMP, [L3IPv6ValidProtocol]) --
        , (L4ICMPValidType, [ClassifiedL4ICMP])
        , (VerifiedL4ICMP, [L4ICMPValidType])
        , (VerifiedL4ICMP, [VerifiedL3IPv6])
        , (VerifiedL4ICMP, [VerifiedL3IPv4])

        , (ClassifiedL4UDP, [L3IPv4ValidProtocol]) -- UDP classification
        , (ClassifiedL4UDP, [L3IPv6ValidProtocol]) --
        , (L4UDPValidSrc, [ClassifiedL4UDP])
        , (L4UDPValidDest, [ClassifiedL4UDP])
        , (L4UDPValidLength, [ClassifiedL4UDP])
        , (L4UDPValidChecksum, [ClassifiedL4UDP])
        , (VerifiedL4UDP, [L4UDPValidSrc, L4UDPValidDest
            , L4UDPValidLength, L4UDPValidChecksum])
        , (VerifiedL4UDP, [VerifiedL3IPv4])
        , (VerifiedL4UDP, [VerifiedL3IPv6])


        , (ClassifiedL4TCP, [L3IPv4ValidProtocol]) -- TCP classification
        , (ClassifiedL4TCP, [L3IPv6ValidProtocol]) --

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
            , L4TCPValidState])
        , (VerifiedL4TCP, [VerifiedL3IPv6])
        , (VerifiedL4TCP, [VerifiedL3IPv4])

        , (L4TCPUpdateProtoState, [VerifiedL4TCP])
        -- TCP writeback State

        -- clasifies the next level protocol
        , (ToUserMemory, [L4TCPUpdateProtoState])
        , (ToUserMemory, [VerifiedL4UDP])
        , (ToUserMemory, [VerifiedL4TCP])
    ]


data Module = Module {
        name :: String
        , computations :: [Computation]
    } deriving (Eq, Ord, DD.Typeable, DD.Data)

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
        putStrLn out2
        putStrLn lineBreak
--        putStrLn $ show gr

    where
        lineBreak = "\n\n"
        -- m = L4TCPValidAckNo
        m = Module "Ethernet" [L2ValidLen, L2ValidCRC]
        out1 = show $ getModLst
        out2 = show $ DD.typeOf (m)
--        gr = DG.buildG (1,3) [(1, 2),(2, 2),(2, 2),(2, 3),(1, 3)]
--        gr = DG.buildG (1,3) [(1,'a',2),(2,'a',2),(2,'b',2),(2,'c',3),(1,'a',3)]

main_debug :: IO()
main_debug = do
        putStrLn "########### Actual Graph #############"
        putStrLn out1
        putStrLn "########### Vertices #############"
        putStrLn out2
        putStrLn "########### Edges #############"
        putStrLn out3
        putStrLn "########### DoT #############"
        putStrLn outDot

    where
        lineBreak = "\n\n"
        out1 = myShowList $ getNetworkDependencyDummy
        out2 = myShowList $ getVertices getNetworkDependencyDummy
        out3 = myShowList $ getEdges getNetworkDependencyDummy
        outDot = showGraphViz (getVertices getNetworkDependencyDummy)
                    (getEdges getNetworkDependencyDummy)


main  :: IO()
main = do
        putStrLn outDot
    where
        outDotDummy = showGraphViz (getVertices getNetworkDependencyDummy)
                    (getEdges getNetworkDependencyDummy)
        outDot = showGraphViz (getVertices getNetworkDependency)
                    (getEdges getNetworkDependency)

