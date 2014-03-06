#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.DotGenerator
import Dragonet.Embedding
import Dragonet.Constraints

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DGIDFS
import qualified Data.List as L

[unicorn|
graph prg {
    node HWDrop { }

    cluster L2Ether {
        boolean Classified {
            port true[ValidCRC]
            port false[] }

        boolean ValidCRC {
            port true[ClassifyL3_]
            port false[.HWDrop] }

        node ClassifyL3_ {
            port ipv4[.L3IPv4Classified .L3IPv4Checksum_]
            port other[.CSynFilter] }

    }
    
    cluster L3IPv4 {

        boolean Classified {
            attr "software"
            port true false[] } 
    
        node Checksum_ {
            port out[ValidChecksum .CSynFilter] }

        boolean ValidChecksum {
            attr "software"
            port true false[] }
    }

    config CSynFilter {
        port true[HWIsTCPSyn]
        port false[HWIsUDPDest53] }

    boolean HWIsTCPSyn {
        port true[CSynOutput]
        port false[HWIsUDPDest53]
        constraint true "TCP&TCPSYNFlag"
        constraint false "!(TCP&TCPSYNFlag)" }

    config CSynOutput {
        port Q0[Queue0]
        port Q1[Queue1]
        port Q2[Queue2] }

    boolean HWIsUDPDest53 {
        port true[L2EtherValidUnicast]
        port false[Queue0]
        constraint true "UDP&DestPort=53"
        constraint false "!(UDP&DestPort=53)" }

    boolean L2EtherValidUnicast {
        port true[Queue1]
        port false[] }

    node Queue0 {
        attr "software"
        port out[] }

    node Queue1 {
        attr "software"
        port out[] }

    node Queue2 {
        attr "software"
        port out[] }
}
|]

[unicorn|
graph lpg {
    node Queue {
        port out[L2EtherClassified] }

    cluster L2Ether {
        boolean Classified {
            port true[ValidType ValidUnicast ValidBroadcast
                      ValidCRC ValidSrc]
            port false[] }

        boolean ValidType {
            port true[ClassifyL3]
            port false[] }

        boolean ValidUnicast {
            port true false[ValidDst] }

        boolean ValidBroadcast {
            port true[ValidDst .L3ARPIsRequest]
            port false[ValidDst] }

        or ValidDst {
            port true false[.L2Verified] }

        boolean ValidCRC {
            port true false[.L2Verified] }

        boolean ValidSrc {
            port true false[.L2Verified] }

        node ClassifyL3 {
            port out[.L3IPv4Classified] }

	/*
        boolean L3IPv4Classified {
            port true[.L3IPv4Classified]
            port false[] }

	*/

    }

    and L2Verified {
        port true false[L3IPv4Verified] }

    cluster L3IPv4 {
        boolean Classified {
            port true[ValidChecksum ValidProtocol]
            port false[] }

        boolean ValidChecksum {
            port true false[Verified] }

        boolean ValidProtocol {
            port true false[.L3Classified] }

        and Verified {
            port true false[.L4UDPVerified] }
    }

    cluster L3IPv6 {
        boolean Classified {
            port true[ValidProtocol]
            port false[] }

        boolean ValidProtocol {
            port true false[.L3Classified] }
    }

    or L3Classified {
        port true[L4UDPClassified]
        port false[] }

    cluster L4UDP {
        boolean Classified {
            port true[ValidChecksum ValidSrc ValidDst ValidLen .L4Classified]
            port false[.L4Classified]
            constraint true "UDP"
            constraint false "!UDP" }

        boolean ValidChecksum {
            port true false[Verified] }

        boolean ValidSrc {
            port true false[Verified] }

        boolean ValidDst {
            port true false[Verified] }

        boolean ValidLen {
            port true false[Verified] }

        and Verified {
            port true false[.dhcpd .named] }
    }

    cluster L3ARP {
        boolean IsRequest {
            port true false[] }
    }

    or L4Classified {
        port true [IsUDPDest53 IsUDPDest67]
        port false []}

    /*
    or L4Verified {
        port true false[dhcpd  named] }

    */
    /*
    and Default {
        port true false[] }
    */

    boolean IsUDPDest53 {
        port true false[named]
        constraint true "UDP&DestPort=53"
        constraint false "!(UDP&DestPort=53)" }

    boolean IsUDPDest67 {
        port true false[dhcpd]
        constraint true "UDP&DestPort=67"
        constraint false "!(UDP&DestPort=67)" }

    and named {
        port true [Named]
	port false[] }

    and dhcpd {
        port true [Dhcpd]
	port false[] }

    node Named {}
    node Dhcpd {}
}
|]

-- Drop all nodes before the QueueX nodes
dropBeforeQ :: PGraph -> PGraph
dropBeforeQ g = DGI.delNodes dropN g
    where
        dropN = filter (`notElem` reach) $ L.nub $ DGI.nodes g
        reach = concatMap (`DGIDFS.reachable` g) queues
        --queues = map fst $ GH.filterNodesByL (L.isPrefixOf "Queue" . nLabel) g
        queues = map fst $ GH.filterNodesByL ((== "Queue0") . nLabel) g

nodeLabel :: String -> String
nodeLabel nodename = t ++ (short $ removeprefix s)
  where
  (t,s) = splittype nodename
  splittype x
        | (L.isPrefixOf "P:" x)  = ("P:", (drop (length "P:") x)) 
        | (L.isPrefixOf "L:" x)  = ("L:", (drop (length "L:") x)) 
        | otherwise  = ("", x)
  removeprefix x
        | (L.isPrefixOf "L2EtherL3IPv3" x) = "EthIPv4" ++ (drop (length "L2Ether") x)
        | (L.isPrefixOf "L2Ether" x) = "Eth" ++ (drop (length "L2Ether") x)
        | (L.isPrefixOf "L3IPv4" x)  = "IPv4" ++ (drop (length "L3IPv4") x)
        | (L.isPrefixOf "L3ARP" x)  = "ARP" ++ (drop (length "L3ARP") x)
        | (L.isPrefixOf "L4UDP" x)   = "UDP" ++ (drop (length "L4UDP") x)
        | otherwise  = x 
  short x
        | (x == "CSynOutput") = "CSynOut"
        | (x == "IsUDPDest53") = "UDP/*:53"
        | (x == "IsUDPDest67") = "UDP/*:67"
        | (x == "SoftwareEntry") = "SW"
        | (L.isPrefixOf "OR:" x) = "OR"
        | (L.isPrefixOf "AND:" x) = "AND"
        | (L.isPrefixOf "Queue" x) = "Q" ++ (drop (length "Queue") x)
        | (L.isSuffixOf "Broadcast" x) = (take ((length x) - (length "Broadcast")) x) ++ "Bcast"
        | (L.isSuffixOf "IsRequest" x) = (take ((length x) - (length "IsRequest")) x) ++ "Req"
        | (L.isSuffixOf "Protocol" x) = (take ((length x) - (length "Protocol")) x) ++ "Prot"
        | (L.isSuffixOf "Checksum" x) = (take ((length x) - (length "Checksum")) x) ++ "Csum"
        | (L.isSuffixOf "Classified" x) = "Is" ++ (take ((length x) - (length "Classified")) x)
        | otherwise  = x 

portLabel :: String -> String
portLabel "true" = "T"
portLabel "false" = "F"
portLabel p = p

main:: IO()
main = do
    writeFile ("PRGUnconf" ++ suffix ++ ".dot") $ dot prg
    writeFile ("PRG" ++ suffix ++ ".dot") $ dot prgTConf
    writeFile ("LPG" ++ suffix ++ ".dot") $ dot lpg
    writeFile ("PRG" ++ suffix ++ "-clustered.dot") $ dotClustered prg prgClusters
    writeFile ("LPG" ++ suffix ++ "-clustered.dot") $ dotClustered lpg lpgClusters
    writeFile ("Embedded" ++ suffix ++ ".dot") $ dot $ dropBeforeQ embedded
    constrained <- constrain embedded
    writeFile ("EmbeddedFull" ++ suffix ++ ".dot") $ dot constrained
    where
        suffix = "paper"

        lpgT = pgSetType GTLpg lpg
        prgTConf = pgSetType GTPrg prgConfigured

        prgConfigured = applyConfig config prg
        config = [("CSynFilter", "true"), ("CSynOutput","Q2")]

        embedded = fullEmbedding prgTConf lpgT

        dot = toDotWith nodeLabel portLabel
        dotClustered = toDotClusteredWith nodeLabel portLabel

