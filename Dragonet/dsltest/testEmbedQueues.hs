#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Unicorn
import qualified Operations as OP
import DotGenerator as DG
import Embedding as E
import qualified Data.List as L
import Data.Maybe

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
        port false[HWIsUDPDest53] }

    config CSynOutput {
        port Q0[Queue0]
        port Q1[Queue1]
        port Q2[Queue2] }

    boolean HWIsUDPDest53 {
        port true[Queue1]
        port false[Queue0] }


    node Queue0 {
        port out[] }

    node Queue1 {
        port out[] }

    node Queue2 {
        port out[] }
}
|]

[unicorn|
graph lpg {
    node Queue0 {
        port out[L2EtherClassified] }

    node Queue1 {
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
            port false[] }

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
        port true false[named] }

    boolean IsUDPDest67 {
        port true false[dhcpd] }

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

{-queueConfig :: OP.Node -> [(OP.Node,String)] -> [(String,OP.Node)] -> String -> [(OP.Node,String,OP.Node)]
queueConfig n inE outE cfg = map (\(a,b) -> (a,b,queueN)) inE
    where queueN = OP.getDecNode ("Queue" ++ cfg) "" (OP.NaryNode []) []-}


mainPaper :: IO()
mainPaper = do
    writeFile ("PRGUnconf" ++ suffix ++ ".dot") $ DG.toDotFromDLP prgU
    writeFile ("PRG" ++ suffix ++ ".dot") $ DG.toDotFromDLP prg
    writeFile ("LPG" ++ suffix ++ ".dot") $ DG.toDotFromDLP lpg
    writeFile ("PRG" ++ suffix ++ "-clustered.dot") $ DG.toDotClustered prgClusters prgNodes
    writeFile ("LPG" ++ suffix ++ "-clustered.dot") $ DG.toDotClustered lpgClusters lpgNodes
    writeFile ("Embedded" ++ suffix ++ ".dot") $ DG.toDotFromDLP embedded
    where
        suffix = "paper"
        embedded = E.testEmbeddingV3 prg lpg

        prgU = E.getDepEdgesP prgL2EtherClassified
        prg = OP.applyConfig config prgU
        lpg = E.getDepEdgesP lpgQueue0

        config = [("CSynFilter", "true"), ("CSynOutput", "Q2")]


main = mainPaper
