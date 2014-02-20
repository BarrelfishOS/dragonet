#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.DotGenerator
import Dragonet.Embedding
import Dragonet.Constraints
import Dragonet.Pipelines

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Implementation as Impl
import qualified Data.Graph.Inductive.Graph as DGI

import qualified LPGImpl as LPGI -- (graphGen)
import qualified LPGEx1 as LPG1 -- (graphGen)
import qualified LPGEx2 as LPG2 -- (graphGen)

import LPGImpl

[unicorn|
graph prg {

    cluster Rx {

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
        port Q0[.Queue0]
        port Q1[.Queue1]
        port Q2[.Queue2] }

    boolean HWIsUDPDest53 {
        port true[L2EtherValidUnicast]
        port false[.Queue0]
        constraint true "UDP&DestPort=53"
        constraint false "!(UDP&DestPort=53)" }

    boolean L2EtherValidUnicast {
        port true[.Queue1]
        port false[] }

    } /* end cluser : RX */

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
        port out[RxL2EtherClassified] }
//        port out[L2EtherClassified] }

    cluster Rx {

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
            port out[.L3IPv4Classified .L3IPv6Classified] }
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

        cluster Foo {
        boolean .ValidLen {
            port true false[.Verified] }
        }

        and Verified {
            port true false[..dhcpd ..named] }
//            port true false[.dhcpd .named] }
    }

    cluster L3ARP {
        boolean IsRequest {
            port true false[] }
    }

    or L4Classified {
//        port true [IsUDPDest53 IsUDPDest67]
        port true [UDPPortClassify]
        port false []}

/*
    boolean IsUDPDest53 {
        port true false[.named]
        constraint true "UDP&DestPort=53"
        constraint false "!(UDP&DestPort=53)" }

    boolean IsUDPDest67 {
        port true false[.dhcpd]
        constraint true "UDP&DestPort=67"
        constraint false "!(UDP&DestPort=67)" }
*/

    boolean UDPPortClassify {
        port true false[.dhcpd .named]
        constraint true "UDP"
        constraint false "!(UDP)" }


    } /* end cluster : Rx */

    /* Applications  */
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

-- The protocol graph


myWriteFile :: String -> String -> IO()
myWriteFile fname contents = do
    putStrLn ("Generating " ++ fname ++ " files...")
    writeFile fname contents

main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    myWriteFile "lpg.dot" $ toDotClustered lpgT lpgClusters
    myWriteFile "prg.dot" $ toDot prg
    myWriteFile "prg_conf.dot" $ toDot prgTConf

    myWriteFile "embedded.dot" $ toDot $ embedded
    constrained <- constrain embedded
    myWriteFile "constrained.dot" $ toDot $ constrained

    -- Divide graph into pipelines
    let plg = generatePLG nodePipeline constrained
    myWriteFile "pipelines.dot" $ pipelinesDot plg
    mapM_ (\pl ->
        myWriteFile ("pl_" ++ plLabel pl ++ ".dot") $ toDot $ plGraph pl
        ) $ map snd $ DGI.labNodes plg

    -- Also use impl graph
    myWriteFile "lpgImpl.dot" $ toDotClustered lpgTImpl lpgClusters
    myWriteFile "embeddedImpl.dot" $ toDot $ embeddedImpl
    constrainedImpl <- constrain embeddedImpl
    myWriteFile "constrainedImpl.dot" $ toDot $ constrainedImpl


    where
        lpgT = pgSetType GTLpg lpg
        prgTConf = pgSetType GTPrg prgConfigured

        prgConfigured = applyConfig config prg
--        config = [("CSynFilter", "true"), ("CSynOutput","Q2")]
        config = [("RxCSynFilter", "true"), ("RxCSynOutput","Q2")]

        lpgTImpl = pgSetType GTLpg LPG2.lpg
        embedded = fullEmbedding prgTConf lpgT
        embeddedImpl = fullEmbedding prgTConf lpgTImpl

        nodePipeline (_,n) = nTag n

