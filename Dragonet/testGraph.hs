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
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.Text.Lazy as T
import qualified Util.GraphHelpers as GH
import qualified Data.Map as M
import Util.Misc

import qualified LPGImpl as LPGI -- (graphGen)
import qualified LPGEx1 as LPG1 -- (graphGen)
import qualified LPGEx2 as LPG2 -- (graphGen)
import qualified LPGicmp as LPGicmp -- (graphGen)

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

    node Named {
        attr "app" }
    node Dhcpd {
        attr "app" }
}
|]

-- The protocol graph


getGraphDirName :: String
getGraphDirName = "./graphsGen/"

myWriteFile :: String -> String -> IO()
myWriteFile fnamefile contents = do
    let fname = (getGraphDirName  ++ fnamefile)
    putStrLn ("Generating " ++ fname ++ " files...")
    writeFile fname contents

-- Merge nodes with the same label that have the app attribute into one, and
-- update the tag of the kept nodes to be their label
mergeAppNodes :: PGraph -> PGraph
mergeAppNodes g =
    DGI.insEdges newEdges $ DGI.insNodes newNodes $ DGI.delNodes drop g
    where
        isAppN n = elem "app" $ nAttributes n
        appNodes = GH.filterNodesByL isAppN g
        parted = partListBy (nLabel . snd) appNodes
        -- Keep one node for each app
        keep = M.fromList $ map (\(a,b) -> (a, head b)) parted
        newNodes = map fixKept $ M.elems keep
        fixKept (n,l) = (n,l { nTag = nLabel l })
        -- Drop original nodes
        dropL = concatMap snd parted
        drop = map fst dropL
        -- Replacement edges
        newEdges = concatMap fixEdges dropL
        fixEdges (n,l) = map (\(s,el) -> (s,rn,el)) $ DGI.lpre g n
            where (rn,_) = keep M.! nLabel l


main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    myWriteFile "lpg.dot" $ toDotClustered lpgT lpgClusters
    myWriteFile "prg.dot" $ toDot prg
    myWriteFile "prg_conf.dot" $ toDot prgTConf

    myWriteFile "embedded.dot" $ toDot $ embedded
    constrained <- fmap mergeAppNodes $ constrain embedded
    myWriteFile "constrained.dot" $ toDot constrained

    -- Divide graph into pipelines
    let plg = generatePLG nodePipeline constrained
    myWriteFile "pipelines.dot" $ pipelinesDot Nothing plg
    mapM_ (\pl ->
        myWriteFile ("pl_" ++ plLabel pl ++ ".dot") $ toDot $ plGraph pl
        ) $ map snd $ DGI.labNodes plg

    -- Also use impl graph
    myWriteFile "lpgImpl.dot" $ toDotClustered lpgTImpl LPG2.lpgClusters
    myWriteFile "embeddedImpl.dot" $ toDot $ embeddedImpl
    constrainedImpl <- constrain embeddedImpl
    myWriteFile "constrainedImpl.dot" $ toDot $ constrainedImpl

    putStrLn $ show LPG2.lpgClusters

    let plg' = generatePLG (nodeImplPipeline LPG2.lpgClusters) lpgTImpl
    myWriteFile "pipelines_impl.dot" $ pipelinesDot (Just linkMap) plg'
    mapM_ (\pl ->
        myWriteFile ("pli_" ++ plLabel pl ++ ".dot") $
            toDotWith' (plDotParams pl) $ plGraph pl) $
            map snd $ DGI.labNodes plg'



    -- Also use impl graph for ICMP
    myWriteFile "lpgICMPImpl.dot" $ toDotClustered lpgTIcmpImpl LPGicmp.lpgClusters

    myWriteFile "embeddedICMPImpl.dot" $ toDot $ embeddedIcmpImpl
    constrainedIcmpImpl <- constrain embeddedIcmpImpl
    myWriteFile "constrainedICMPImpl.dot" $ toDot $ constrainedIcmpImpl

    putStrLn $ show LPGicmp.lpgClusters

    let plg' = generatePLG (nodeImplPipeline LPGicmp.lpgClusters) lpgTIcmpImpl
    myWriteFile "pipelines_icmp_impl.dot" $ pipelinesDot (Just linkMap) plg'
    mapM_ (\pl ->
        myWriteFile ("pli_icmp_" ++ plLabel pl ++ ".dot") $
            toDotWith' (plDotParams pl) $ plGraph pl) $
            map snd $ DGI.labNodes plg'


    where
        lpgT = pgSetType GTLpg lpg
        prgTConf = pgSetType GTPrg prgConfigured

        prgConfigured = applyConfig config prg
--        config = [("CSynFilter", "true"), ("CSynOutput","Q2")]
        config = [("RxCSynFilter", CVEnum 0), ("RxCSynOutput",CVEnum 2)]

        lpgTImpl = pgSetType GTLpg LPG2.lpg
        embedded = fullEmbedding prgTConf lpgTImpl
        embeddedImpl = fullEmbedding prgTConf lpgTImpl

        -- for ICMP Implementation graph
        lpgTIcmpImpl = pgSetType GTLpg LPGicmp.lpg
        embeddedIcmpImpl = fullEmbedding prgTConf lpgTIcmpImpl


        nodePipeline (_,n) = nTag n
        nodeImplPipeline cl (n,_) =
            case lookup n cl of
                Just [] -> "rest"
                Just c -> last c
                Nothing -> "rest"
        linkMap pl = "pli_" ++ plLabel pl ++ ".svg"
        plDotParams pl p = p {
            GV.globalAttributes = GV.globalAttributes p ++ [
                GV.GraphAttrs [GA.Label $ GA.StrLabel $ T.pack $ plLabel pl,
                               GA.LabelLoc $ GA.VTop]] }

