#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Unicorn
import qualified Operations as OP
import DotGenerator as DG
import Embedding as E
import qualified Data.List as L

[unicorn|
graph prg {
    cluster L2Ether {
        boolean Classified {
            port true[ValidType]
            port false[] }

        boolean ValidType {
            port true[ValidCRC]
            port false[] }

        boolean ValidCRC {
            attr "foo"
            port true[ValidBroadcast ValidMulticast ValidUnicast]
            port false[] }

        boolean ValidUnicast {
            port true false[ValidDest] }

        boolean ValidMulticast {
            port true false[ValidDest] }

        boolean ValidBroadcast {
            port true false[ValidDest] }

        or ValidDest {
            port true[.Queue0]
            port false[] }
    }

    node Queue0 {
        port out[] }
}
|]

[unicorn|
graph lpg {
    cluster L2Ether {
        boolean Classified {
            port true[ValidType ValidUnicast ValidMulticast ValidBroadcast
                      ValidCRC ValidSrc]
            port false[] }

        boolean ValidType {
            port true[.L3IPv6Classified .L3IPv4Classified]
            port false[] }

        boolean ValidUnicast {
            port true false[ValidDest] }

        boolean ValidMulticast {
            port true false[ValidDest] }

        boolean ValidBroadcast {
            port true false[ValidDest] }

        or ValidDest {
            port true false[.L2Verified] }

        boolean ValidCRC {
            port true false[.L2Verified] }

        boolean ValidSrc {
            port true false[.L2Verified] }

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
            port true false[ValidProtocol] }

        boolean ValidProtocol {
            port true false[.L3Classified] }
    }

    or L3Classified {
        port true[L4UDPClassified]
        port false[] }

    cluster L4UDP {
        boolean Classified {
            port true[ValidChecksum ValidSrc ValidDest ValidLength .L4Classified]
            port false[] }

        boolean ValidChecksum {
            port true false[Verified] }

        boolean ValidSrc {
            port true false[Verified] }

        boolean ValidDest {
            port true false[Verified] }

        boolean ValidLength {
            port true false[Verified] }

        and Verified {
            port true false[.L4Verified] }
    }

    or L4Classified {
        port true false[DefaultKernelProcessing] }

    or L4Verified {
        port true false[DefaultKernelProcessing] }

    and DefaultKernelProcessing {
        port true false[] }
}
|]

strEdge (a,_,b) = "(" ++ (OP.nLabel a) ++ "," ++ (OP.nLabel b) ++ ")"

main = do
    --putStrLn (DG.toDotClustered lpgClusters lpgNodes)
    putStrLn (DG.toDotFromDLP embedded)
    --putStrLn ("[" ++ (L.intercalate "\n" $ map strEdge lpgDep) ++ "]")
    where
        embedded = E.testEmbeddingV3 prgL2EtherClassified lpgL2EtherClassified
        {-prgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ prgL2EtherClassified
        lpgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ lpgL2EtherClassified-}
