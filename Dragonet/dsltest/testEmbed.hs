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
    cluster L2Ether {
        boolean Classified {
            port true[ValidType]
            port false[] }

        boolean ValidType {
            port true[CValidCRC]
            port false[] }

        config CValidCRC {
            port true[ValidCRC]
            port false[ValidBroadcast ValidMulticast ValidUnicast] }

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

applyConfig :: [(String,String)] -> [(OP.Node,String,OP.Node)] -> [(OP.Node,String,OP.Node)]
applyConfig cfg g =
    cleaned
    where
        isConfN :: OP.Node -> Bool
        isConfN (OP.Conf _) = True
        isConfN _ = False

        fst3 (a,_,_) = a
        third3 (_,_,a) = a

        sminus a b = filter (not . (flip elem b)) a

        edge :: (OP.Node,String,OP.Node) -> [(OP.Node,String,OP.Node)]
        edge (n1,p,n2) =
            if isConfN n1 then
                []
            else if isConfN n2 then
                map (\n -> (n1,p,n)) $ confDests n2
            else
                [(n1,p,n2)]

        confDests :: OP.Node -> [OP.Node]
        confDests n = map (\(_,_,c) -> c) $
            filter (\(a,b,_) -> a == n && b == (fromJust $ lookup (OP.nLabel n) cfg)) g

        sources g' = (map fst3 g') `sminus` (map third3 g')
        configured = concatMap edge g

        cleaned = rmNewSources configured

        rmNode g' n = filter (\a -> fst3 a /= n && third3 a /= n) g'

        rmNewSources g'
            | (null newSources) = g'
            | otherwise = rmNewSources g''
            where
                newSources = (sources g') `sminus` (sources g)
                n = head newSources
                g'' = rmNode g' n

main = do
    --putStrLn (DG.toDotClustered prgClusters prgNodes)
    putStrLn (DG.toDotFromDLP embedded)
    --putStrLn (DG.toDotFromDLP prg)
    --putStrLn ("[" ++ (L.intercalate "\n" $ map strEdge lpgDep) ++ "]")
    where
        embedded = E.testEmbeddingV3 prg lpg

        prgU = E.getDepEdgesP prgL2EtherClassified
        prg = applyConfig config prgU
        lpg = E.getDepEdgesP lpgL2EtherClassified

        config = [("L2EtherCValidCRC", "true")]

        {-prgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ prgL2EtherClassified
        lpgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ lpgL2EtherClassified-}
