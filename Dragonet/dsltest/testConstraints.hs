#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Unicorn
import qualified Operations as OP
import DotGenerator as DG
import Embedding as E
import qualified Data.List as L
import Data.Maybe
import qualified Data.Char as C
import qualified Debug.Trace as TR
import qualified BoolExp as BE

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
        port true[L2EtherValidUnicast]
        port false[Queue0] }

    boolean L2EtherValidUnicast {
        port true[Queue1]
        port false[] }

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
            port false[.L4Classified] }

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


{-simplifyN (BEOr BETrue _) = BETrue
simplifyN (BEOr _ BETrue) = BETrue
simplifyN (BEAnd BETrue e) = e
simplifyN (BEAnd e BETrue) = e
simplifyN e = e

{-simplify (BEAnd a b) = simplifyN (BEAnd (simplify a) (simplify b))
simplify (BEOr a b) = simplifyN (BEOr (simplify a) (simplify b))
simplify (BENot a) = simplifyN (BENot (simplify a))
simplify e = simplifyN e-}
simplify = recInOut simplifyN-}



type CExp = BE.CNFBExp

cAnd = BE.cnfAnd
cOr = BE.cnfOr
cNot = BE.cnfNot
cVar = BE.cnfVar
cImpl a b = (cNot a) `cOr` b

cAndL = BE.cnfAndL
cOrL = BE.cnfOrL

-- Get constraints for specific port on node
nConst :: OP.Node -> String -> Maybe CExp
nConst n p =
    case (stripL $ OP.nLabel n,p) of
        ("IsUDPDest53","T") -> Just $ cAnd (cVar "UDP") (cVar "DestPort53")
        ("IsUDPDest53","F") -> Just $ cNot $ fromJust $ nConst n "T"
        ("IsUDPDest67","T") -> Just $ cAnd (cVar "UDP") (cVar "DestPort67")
        ("IsUDPDest67","F") -> Just $ cNot $ fromJust $ nConst n "T"
        ("HWIsUDPDest53","T") -> Just $ cAnd (cVar "UDP") (cVar "DestPort53")
        ("HWIsUDPDest53","F") -> Just $ cNot $ fromJust $ nConst n "T"
        ("L4UDPClassified","T") -> Just $ (cVar "UDP")
        ("L4UDPClassified","F") -> Just $ cNot $ fromJust $ nConst n "T"
        _ -> Nothing
    where
        stripL s = drop 4 s

additionalConstraints = cAndL [
        ((cVar "DestPort53") `cImpl` (cNot $ cVar "DestPort67")),
        ((cVar "DestPort67") `cImpl` (cNot $ cVar "DestPort53"))
    ]

fst3 (a,_,_) = a
snd3 (_,a,_) = a
third3 (_,_,a) = a

nOPIsAnd :: OP.Node -> Bool
nOPIsAnd n = L.isInfixOf ":AND:" $ OP.nLabel n

findSources :: [(OP.Node,String,OP.Node)] -> [OP.Node]
findSources g = (map fst3 g) `lminus` (map third3 g)
    where
        lminus a b = filter (not . (flip elem b)) a

generateConstraints :: [(OP.Node,String,OP.Node)] -> [((OP.Node,String),CExp)]
generateConstraints g = foldl node []  $ E.topSort3 g
    where
        node l n
            | OP.nIsOP n = ((n,"T"),opT) : ((n,"F"),opF) : l
            | otherwise = (foldl handlePort l ports)
            where
                inE = filter ((== n) . third3) g
                outE = filter ((== n) . fst3) g
                evaluate a = fromJust $ lookup a l

                -- specific for F nodes
                ports = L.nub $ map snd3 outE
                [inEdge] = inE
                inC =
                    case inE of
                        [] -> cVar "Dummy"
                        [(a,b,_)] -> evaluate (a,b)
                        _ -> TR.trace ("Warning: Multiple incoming edges on F node " ++ (OP.nLabel n)) (
                            cOrL $ map (\(n',p',_) -> evaluate (n',p')) inE)
                handlePort l' p =
                    case nConst n p of
                        Nothing -> ((n,p),inC):l'
                        Just e -> ((n,p),cAnd inC e):l'

                -- specific for OP nodes
                opFun = if nOPIsAnd n then cAndL else cOrL
                opT = opFun $ map (\(n',p',_) -> evaluate (n',p')) $ filter ((== "T") . snd3) inE
                opF = cNot opT


addAdditional :: [((OP.Node,String),CExp)] -> [((OP.Node,String),CExp)]
addAdditional cs = map (\(a,e) -> (a,(cAnd e additionalConstraints))) cs

showNode n = (OP.nLabel n) ++ "[" ++ (OP.nTag n) ++ "]"
showEdge (a,b,c) = "(" ++ (showNode a) ++ "," ++ b ++ "," ++ (showNode c) ++ ")"
showConstr ((n,p),e) = "(" ++ (showNode n) ++ "." ++ p ++ " = " ++ (show e) ++ ")"

cLookup :: String -> String -> String -> [((OP.Node,String),CExp)] -> ((OP.Node,String),CExp)
cLookup l t p cs = head $ filter (\((n,p'),e) -> (OP.nLabel n == l) && (OP.nTag n == t) && (p' == p)) cs


mainPaper :: IO()
mainPaper = do
    {-writeFile ("PRGUnconf" ++ suffix ++ ".dot") $ DG.toDotFromDLP prgU
    writeFile ("PRG" ++ suffix ++ ".dot") $ DG.toDotFromDLP prg
    writeFile ("LPG" ++ suffix ++ ".dot") $ DG.toDotFromDLP lpg
    writeFile ("PRG" ++ suffix ++ "-clustered.dot") $ DG.toDotClustered prgClusters prgNodes
    writeFile ("LPG" ++ suffix ++ "-clustered.dot") $ DG.toDotClustered lpgClusters lpgNodes-}
    --writeFile ("Embedded" ++ suffix ++ ".dot") $ DG.toDotFromDLP $ embedded
    --putStrLn $ unlines $ map (showConstr) $ reverse $ addAdditional $ generateConstraints embedded
    --putStrLn $ BE.toDIMACS $ snd $ cLookup "LPG:IsUDPDest67" "Queue1" "T" constraints
    mapM_ genF constraints
    where
        suffix = "paper"
        embedded = E.testEmbeddingV3 prg lpg

        prgU = E.getDepEdgesP prgL2EtherClassified
        prg = OP.applyConfig config prgU
        lpg = E.getDepEdgesP lpgQueue

        config = [("CSynFilter", "true"), ("CSynOutput", "Q2")]

        constraints = addAdditional $ generateConstraints embedded
        genF (a,e) = writeFile ("satout/" ++ (fname a)) $ BE.toDIMACS e
        sanitizeFN fn = filter (\c -> C.isAlphaNum c || (c == '_')) fn
        fname (n,p) = sanitizeFN ((OP.nLabel n) ++ "_" ++ (OP.nTag n) ++ "_" ++ p)


main = mainPaper
