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
import qualified System.IO as SI
import qualified System.Cmd as SC
import System.IO.Temp
import System.Exit

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


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a
third3 :: (a,b,c) -> c
third3 (_,_,a) = a

-- Check if an OP node is an AND node (not very pretty :-( )
nOPIsAnd :: OP.Node -> Bool
nOPIsAnd n = L.isInfixOf ":AND:" $ OP.nLabel n

-- Helpers for making displaying these things less messy
showNode :: OP.Node -> String
showNode n = OP.nLabel n ++ "[" ++ OP.nTag n ++ "]"
showEdge :: (OP.Node,String,OP.Node) -> String
showEdge (a,b,c) = "(" ++ showNode a ++ "," ++ b ++ "," ++ showNode c ++ ")"
showConstr :: ((OP.Node,String),CExp) -> String
showConstr ((n,p),e) = "(" ++ showNode n ++ "." ++ p ++ " = " ++ show e ++ ")"



-- Wrappers for building logical expressions
-- Can be used to switch between generating CNF on the fly and generating the
-- raw expression.
type CExp = BE.CNFBExp
cAnd :: CExp -> CExp -> CExp
cAnd = BE.cnfAnd
cOr :: CExp -> CExp -> CExp
cOr = BE.cnfOr
cNot :: CExp -> CExp
cNot = BE.cnfNot
cVar :: String -> CExp
cVar = BE.cnfVar
cImpl :: CExp -> CExp -> CExp
cImpl a b = cNot a `cOr` b
cAndL :: [CExp] -> CExp
cAndL = BE.cnfAndL
cOrL :: [CExp] -> CExp
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
        ("L4UDPClassified","T") -> Just $ cVar "UDP"
        ("L4UDPClassified","F") -> Just $ cNot $ fromJust $ nConst n "T"
        ("HWIsTCPSyn","T") -> Just $ cAnd (cVar "TCP") (cVar "TCPSYNFlag")
        ("HWIsTCPSyn","F") -> Just $ cNot $ fromJust $ nConst n "T"
        _ -> Nothing
    where
        stripL = drop 4

-- Additional constraints (that are not non-specific
additionalConstraints :: [((OP.Node,String),CExp)] -> CExp
additionalConstraints _ = cAndL [
        cVar "DestPort53" `cImpl` cNot (cVar "DestPort67"),
        cVar "DestPort67" `cImpl` cNot (cVar "DestPort53"),
        cVar "TCP" `cImpl` cNot (cVar "UDP"),
        cVar "UDP" `cImpl` cNot (cVar "TCP")
    ]

-- Build up constraints for all nodes
-- This is done by topologically sorting the nodes and iteratively adding
-- constraints to the list while using the constraints of the predecessors.
generateConstraints :: [(OP.Node,String,OP.Node)] -> [((OP.Node,String),CExp)]
generateConstraints g = foldl node []  $ E.topSort3 g
    where
        node l n
            | OP.nIsOP n = ((n,"T"),opT) : ((n,"F"),opF) : l
            | otherwise = foldl handlePort l ports
            where
                inE = filter ((== n) . third3) g
                outE = filter ((== n) . fst3) g
                getConst a = fromJust $ lookup a l

                -- specific for F nodes
                ports = L.nub $ map snd3 outE
                inC =
                    case inE of
                        [] -> cVar "Dummy"
                        [(a,b,_)] -> getConst (a,b)
                        _ -> TR.trace ("Warning: Multiple incoming edges on F node " ++ OP.nLabel n) (
                            cOrL $ map (\(n',p',_) -> getConst (n',p')) inE)
                handlePort l' p =
                    case nConst n p of
                        Nothing -> ((n,p),inC):l'
                        Just e -> ((n,p),cAnd inC e):l'

                -- specific for OP nodes
                opFun = if nOPIsAnd n then cAndL else cOrL
                opT = opFun $ map (\(n',p',_) -> getConst (n',p')) $ filter ((== "T") . snd3) inE
                opF = cNot opT

-- Add the additionalConstraints to all constraints
addAdditional :: [((OP.Node,String),CExp)] -> [((OP.Node,String),CExp)]
addAdditional cs = map (\(a,e) -> (a, cAnd e $ additionalConstraints cs)) cs


data Ternary = TTrue | TFalse | TZ
    deriving (Show, Eq)

-- Takes an expression and checks if it is satisfiable by writing it to a
-- file and passing it to minisat.
isSAT :: BE.CNFBExp -> IO Ternary
isSAT e = withSystemTempFile "toMinisat.dimac" toMinisat
    where
        toMinisat fp h = do
            SI.hPutStr h (BE.toDIMACS e)
            SI.hFlush h
            rc <- SC.system ("minisat " ++ fp ++ " >/dev/null")
            return (case rc of
                (ExitFailure 10) -> TTrue  -- Satisfiable
                (ExitFailure 20) -> TFalse -- Not satisfiable
                _ -> TZ)
                

-- Apply list of unsatisfiable ports to graph
applyConstraints :: [(OP.Node,String,OP.Node)] -> [(OP.Node,String)] -> [(OP.Node,String,OP.Node)]
applyConstraints g u = filter (\(n,p,_) -> notElem (n,p) u) g


main :: IO()
main = do
    putStrLn "Writing Embedded Graph to Embedded.dot"
    writeFile "Embedded.dot" $ DG.toDotFromDLPTagClustered embedded

    putStrLn "Calculating constraints and checking satisfiability"
    sats <- mapM checkSatNode constraints

    putStrLn "Non-satisfiable ports:"
    let satsFiltered = filter ((== TFalse) . snd) sats
    putStrLn $ unlines $
        map (\((n,p),_) -> "    " ++ showNode n ++ "." ++ p) satsFiltered

    putStrLn "Writing constrained Embededd Graph to EmbeddedConstr.dot"
    writeFile "EmbeddedConstr.dot" $ DG.toDotFromDLPTagClustered $
        applyConstraints embedded $ map fst satsFiltered
    where
        config = [("CSynFilter", "true"), ("CSynOutput", "Q2")]
        prgU = E.getDepEdgesP prgL2EtherClassified
        prg = OP.applyConfig config prgU
        lpg = E.getDepEdgesP lpgQueue
        embedded = E.testEmbeddingV3 prg lpg

        constraints = addAdditional $ generateConstraints embedded
        checkSatNode (a,e) = do { t <- isSAT e ; return (a,t) }

