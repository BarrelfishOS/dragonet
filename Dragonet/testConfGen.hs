#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.DotGenerator
import Dragonet.Embedding
import Dragonet.Constraints
import Dragonet.Implementation.IPv4 as IP4

import Data.Word
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Graph.Inductive as DGI
import qualified Util.Misc as UM
import qualified Util.GraphHelpers as GH
import qualified Util.GraphMonad as GM
import Control.Monad

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
            port other[.C5TupleFilter] }

    }
    
    cluster L3IPv4 {

        boolean Classified {
            attr "software"
            port true false[] } 
    
        node Checksum_ {
            port out[ValidChecksum .C5TupleFilter] }

        boolean ValidChecksum {
            attr "software"
            port true false[] }
    }

    config C5TupleFilter {
        function config5tuple
        port queues[Q0Valid Q1Valid Q2Valid]
        port default[ToDefaultQueue] }

    boolean ToDefaultQueue {
        port true false[Q0Valid] }

    or Q0Valid {
        port true[Queue0]
        port false[] }
    node Queue0 {
        attr "software"
        port out[] }

    or Q1Valid {
        port true[Queue1]
        port false[] }
    node Queue1 {
        attr "software"
        port out[] }

    or Q2Valid {
        port true[Queue2]
        port false[] }
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
            port true false[] }
    }

    cluster L3ARP {
        boolean IsRequest {
            port true false[] }
    }

    or L4Classified {
        port true []
        port false []}
}
|]

type IPv4Address = Word32
type UDPPort = Word16

data C5TL3Proto = C5TPL3IPv4 | C5TPL3IPv6
instance Show C5TL3Proto where
    show C5TPL3IPv4 = "IPv4"
    show C5TPL3IPv6 = "IPv6"

data C5TL4Proto = C5TPL4TCP | C5TPL4UDP
instance Show C5TL4Proto where
    show C5TPL4TCP = "TCP"
    show C5TPL4UDP = "UDP"

data C5Tuple = C5Tuple {
    c5tPriority :: Int,
    c5tQueue    :: Int,
    c5tL3Proto  :: Maybe C5TL3Proto,
    c5tL4Proto  :: Maybe C5TL4Proto,
    c5tL3Src    :: Maybe String,
    c5tL3Dst    :: Maybe String,
    c5tL4Src    :: Maybe UDPPort,
    c5tL4Dst    :: Maybe UDPPort
}

c5tString :: C5Tuple -> String
c5tString c = "5T("++l3p++"/"++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l3p = fromMaybe "*" $ liftM show $ c5tL3Proto c
        l4p = fromMaybe "*" $ liftM show $ c5tL4Proto c
        l3s = fromMaybe "*" $ c5tL3Src c
        l3d = fromMaybe "*" $ c5tL3Dst c
        l4s = fromMaybe "*" $ liftM show $ c5tL4Src c
        l4d = fromMaybe "*" $ liftM show $ c5tL4Dst c

c5tAttr :: C5Tuple -> [String]
c5tAttr c =
    if null constr then [] else [aTrue, aFalse]
    where
        constr = catMaybes $ [
                do {p <- c5tL3Proto c; return (show p)},
                do {p <- c5tL4Proto c; return (show p)},
                do {s <- c5tL3Src c; return ("SourceIP=" ++ s)},
                do {d <- c5tL3Dst c; return ("DestIP=" ++ d)},
                do {s <- c5tL4Src c; return ("SourcePort=" ++ show s)},
                do {d <- c5tL4Dst c; return ("DestPort=" ++ show d)} ]
        cT = foldl1 (\a b -> a ++ "&" ++ b) constr
        aTrue = "C.true:" ++ cT
        aFalse = "C.false:!(" ++ cT ++ ")"
            

parse5tCFG :: String -> [C5Tuple]
parse5tCFG s = map parseTuple $ UM.splitBy '#' s
    where
        parseTuple s' = C5Tuple {
            c5tPriority = read (parts !! 0),
            c5tQueue    = read (parts !! 1),
            c5tL3Proto  = if (parts !! 2) == "IPv4" then Just C5TPL3IPv4 else
                          if (parts !! 2) == "IPv6" then Just C5TPL3IPv6 else
                          Nothing,
            c5tL4Proto  = if (parts !! 3) == "TCP" then Just C5TPL4TCP else
                          if (parts !! 3) == "UDP" then Just C5TPL4UDP else
                          Nothing,
            c5tL3Src    = if null (parts !! 4) then Nothing else
                          Just (parts !! 4),
            c5tL3Dst    = if null (parts !! 5) then Nothing else
                          Just (parts !! 5),
            c5tL4Src    = if null (parts !! 6) then Nothing else
                          Just (read (parts !! 6)),
            c5tL4Dst    = if null (parts !! 7) then Nothing else
                          Just (read (parts !! 7))
        }
            where
            parts = UM.splitBy ',' s'


config5tuple :: ConfFunction
config5tuple _ inE outE cfg = do
    ((endN,endP),edges) <- foldM addFilter (start,[]) cfgs
    let lastEdge = (endN,defaultN,endP)
    return (edges ++ [lastEdge])
    where
        -- Node and Port for the incoming edge for the first node
        start = (fst $ fst $ head inE, snd $ head inE)
        -- Node for default queue
        (Just ((defaultN,_),_)) = L.find ((== "default") . snd) outE
        -- Lookup node id for specified queue
        queue i = queueN
            where
                (Just ((queueN,_),_)) =
                    L.find (\((_,n),_) -> nLabel n == "Q" ++ show i ++ "Valid") outE
        
        -- Get filter configurations ordered by priority
        cmpPrio a b = compare (c5tPriority a) (c5tPriority b)
        cfgs = L.sortBy cmpPrio $ parse5tCFG cfg

        -- Generate node and edges for one filter
        bports = ["true","false"]
        nodeL c = baseFNode (c5tString c) (c5tAttr c) bports Nothing
        addFilter ((iN,iE),es) c = do
            (n,_) <- confMNewNode $ nodeL c
            let inEdge = (iN,n,iE)
            let tEdge = (n,queue $ c5tQueue c,"true")
            let fEdge = (n,queue $ c5tQueue c,"false")
            return ((n,"false"), es ++ [inEdge,tEdge,fEdge])
            






data Flow =
    UDPIPv4Listen IPv4Address UDPPort |
    UDPIPv4Flow IPv4Address IPv4Address UDPPort UDPPort

type SocketID = Int
data SocketDesc = SocketDesc {
    sdID :: SocketID,
    sdFlow :: Flow,
    sdNodes :: [DGI.Node],
    sdQueue :: Int
}

lpgAddSocket :: PGraph () -> SocketDesc -> (SocketDesc,PGraph ())
lpgAddSocket g sd = GM.runOn (do
    let boolP = ["true","false"]
    let sockL = "Socket" ++ show sockID
    let validL = sockL ++ "Valid"

    sockN <- GM.newNode $ baseFNode sockL [] [] Nothing
    validN <- GM.newNode $ baseONode validL [] boolP OpAnd Nothing
    GM.newEdge (validN, sockN, "true")

    l4C <- findN "L4Classified"

    let udpNode l cT = do
        let constrF = "!(" ++ cT ++ ")"
        let attrs = ["C.true:" ++ cT, "C.false:" ++ constrF]
        fN <- GM.newNode $ baseFNode l attrs boolP Nothing
        GM.newEdge (l4C, fN, "true")

        GM.newEdge (fN, validN, "true")
        GM.newEdge (fN, validN, "false")

        l4uvN <- findN "L4UDPVerified"
        GM.newEdge (l4uvN, validN, "true")
        GM.newEdge (l4uvN, validN, "false")
        return [fN]

    let handleFlow (UDPIPv4Listen addr port) = do
            let ipL = if addr == 0 then "*" else IP4.ipToString addr
            let filterL = "UDP/" ++ ipL ++ ":" ++ show port
            let ipC = if addr == 0 then "" else ("&DestIP=" ++ show addr)
            let constrT = "IPv4&UDP&DestPort=" ++ show port ++ ipC
            udpNode filterL constrT

        handleFlow (UDPIPv4Flow sAddr dAddr sPort dPort) = do
            let filterL = "UDP/" ++ IP4.ipToString sAddr ++ ":" ++ show sPort ++
                            "/" ++ IP4.ipToString dAddr ++ ":" ++ show dPort
            let constrT = "IPv4&UDP&SourceIP=" ++ show sAddr ++ "&SourcePort="
                            ++ show sPort ++ "&DestIP=" ++ show dAddr
                            ++ "&DestPort=" ++ show dPort
            udpNode filterL constrT

    fNodes <- handleFlow flow
    return (sd { sdNodes = fNodes ++ [sockN, validN] })
    ) g
    where
        (SocketDesc sockID flow _ _) = sd
        findN l = GM.withGr (\g' -> fst $ fromJust $
                             GH.findNodeByL ((== l) . nLabel) g')

lpgDelSocket :: PGraph () -> SocketDesc -> PGraph ()
lpgDelSocket g sd = DGI.delNodes (sdNodes sd) g


data StackState = StackState {
    ssQueues :: Int,
    ssNextSock :: SocketID,
    ssSockets :: [SocketDesc],
    ssConfig :: [(String,String)]
}

emptyStackState :: Int -> [(String,String)] -> StackState
emptyStackState queues cfg = StackState {
    ssQueues = queues,
    ssNextSock = 0,
    ssSockets = [],
    ssConfig = cfg
}

ssAddFlow :: StackState -> Flow -> (StackState,SocketID)
ssAddFlow ss flow =
    (ss { ssNextSock = sid + 1, ssSockets = ssSockets ss ++ [sd] },sid)
    where
        sid = ssNextSock ss
        sd = SocketDesc sid flow [] queue
        qInit = zip (take ((ssQueues ss) - 1) [1..]) (repeat 0 :: [Int])
        accumSock m s = M.insert q ((m M.! q) + 1) m
            where q = sdQueue s
        qMap = M.toList $ foldl accumSock (M.fromList qInit) $ ssSockets ss
        queue = fst $ L.minimumBy (\a b -> compare (snd a) (snd b)) qMap

ssDelSocket :: StackState -> SocketID -> StackState
ssDelSocket ss sock =
    ss { ssSockets = filter (\s -> sdID s /= sock) $ ssSockets ss }

ssLPG :: StackState -> PGraph ()
ssLPG ss =
    foldl (\a b -> snd $ lpgAddSocket a b) lpg $ ssSockets ss

ssPRGConfig :: StackState -> [(String,String)]
ssPRGConfig ss = 
        ssConfig ss ++ [("C5TupleFilter",config)]
    where
        config = (map sdCfg $ ssSockets ss)`UM.joinBy` "#"
        sdCfg sd = sdConfigP sd `UM.joinBy` ","
        sdConfigP (SocketDesc _ (UDPIPv4Listen ip port) _ q) =
            [ "1", show q, "IPv4", "UDP", "", if ip == 0 then "" else show ip,
              "", show port ]
        sdConfigP (SocketDesc _ (UDPIPv4Flow sAddr dAddr sPort dPort) _ q) =
            [ "0", show q, "IPv4", "UDP", show sAddr, show dAddr, show sPort,
              show dPort ]
                

ssGraphs :: StackState -> String -> IO ()
ssGraphs ss prefix = do
    putStrLn ("Generating " ++ prefix ++ " graphs...")

    let cfg = ssPRGConfig ss
    let prgC = pgSetType GTPrg $ applyConfig cfg prg
    let lpg' = pgSetType GTLpg $ ssLPG ss
    let embedded = fullEmbedding prgC lpg'

    writeFile (prefix ++ "_lpg.dot") $ toDot lpg'
    writeFile (prefix ++ "_prg.dot") $ toDot prgC
    writeFile (prefix ++ "_embedded.dot") $ toDot embedded

    putStrLn "  Constraining..."
    constrained <- constrain embedded
    putStrLn "  Constrained"
    writeFile "constrained.dot" $ toDot constrained

    return ()


main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    writeFile "prgU.dot" $ toDot prg

    let ss0 = emptyStackState 3 config
    let (ss1,s0) = ssAddFlow ss0 (UDPIPv4Listen 0 56)
    let (ss2,s1) = ssAddFlow ss1 (UDPIPv4Flow
                                (fromJust $ IP4.ipFromString "192.168.1.1")
                                (fromJust $ IP4.ipFromString "8.8.8.8") 1234 53)
    let (ss3,s2) = ssAddFlow ss2 (UDPIPv4Listen 0 54)
    let (ss4,s3) = ssAddFlow ss3 (UDPIPv4Listen 0 55)
    let (ss5,s4) = ssAddFlow ss4 (UDPIPv4Listen 0 57)
    let ss6 = ssDelSocket ss5 s3

    ssGraphs ss5 "first"

    where
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]
