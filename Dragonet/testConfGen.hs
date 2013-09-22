#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.DotGenerator
import Dragonet.Embedding
import Dragonet.Constraints

import Data.Word
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Graph.Inductive as DGI
import qualified Util.Misc as UM
import qualified Util.GraphHelpers as GH
import qualified Util.GraphMonad as GM
import Control.Monad
import qualified Debug.Trace as T

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
        port queues[Queue0 Queue1 Queue2]
        port default[Queue0] }

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

c5tString c = "5T("++l3p++"/"++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l3p = fromMaybe "*" $ liftM show $ c5tL3Proto c
        l4p = fromMaybe "*" $ liftM show $ c5tL4Proto c
        l3s = fromMaybe "*" $ c5tL3Src c
        l3d = fromMaybe "*" $ c5tL3Dst c
        l4s = fromMaybe "*" $ liftM show $ c5tL4Src c
        l4d = fromMaybe "*" $ liftM show $ c5tL4Dst c

c5tAttr c =
    if null constr then [] else T.trace ("Constr:" ++ cT) [aTrue, aFalse]
    where
        constr = catMaybes $ [
                {-do {p <- c5tL3Proto c; return (show p)},-}
                do {p <- c5tL4Proto c; return (show p)},
                {-do {s <- c5tL3Src c; return ("SourceIP=" ++ s)},
                do {d <- c5tL3Dst c; return ("DestIP=" ++ d)},-}
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
config5tuple node inE outE cfg = do
    ((endN,endP),edges) <- foldM addFilter (start,[]) cfgs
    let lastEdge = (endN,defaultN,endP)
    return (edges ++ [lastEdge])
    where
        -- Node and Port for the incoming edge for the first node
        start = (fst $ fst $ head inE, snd $ head inE)
        -- Node for default queue
        defaultN = fst $ fst $ fromJust $ L.find ((== "default") . snd) outE
        -- Lookup node id for specified queue
        queue i = fst $ fst $ fromJust $ L.find
                    (\((_,n),_) -> nLabel n == "Queue" ++ show i) outE
        
        -- Get filter configurations ordered by priority
        cmpPrio a b = compare (c5tPriority a) (c5tPriority b)
        cfgs = L.sortBy cmpPrio $ parse5tCFG cfg

        -- Generate node and edges for one filter
        bports = ["true","false"]
        nodeL c = baseFNode (c5tString c) (c5tAttr c) bports Nothing
        addFilter ((iN,iE),es) c = do
            (n,l) <- confMNewNode $ nodeL c
            let inEdge = (iN,n,iE)
            let tEdge = (n,queue $ c5tQueue c,"true")
            return ((n,"false"), es ++ [inEdge,tEdge])
            



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
    let handleFlow (UDPIPv4Listen addr port) = do
        let filterL = "UDP/*:" ++ show port
        let constrT = "UDP&DestPort=" ++ show port
        let constrF = "!(" ++ constrT ++ ")"
        let attrs = ["C.true:" ++ constrT, "C.false:" ++ constrF]
        fN <- GM.newNode $ baseFNode filterL attrs boolP Nothing
        GM.newEdge (l4C, fN, "true")

        GM.newEdge (fN, validN, "true")
        GM.newEdge (fN, validN, "false")

        l4uvN <- findN "L4UDPVerified"
        GM.newEdge (l4uvN, validN, "true")
        GM.newEdge (l4uvN, validN, "false")

        return [fN]

    fNodes <- handleFlow flow
    return (sd { sdNodes = fNodes ++ [sockN, validN] })
    ) g
    where
        (SocketDesc sockID flow _ _) = sd
        findN l = GM.withGr (\g -> fst $ fromJust $
                             GH.findNodeByL ((== l) . nLabel) g)

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

ssAddFlow ss flow =
    ss { ssNextSock = sid + 1, ssSockets = ssSockets ss ++ [sd] }
    where
        sid = ssNextSock ss
        sd = SocketDesc sid flow [] queue
        qInit = zip (take (ssQueues ss) [1..]) (repeat 0)
        accumSock m s = M.insert q ((m M.! q) + 1) m
            where q = sdQueue s
        qMap = M.toList $ foldl accumSock (M.fromList qInit) $ ssSockets ss
        queue = fst $ L.minimumBy (\a b -> compare (snd a) (snd b)) qMap
        
ssDelSocket ss sock =
    ss { ssSockets = filter (\s -> sdID s /= sock) $ ssSockets ss }


ssLPG ss =
    foldl (\a b -> snd $ lpgAddSocket a b) lpg $ ssSockets ss

joinBy l sep = L.foldl1 (\a b -> a ++ sep ++ b) l

ssPRGConfig ss = 
        ssConfig ss ++ [("C5TupleFilter",config)]
    where
        config = (map sdConfig $ ssSockets ss)`joinBy` "#"
        sdConfig (SocketDesc _ (UDPIPv4Listen ip port) _ q) =
            parts `joinBy` ","
            where parts = [ "0", show q, "IPv4", "UDP", "", show ip, "",
                            show port ]
                

main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    {-writeFile "lpg.dot" $ toDotClustered lpgT lpgClusters-}
    writeFile "prgU.dot" $ toDot prg
    let ss = emptyStackState 3 config
    let ss' = ssAddFlow ss (UDPIPv4Listen 0 56)
    let ss'' = ssAddFlow ss' (UDPIPv4Listen 0 53)
    let ss''' = ssDelSocket ss'' 0
    let cfg' = ssPRGConfig ss'''
    let prgC = pgSetType GTPrg $ applyConfig cfg' prg
    let lpg = pgSetType GTLpg $ ssLPG ss'''
    let embedded = fullEmbedding prgC lpg
    putStrLn "constraining"
    constrained <- constrain embedded
    putStrLn "constrained"
    writeFile "prg.dot" $ toDot prgC
    writeFile "lpg.dot" $ toDot lpg
    writeFile "embedded.dot" $ toDot embedded
    writeFile "constrained.dot" $ toDot constrained
    {-let (s1,lpg') = lpgAddSocket lpgT (SocketDesc 1 (UDPIPv4Listen 0 56) [] 0)
    let (s2,lpg'') = lpgAddSocket lpg' (SocketDesc 2 (UDPIPv4Listen 0 53) [] 0)
    let lpg''' = lpgDelSocket lpg'' s1-}
    {-writeFile "prg.dot" $ toDot prg
    writeFile "prg_conf.dot" $ toDot prgTConf
    writeFile "embedded.dot" $ toDot $ embedded
    constrained <- constrain embedded
    writeFile "constrained.dot" $ toDot $ constrained-}
    where
        lpgT = pgSetType GTLpg lpg
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]
        --          ("C5TupleFilter", "0,1,IPv4,TCP,,,80,#0,2,,UDP,,,53,")]

        --embedded = fullEmbedding prgTConf lpgT
