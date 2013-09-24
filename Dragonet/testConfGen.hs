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
import Control.Monad.Trans.State as TS
import Control.Monad.IO.Class as MTIO


type IPv4Address = Word32
type UDPPort = Word16

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

data StackState = StackState {
    ssQueues :: Int,
    ssNextSock :: SocketID,
    ssSockets :: [SocketDesc],
    ssConfig :: [(String,String)]
}





[unicorn|
graph prg {
    node HWDrop { }

    cluster L2Ether {
        boolean Classified {
            attr "source"
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
        attr "sink"
        port out[] }

    or Q1Valid {
        port true[Queue1]
        port false[] }
    node Queue1 {
        attr "software"
        attr "sink"
        port out[] }

    or Q2Valid {
        port true[Queue2]
        port false[] }
    node Queue2 {
        attr "software"
        attr "sink"
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
            port false[]
            constraint true "IPv4"
            constraint false "!IPv4" }

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
            port false[]
            constraint true "IPv6"
            constraint false "!IPv6" }

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


-------------------------------------------------------------------------------
-- Implementation of configuration of 5-tuple filters

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
parse5tCFG [] = []
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
            





--------------------------------------------------------------------------------
-- Adapting LPG

-- Adds a new socket to the specified LPG
lpgAddSocket :: PGraph () -> SocketDesc -> (SocketDesc,PGraph ())
lpgAddSocket g sd = GM.runOn (do
    let boolP = ["true","false"]
    let sockL = "Socket" ++ show sockID
    let validL = sockL ++ "Valid"

    sockN <- GM.newNode $ baseFNode sockL ["sink"] [] Nothing
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

-- Remove a socket from the LPG
lpgDelSocket :: PGraph () -> SocketDesc -> PGraph ()
lpgDelSocket g sd = DGI.delNodes (sdNodes sd) g


--------------------------------------------------------------------------------
-- State of the whole network stack


emptyStackState :: Int -> [(String,String)] -> StackState
emptyStackState queues cfg = StackState {
    ssQueues = queues,
    ssNextSock = 0,
    ssSockets = [],
    ssConfig = cfg
}

-- Add a new socket
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

-- Remove a socket
ssDelSocket :: StackState -> SocketID -> StackState
ssDelSocket ss sock =
    ss { ssSockets = filter (\s -> sdID s /= sock) $ ssSockets ss }

-- Generate LPG corresponding to the specified LPG
ssLPG :: StackState -> PGraph ()
ssLPG ss =
    foldl (\a b -> snd $ lpgAddSocket a b) lpg $ ssSockets ss

-- Generate configuration for PRG
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
                

-- Iteratively drop all sink nodes that don't have the sink attribute
dropNonsinks :: PGraph () -> PGraph ()
dropNonsinks graph =
        foldl dropNS graph ts
    where
        ts = reverse $ GH.topsortLN graph
        dropNS g (n,l) =
            if (null $ DGI.suc g n) && (notElem "sink" $ nAttributes l) then
                DGI.delNode n g
            else g

ssGraphs :: StackState -> String -> IO ()
ssGraphs ss prefix = do
    putStrLn ("Generating " ++ prefix ++ " graphs...")

    let cfg = ssPRGConfig ss
    let prgC = pgSetType GTPrg $ applyConfig cfg prg
    let lpg' = pgSetType GTLpg $ ssLPG ss
    let prgR = dropUnreachable prgC
    let embedded = fullEmbedding prgR lpg'

    writeFile (prefix ++ "_lpg.dot") $ toDot lpg'
    writeFile (prefix ++ "_prg.dot") $ toDot prgC
    writeFile (prefix ++ "_embedded.dot") $ toDot embedded

    putStrLn "  Constraining..."
    constrained <- constrain embedded
    putStrLn "  Constrained"
    writeFile (prefix ++ "_constrained.dot") $ toDot constrained
    writeFile (prefix ++ "_final.dot") $ toDot $ dropNonsinks constrained

    return ()
    where
        dropUnreachable g =
            foldl dropZeroIndeg g $ GH.topsortLN g
        dropZeroIndeg g (n,l) =
            if null (DGI.pre g n) && (notElem "source" $ nAttributes l) then
                DGI.delNode n g
            else g

------------------------------------------------------------------------------
-- Config generation monad to simplify things

type ConfigGenM a = TS.StateT StackState IO a

cgmIO :: IO a -> ConfigGenM a
cgmIO = MTIO.liftIO

cgmNewSocket :: Flow -> ConfigGenM SocketID
cgmNewSocket f = do
    ss <- get
    let (ss',sock) = ssAddFlow ss f
    put ss'
    return sock

cgmDelSocket :: SocketID -> ConfigGenM ()
cgmDelSocket sock = do
    ss <- get
    put $ ssDelSocket ss sock

cgmGraphs :: String -> ConfigGenM ()
cgmGraphs pref = do
    ss <- get
    cgmIO $ ssGraphs ss pref

cgmRun :: Int -> [(String,String)] -> ConfigGenM () -> IO ()
cgmRun queues config c = do
    let ss = emptyStackState queues config
    TS.evalStateT c ss



main :: IO ()
main = do
    let cgm = do
        cgmGraphs "0"
        s0 <- cgmNewSocket $ UDPIPv4Listen 0 56
        cgmGraphs "1"
        s1 <- cgmNewSocket $ UDPIPv4Flow
                                (fromJust $ IP4.ipFromString "192.168.1.1")
                                (fromJust $ IP4.ipFromString "8.8.8.8") 1234 53
        cgmGraphs "2"
        s2 <- cgmNewSocket $ UDPIPv4Listen 0 54
        cgmGraphs "3"
        cgmDelSocket s0
        cgmGraphs "4"
        s3 <- cgmNewSocket $ UDPIPv4Listen 0 55
        cgmGraphs "5"
        s4 <- cgmNewSocket $ UDPIPv4Listen 0 57
        cgmGraphs "6"

    putStrLn "Generating .dot files..."
    writeFile "prgU.dot" $ toDot prg
    cgmRun 3 config cgm

    where
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]
