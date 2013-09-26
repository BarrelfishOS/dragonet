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

import qualified Debug.Trace as T


type IPv4Address = Word32
type UDPPort = Word16

data Flow =
    UDPIPv4Listen IPv4Address UDPPort |
    UDPIPv4Flow IPv4Address IPv4Address UDPPort UDPPort

flowDest :: Flow -> (IPv4Address,UDPPort)
flowDest (UDPIPv4Listen ip port) = (ip,port)
flowDest (UDPIPv4Flow _ ip _ port) = (ip,port)

flowIsListen :: Flow -> Bool
flowIsListen (UDPIPv4Listen _ _) = True
flowIsListen _ = False

destCovers :: (IPv4Address,UDPPort) -> (IPv4Address,UDPPort) -> Bool
destCovers (0,pL) (_,pF) = pL == pF
destCovers (ipL,pL) (ipF,pF) = ipL == ipF && pL == pF

type SocketID = Int
data SocketDesc = SocketDesc {
    sdID :: SocketID,
    sdFlow :: Flow,
    sdNodes :: [DGI.Node],
    sdQueue :: Int
}

data StackState = StackState {
    ssQueues :: Int,
    ss5Tuples :: Int,
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
        port default[CFDirFilter] }

    config CFDirFilter {
        function configFDir
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


-------------------------------------------------------------------------------
-- Implementation of configuration of the flow director filters

data CFDirTuple = CFDirTuple {
    cfdtQueue    :: Int,
    cfdtL3Proto  :: C5TL3Proto,
    cfdtL4Proto  :: C5TL4Proto,
    cfdtL3Src    :: String,
    cfdtL3Dst    :: String,
    cfdtL4Src    :: UDPPort,
    cfdtL4Dst    :: UDPPort
}

cFDtString :: CFDirTuple -> String
cFDtString c = "FDir("++l3p++"/"++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l3p = show $ cfdtL3Proto c
        l4p = show $ cfdtL4Proto c
        l3s = cfdtL3Src c
        l3d = cfdtL3Dst c
        l4s = show $ cfdtL4Src c
        l4d = show $ cfdtL4Dst c

cFDtAttr :: CFDirTuple -> [String]
cFDtAttr c =
    [aTrue, aFalse]
    where
        constr :: [String]
        constr = [show $ cfdtL3Proto c, show $ cfdtL4Proto c,
                  ("SourceIP=" ++ cfdtL3Src c), ("DestIP=" ++ cfdtL3Dst c),
                  ("SourcePort=" ++ (show $ cfdtL4Src c)),
                  ("DestPort=" ++ (show $ cfdtL4Dst c)) ]
        cT = constr `UM.joinBy` "&"
        aTrue = "C.true:" ++ cT
        aFalse = "C.false:!(" ++ cT ++ ")"

parseFDirCFG :: String -> [CFDirTuple]
parseFDirCFG [] = []
parseFDirCFG s = map parseTuple $ UM.splitBy '#' s
    where
        parseTuple s' = CFDirTuple {
            cfdtQueue   = read (parts !! 0),
            cfdtL3Proto = if (parts !! 1) == "IPv4" then C5TPL3IPv4 else
                           if (parts !! 1) == "IPv6" then C5TPL3IPv6 else
                               error "Invalid L3 protocol",
            cfdtL4Proto = if (parts !! 2) == "TCP" then C5TPL4TCP else
                           if (parts !! 2) == "UDP" then C5TPL4UDP else
                               error "Invalid L4 protocol",
            cfdtL3Src   = parts !! 3,
            cfdtL3Dst   = parts !! 4,
            cfdtL4Src   = read (parts !! 5),
            cfdtL4Dst   = read (parts !! 6) }
            where
            parts = UM.splitBy ',' s'

configFDir :: ConfFunction
configFDir _ inE outE cfg = do
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

        -- Get filter configurations
        cfgs = parseFDirCFG cfg

        -- Generate node and edges for one filter
        bports = ["true","false"]
        nodeL c = baseFNode (cFDtString c) (cFDtAttr c) bports Nothing
        addFilter ((iN,iE),es) c = do
            (n,_) <- confMNewNode $ nodeL c
            let inEdge = (iN,n,iE)
            let tEdge = (n,queue $ cfdtQueue c,"true")
            let fEdge = (n,queue $ cfdtQueue c,"false")
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


emptyStackState :: Int -> Int -> [(String,String)] -> StackState
emptyStackState queues n5tuples cfg = StackState {
    ssQueues = queues,
    ss5Tuples = n5tuples,
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


-- Generate a 5-tuple filter configuration for the specified socket list
generate5TConfig :: StackState -> [SocketDesc] -> String
generate5TConfig ss sockets =
    map genTuple sockets `UM.joinBy` "#"
    where
        genTuple sd = genParts sd `UM.joinBy` ","
        genParts (SocketDesc _ (UDPIPv4Listen 0 port) _ q) =
            [ "2", show q, "IPv4", "UDP", "", "", "", show port ]
        genParts (SocketDesc _ (UDPIPv4Listen ip port) _ q) =
            [ "1", show q, "IPv4", "UDP", "", show ip, "", show port ]
        genParts (SocketDesc _ (UDPIPv4Flow sAddr dAddr sPort dPort) _ q) =
            [ "0", show q, "IPv4", "UDP", show sAddr, show dAddr, show sPort,
              show dPort ]

-- Generate a FlowDir filter configuration for the specified socket list
generateFDirConfig :: StackState -> [SocketDesc] -> String
generateFDirConfig ss sockets =
    map genTuple sockets `UM.joinBy` "#"
    where
        genTuple sd = genParts sd `UM.joinBy` ","
        genParts (SocketDesc _ (UDPIPv4Listen _ _) _ _) =
            error "Listening sockets not supported in FlowDir filters"
        genParts (SocketDesc _ (UDPIPv4Flow sAddr dAddr sPort dPort) _ q) =
            [ show q, "IPv4", "UDP", show sAddr, show dAddr, show sPort,
              show dPort ]


-- Generate configuration for PRG
ssPRGConfig :: StackState -> [(String,String)]
ssPRGConfig ss = 
        ssConfig ss ++ [("C5TupleFilter",config5T),("CFDirFilter",configFD)]
    where
        -- Divide sockets up into listening and flows
        (listens,flows) = L.partition (flowIsListen . sdFlow) $ ssSockets ss
        -- Conflicts: Flows with same destination as listening sockets
        rawConflicts = map (\l -> (l,badS $ flowDest $ sdFlow l)) listens
            where
                badS l = filter ((l `destCovers`) . flowDest . sdFlow) flows

        cleanListens = map fst $ filter (null . snd) rawConflicts
        --conflicts = filter (not . null . snd) rawConflicts

        -- Destinations for all listen sockets
        listenDests = L.nub $ map (flowDest . sdFlow) listens

        -- Mixed flows are flows that have the same destination as a listening
        -- socket. These flows can't just be moved to FDir filters
        isMixedFlow f = any (`destCovers` (flowDest $ sdFlow f)) listenDests
        (mixedFlows,cleanFlows) = L.partition isMixedFlow flows

        -- Assign flows to filters
        (flows5T,flowsFD) =
            if (length listens) + (length mixedFlows) <= ss5Tuples ss then
                -- No problem, can use 5-tuples for listens and mixed flows
                (listens ++ mixedFlows, cleanFlows)
            else if (length cleanListens) > ss5Tuples ss then
                T.trace ("More listens than 5-tuples, handling arbitrary " ++
                         "listens in software")
                        (take (ss5Tuples ss) cleanListens, cleanFlows)
            else
                T.trace ("Too many mixed flows and listens, dropping all " ++
                         "mixed flows and listens in software")
                        (cleanListens, cleanFlows)

        config5T = generate5TConfig ss flows5T
        configFD = generateFDirConfig ss flowsFD



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

cgmRun :: Int -> Int -> [(String,String)] -> ConfigGenM () -> IO ()
cgmRun queues n5tuples config c = do
    let ss = emptyStackState queues n5tuples config
    TS.evalStateT c ss



main :: IO ()
main = do
    let cgm = do
        cgmGraphs "0"
        s0 <- cgmNewSocket $ UDPIPv4Listen 0 56
        cgmGraphs "1"
        s1 <- cgmNewSocket $ UDPIPv4Flow
                                (fromJust $ IP4.ipFromString "192.168.1.1")
                                (fromJust $ IP4.ipFromString "8.8.8.8") 1234 54
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
    let queues = 3
        n5tuples = 3
    cgmRun queues n5tuples config cgm

    where
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]
