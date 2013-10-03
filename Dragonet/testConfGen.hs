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
import Control.Arrow
import System.Random as R
import qualified Data.Text.Lazy as T
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA

import qualified Debug.Trace as T


type IPv4Address = Word32
type UDPPort = Word16

data Flow =
    UDPIPv4Listen IPv4Address UDPPort |
    UDPIPv4Flow IPv4Address IPv4Address UDPPort UDPPort
    deriving Eq

flowDest :: Flow -> (IPv4Address,UDPPort)
flowDest (UDPIPv4Listen ip port) = (ip,port)
flowDest (UDPIPv4Flow _ ip _ port) = (ip,port)

flowIsListen :: Flow -> Bool
flowIsListen (UDPIPv4Listen _ _) = True
flowIsListen _ = False

flowCovers :: Flow -> Flow -> Bool
flowCovers (UDPIPv4Listen ipL pL) (UDPIPv4Listen ipF pF) =
    (ipL,pL) `destCovers` (ipF,pF)
flowCovers (UDPIPv4Listen ipL pL) (UDPIPv4Flow _ ipF _ pF) =
    (ipL,pL) `destCovers` (ipF,pF)
flowCovers _ _ = False


destCovers :: (IPv4Address,UDPPort) -> (IPv4Address,UDPPort) -> Bool
destCovers (0,pL) (_,pF) = pL == pF
destCovers (ipL,pL) (ipF,pF) = ipL == ipF && pL == pF

type SocketID = Int
data SocketDesc = SocketDesc {
    sdID :: SocketID,
    sdFlow :: Flow,
    sdNodes :: [DGI.Node],
    sdQueue :: Int
} deriving Eq

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
            port true false[]
            constraint true "IPv4"
            constraint false "!IPv4" }
    
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
            port true false[Verified .L3Classified] }

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
        port true false[] }
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
        showIP s = IP4.ipToString $ read s
        l3s = maybe "*" showIP $ c5tL3Src c
        l3d = maybe "*" showIP $ c5tL3Dst c
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
        l3s = IP4.ipToString $ read $ cfdtL3Src c
        l3d = IP4.ipToString $ read $ cfdtL3Dst c
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

-- Generate LPG corresponding to the specified LPG
ssLPG :: StackState -> PGraph ()
ssLPG ss = snd $ GM.runOn (do
    -- Create socket and validS-nodes
    vns <- mapM (\sd -> do
            validN <- GM.newNode $ baseONode (vLbl sd) [] boolP OpAnd Nothing
            sockN <- GM.newNode $ baseFNode (sLbl sd) ["sink"] [] Nothing
            GM.newEdge (validN, sockN, "true")
            return (sd,validN)) $ ssSockets ss

    -- Add dependencies between validS nodes depending on how many fields are
    -- to be matched.
    let addDeps (sd,vN) = do
        let dst = sdFlow sd
        -- Look for flows that are covered by our destination
        let isCovered (sd',_) = sd /= sd' && (dst `flowCovers` sdFlow sd')
        let covered = filter isCovered vns
        if not $ null covered then do
            nor <- GM.newNode $ baseONode (exLbl sd) [] boolP OpNOr Nothing
            newDblEdge nor vN
            mapM_ (\(_,n) -> newDblEdge n nor) covered
        else
            return ()
    mapM_ addDeps vns

    -- Nodes for matching source and dest IP
    let mIPNode dir ip = do
        let lbl = "IPv4" ++ dir ++ IP4.ipToString ip
            cAttr = boolCA ("IPv4&" ++ dir ++ "IP=" ++ show ip)
            attr = cAttr ++ ["decision"]
        node <- GM.newNode $ baseFNode lbl attr boolP Nothing
        cN <- findN "L3IPv4Classified"
        GM.newEdge (cN, node, "true")
        return (ip,node)
    sIPs <- mapM (mIPNode "Source") $ L.nub $ catMaybes $ map fSrcIP flows
    dIPs <- mapM (mIPNode "Dest") $ L.nub $ catMaybes $ map fDstIP flows

    -- Nodes for matching source and dest UDP
    let mUDPNode dir p = do
        let lbl = "UDP" ++ dir ++ show p
            cAttr = boolCA ("UDP&" ++ dir ++ "Port=" ++ show p)
            attr = cAttr ++ ["decision"]
        node <- GM.newNode $ baseFNode lbl attr boolP Nothing
        cN <- findN "L4UDPClassified"
        GM.newEdge (cN, node, "true")
        return (p,node)
    sUPs <- mapM (mUDPNode "Source") $ L.nub $ catMaybes $ map fUDPSrcP flows
    dUPs <- mapM (mUDPNode "Dest") $ L.nub $ catMaybes $ map fUDPDstP flows



    -- Add edges from the ip/udp port nodes to the socket valid nodes
    l4uv <- findN "L4UDPVerified"
    l3ip4v <- findN "L3IPv4Verified"
    let addEdges (UDPIPv4Listen 0 p,vN) = do
            let (Just pN) = lookup p dUPs
            newDblEdge l3ip4v vN
            newDblEdge l4uv vN
            newDblEdge pN vN
        addEdges (UDPIPv4Listen ip p,vN) = do
            let (Just ipN) = lookup ip dIPs
            let (Just pN) = lookup p dUPs
            newDblEdge l3ip4v vN
            newDblEdge l4uv vN
            newDblEdge pN vN
            newDblEdge ipN vN
        addEdges (UDPIPv4Flow sIP dIP sP dP,vN) = do
            let (Just sIPn) = lookup sIP sIPs
            let (Just dIPn) = lookup dIP dIPs
            let (Just sPn) = lookup sP sUPs
            let (Just dPn) = lookup dP dUPs
            newDblEdge l3ip4v vN
            newDblEdge l4uv vN
            newDblEdge sIPn vN
            newDblEdge dIPn vN
            newDblEdge sPn vN
            newDblEdge dPn vN
    mapM_ addEdges $ map (first sdFlow) vns

    return ()) lpg
    where
        boolP = ["true","false"]
        boolCA cT = ["C.true:" ++ cT, "C.false:!(" ++ cT ++ ")"]
        sLbl sd = "Socket" ++ (show $ sdID sd)
        vLbl sd = "ValidS" ++ (show $ sdID sd)
        exLbl sd = "ExcludeS" ++ (show $ sdID sd)

        flows = map sdFlow $ ssSockets ss
        fSrcIP (UDPIPv4Flow ip _ _ _) = Just ip
        fSrcIP _ = Nothing
        fDstIP (UDPIPv4Listen 0 _) = Nothing
        fDstIP (UDPIPv4Listen ip _) = Just ip
        fDstIP (UDPIPv4Flow _ ip _ _) = Just ip
        fUDPSrcP (UDPIPv4Flow _ _ p _) = Just p
        fUDPSrcP _ = Nothing
        fUDPDstP (UDPIPv4Listen _ p) = Just p
        fUDPDstP (UDPIPv4Flow _ _ _ p) = Just p

        findN l = GM.withGr (\g' ->
                    let Just (n,_) = GH.findNodeByL ((== l) . nLabel) g' in n)

        newDblEdge n1 n2 =
            do { GM.newEdge (n1,n2,"true") ; GM.newEdge (n1,n2,"false") }



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
        rawConflicts = map (\l -> (l,badS $ sdFlow l)) listens
            where
                badS l = filter ((l `flowCovers`) . sdFlow) flows

        (cleanListens,mixedListens) = join (***) (map fst) $
            L.partition (null . snd) rawConflicts
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
                        (take (ss5Tuples ss) (cleanListens ++ mixedListens),
                         cleanFlows)

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

ssGraphs :: StackState -> String -> String -> IO ()
ssGraphs ss prefix title = do
    putStrLn ("Generating " ++ prefix ++ " graphs...")

    let cfg = ssPRGConfig ss
    let prgC = pgSetType GTPrg $ applyConfig cfg prg
    let lpg' = pgSetType GTLpg $ ssLPG ss
    let prgR = dropUnreachable prgC
    let embedded = fullEmbedding prgR lpg'

    writeFile (prefix ++ "_lpg.dot") $ myToDot lpg'
    writeFile (prefix ++ "_prg.dot") $ myToDot prgC
    writeFile (prefix ++ "_embedded.dot") $ myToDot embedded

    putStrLn "  Constraining..."
    constrained <- constrain embedded
    putStrLn "  Constrained"
    writeFile (prefix ++ "_constrained.dot") $ myToDot constrained
    writeFile (prefix ++ "_final.dot") $ myToDot $ dropNonsinks $ constrained

    return ()
    where
        dropUnreachable g =
            foldl dropZeroIndeg g $ GH.topsortLN g
        dropZeroIndeg g (n,l) =
            if null (DGI.pre g n) && (notElem "source" $ nAttributes l) then
                DGI.delNode n g
            else g
        myToDot g = toDotWith' addTitle g
        addTitle p = p { GV.globalAttributes = GV.globalAttributes p ++
                        [GV.GraphAttrs [GA.Label $ GA.StrLabel $ T.pack title]] }

------------------------------------------------------------------------------
-- Config generation monad to simplify things

type ConfigGenM a = TS.StateT StackState IO a

cgmIO :: IO a -> ConfigGenM a
cgmIO = MTIO.liftIO

cgmStackState :: ConfigGenM StackState
cgmStackState = get

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
cgmGraphs pref = cgmGraphs' pref ""

cgmGraphs' :: String -> String -> ConfigGenM ()
cgmGraphs' pref title = do
    ss <- get
    cgmIO $ ssGraphs ss pref title

cgmRun :: Int -> Int -> [(String,String)] -> ConfigGenM () -> IO ()
cgmRun queues n5tuples config c = do
    let ss = emptyStackState queues n5tuples config
    TS.evalStateT c ss


rndStep :: ConfigGenM String
rndStep = do
    ss <- cgmStackState
    let socks = ssSockets ss
    let dests = map flowDest $ filter flowIsListen $ map sdFlow socks

    range <- rndR (0 :: Int,2)
    case range of
        -- New listening socket
        0 -> do
            hasIP <- rnd
            dIP <- if hasIP then rndIP else return 0
            dPort <- rndPort
            s <- cgmNewSocket $ UDPIPv4Listen dIP dPort
            return ("New listening socket (" ++ IP4.ipToString dIP ++ "," ++
                        show dPort ++ ") = " ++ show s)

        -- New flow
        1 -> do
            exDest <- rnd
            (dIP,dPort) <-
                if exDest && (not $ null dests) then rndPick dests
                else rndEP
            (sIP,sPort) <- rndEP
            s <- cgmNewSocket $ UDPIPv4Flow sIP dIP sPort dPort
            return ("New flow (" ++ IP4.ipToString sIP ++ "," ++
                        IP4.ipToString dIP ++ "," ++ show sPort ++ "," ++
                        show dPort ++ ") = " ++ show s)

        -- Close socket
        _ -> do
            if null socks then rndStep
            else do
                sock <- rndPick socks
                cgmDelSocket $ sdID sock
                return ("Closing socket " ++ (show $ sdID sock))

    where
        rnd :: Random a => ConfigGenM a
        rnd = cgmIO R.randomIO

        rndR :: Random a => (a,a) -> ConfigGenM a
        rndR a = cgmIO $ R.randomRIO a

        rndPick :: [a] -> ConfigGenM a
        rndPick l = do { i <- rndR (0, (length l) - 1); return (l !! i) }

        rndIP :: ConfigGenM Word32
        rndIP = rnd

        rndPort :: ConfigGenM Word16
        rndPort = rndR (1,1024)

        rndEP :: ConfigGenM (Word32,Word16)
        rndEP = do
            ip <- rndIP
            port <- rndPort
            return (ip,port)


rndScenario :: Int -> ConfigGenM ()
rndScenario steps = do
    cgmGraphs $ toLbl 0
    mapM_ (\i -> do { s <- rndStep ; cgmGraphs' (toLbl i) s }) [1..steps]
    where
        toLbl :: Int -> String
        toLbl i
            | steps < 10 = show i
            | i < 10     = "0" ++ show i
            | otherwise  =  show i


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
        return ()

    putStrLn "Generating .dot files..."
    writeFile "prgU.dot" $ toDot prg
    let queues = 3
        n5tuples = 3
    let rndS = rndScenario 10
    cgmRun queues n5tuples config rndS

    where
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]

