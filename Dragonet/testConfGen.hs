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

import E10k


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
    ssConfig :: [(String,ConfValue)]
}





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





--------------------------------------------------------------------------------
-- Adapting LPG

-- Generate LPG corresponding to the specified LPG
ssLPG :: StackState -> PGraph
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


emptyStackState :: Int -> Int -> [(String,ConfValue)] -> StackState
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
generate5TConfig :: StackState -> [SocketDesc] -> ConfValue
generate5TConfig ss sockets =
    CVList $ map genTuple sockets
    where
        genTuple (SocketDesc _ (UDPIPv4Listen 0 port) _ q) =
            CVTuple [CVMaybe Nothing,
                     CVMaybe Nothing,
                     CVMaybe $ Just $ CVEnum 1,
                     CVMaybe Nothing,
                     CVMaybe $ Just $ CVInt $ fromIntegral port,
                     CVInt 3,
                     CVInt $ fromIntegral q]
        genTuple (SocketDesc _ (UDPIPv4Listen ip port) _ q) =
            CVTuple [CVMaybe Nothing,
                     CVMaybe $ Just $ CVInt $ fromIntegral ip,
                     CVMaybe $ Just $ CVEnum 1,
                     CVMaybe Nothing,
                     CVMaybe $ Just $ CVInt $ fromIntegral port,
                     CVInt 2,
                     CVInt $ fromIntegral q]
        genTuple (SocketDesc _ (UDPIPv4Flow sAddr dAddr sPort dPort) _ q) =
            CVTuple [CVMaybe $ Just $ CVInt $ fromIntegral sAddr,
                     CVMaybe $ Just $ CVInt $ fromIntegral dAddr,
                     CVMaybe $ Just $ CVEnum 1,
                     CVMaybe $ Just $ CVInt $ fromIntegral sPort,
                     CVMaybe $ Just $ CVInt $ fromIntegral dPort,
                     CVInt 1,
                     CVInt $ fromIntegral q]

-- Generate a FlowDir filter configuration for the specified socket list
generateFDirConfig :: StackState -> [SocketDesc] -> ConfValue
generateFDirConfig ss sockets =
    CVList $ map genTuple sockets
    where
        genTuple (SocketDesc _ (UDPIPv4Listen _ _) _ _) =
            error "Listening sockets not supported in FlowDir filters"
        genTuple (SocketDesc _ (UDPIPv4Flow sAddr dAddr sPort dPort) _ q) =
            CVTuple [
                CVInt $ fromIntegral sAddr,
                CVInt $ fromIntegral dAddr,
                CVEnum 1,
                CVInt $ fromIntegral sPort,
                CVInt $ fromIntegral dPort,
                CVInt $ fromIntegral q]


-- Generate configuration for PRG
ssPRGConfig :: StackState -> [(String,ConfValue)]
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
dropNonsinks :: PGraph -> PGraph
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

cgmRun :: Int -> Int -> [(String,ConfValue)] -> ConfigGenM () -> IO ()
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
        config = []

