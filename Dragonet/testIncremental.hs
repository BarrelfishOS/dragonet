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
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Graph.Inductive as DGI
import qualified Util.Misc as UM
import qualified Util.GraphHelpers as GH
import qualified Util.GraphMonad as GM
import Control.Monad
import qualified Control.Monad.Trans as MT
import qualified Control.Monad.State as MS
import Control.Monad.State.Class
import qualified Control.Monad.Random as MR
import qualified Control.Monad.Writer as MW
import Control.Arrow
import qualified System.Random as R
import qualified Data.Text.Lazy as T
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA

import qualified Debug.Trace as T

import E10k


type IPv4Address = Word32
type UDPPort = Word16
type QueueID = Int

data Flow =
    UDPIPv4Listen IPv4Address UDPPort |
    UDPIPv4Flow IPv4Address IPv4Address UDPPort UDPPort
    deriving (Eq,Show)

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
    sdQueue :: QueueID
} deriving (Eq,Show)




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




------------------------------------------------------------------------------
-- Policies


data PolicyAction a =
    PActLPGAddSocket SocketDesc QueueID |
    PActLPGDelSocket SocketID QueueID |
    PActHWAction a
    deriving (Show)

data PolicyState a b = PolicyState {
    psSockets :: M.Map SocketID SocketDesc,
    psFlows  :: M.Map SocketID QueueID,
    psQueues :: M.Map QueueID Int,
    psPRGSt  :: a,
    psEvents :: [PolicyAction b]
} deriving (Show)

policyStateInit :: Int -> a -> PolicyState a b
policyStateInit nQ p = PolicyState {
    psSockets = M.empty,
    psFlows = M.empty,
    psQueues = M.fromList $ zip ids $ repeat 0,
    psPRGSt = p,
    psEvents = [] }
    where ids = [0..(nQ - 1)]


type PolicyM a b = MS.State (PolicyState a b)

getPRG :: PolicyM a b a
getPRG = do
    s <- get
    return $ psPRGSt s

putPRG :: a -> PolicyM a b ()
putPRG p = do
    ps <- get
    put (ps { psPRGSt = p })

pSockets :: PolicyM a b [SocketDesc]
pSockets = do
    s <- get
    return $ M.elems $ psSockets s

conflictingListens :: SocketDesc -> PolicyM a b [SocketDesc]
conflictingListens sd = do
    ss <- pSockets
    let f = sdFlow sd
    return $ filter ((`flowCovers` f) . sdFlow) ss

coveredSockets :: SocketDesc -> PolicyM a b [SocketDesc]
coveredSockets sd = do
    ss <- pSockets
    let f = sdFlow sd
    return $ filter ((flowCovers f) . sdFlow) ss

findMatchingQueue :: SocketDesc -> PolicyM a b QueueID
findMatchingQueue sd = do
    ss <- conflictingListens sd
    if null ss then
        return 0
    else do
        let sid = sdID $ L.minimumBy cmp ss
        s <- get
        return $ (psFlows s) M.! sid
    where
        cmp a b = if af `flowCovers` bf then GT else LT
            where (af,bf) = join (***) sdFlow (a,b)

addEvent :: PolicyAction a -> PolicyM b a ()
addEvent e = do
    s <- get
    put $ s { psEvents = psEvents s ++ [e] }


isConnection :: SocketDesc -> Bool
isConnection sd = not $ flowIsListen $ sdFlow sd

policyAddSocket :: (SocketDesc -> QueueID -> PolicyM a b QueueID) -> Flow -> PolicyM a b SocketID
policyAddSocket prgF f = do
    s <- get
    let sm = psSockets s
    let sid = if M.null sm then 0 else ((fst $ M.findMax sm) + 1)
    let sd = SocketDesc { sdID = sid, sdFlow = f, sdNodes = [], sdQueue = -1 }
    let evs = psEvents s
    let (qmin,_) = L.minimumBy (compare `on` snd) $ M.toList $ psQueues s
    q <- prgF sd qmin
    addEvent $ PActLPGAddSocket sd q
    s' <- get
    put $ s' {
        psSockets = M.insert sid sd $ psSockets s',
        psFlows = M.insert sid q $ psFlows s',
        psQueues = M.adjust (+1) q $ psQueues s' }
    return sid

policyRemoveSocket :: (SocketDesc -> PolicyM a b ()) -> (QueueID -> QueueID -> PolicyM a b Bool) -> SocketID -> PolicyM a b ()
policyRemoveSocket prgF prgR sid = do
    s <- get
    let sd = (psSockets s) M.! sid
    prgF sd
    let qid = (psFlows s) M.! sid
    addEvent $ PActLPGDelSocket sid qid
    s' <- get
    put $ s' {
        psSockets = M.delete sid $ psSockets s',
        psFlows = M.delete sid $ psFlows s',
        psQueues = M.adjust ((-) 1) qid $ psQueues s' }
    let nCur = (psQueues s') M.! qid
    let (qM,nM) = L.maximumBy (compare `on` snd) $ M.toList $ psQueues s'
    if (nM - nCur) > 1 then do
        _ <- prgR qM qid
        return ()
    else
        return ()



{-policyStep :: PolicyM a b c -> PolicyState a b -> (c, PolicyState a b,[PolicyAction b])
policyStep f ps = (a, ps' { psEvents = [] }, psEvents ps')
    where (a,ps') = runState f ps -}


------------------------------------------------------------------------------
-- Example Policy

type FTupleID = Int
data E10kPAction =
    E10kPAct5TSet FTupleID C5Tuple |
    E10kPAct5TDel FTupleID |
    E10kPActFDirAdd CFDirTuple |
    E10kPActFDirDel CFDirTuple
    deriving (Show)

data E10kFilterRef = E10kFilterFDir | E10kFilter5T FTupleID
    deriving (Show,Eq,Ord)

data E10kPState = E10kPState {
    e10k5TF :: M.Map FTupleID (Maybe SocketID),
    e10k5TFUnused :: [FTupleID],
    e10kSockets :: M.Map SocketID E10kFilterRef
} deriving (Show)

e10kPStateInit :: Int -> E10kPState
e10kPStateInit nFtf = E10kPState {
    e10k5TF = M.fromList $ zip ids $ repeat Nothing,
    e10k5TFUnused = ids,
    e10kSockets = M.empty }
    where ids = [0..(nFtf - 1)]

type E10kPolicyM = PolicyM E10kPState E10kPAction

ftfAvailable :: Int -> E10kPolicyM Bool
ftfAvailable n = do
    s <- getPRG
    return ((length $ e10k5TFUnused s) >= n)

fTo5T :: Flow -> QueueID -> C5Tuple
fTo5T (UDPIPv4Listen 0 port) q =
    C5Tuple { c5tPriority = 2, c5tQueue = q, c5tL3Proto = Just C5TPL3IPv4,
        c5tL4Proto = Just C5TPL4UDP, c5tL3Src = Nothing, c5tL3Dst = Nothing,
        c5tL4Src = Nothing, c5tL4Dst = Just port }
fTo5T (UDPIPv4Listen ip port) q =
    C5Tuple { c5tPriority = 1, c5tQueue = q, c5tL3Proto = Just C5TPL3IPv4,
        c5tL4Proto = Just C5TPL4UDP, c5tL3Src = Nothing,
        c5tL3Dst = Just $ show ip, c5tL4Src = Nothing, c5tL4Dst = Just port }
fTo5T (UDPIPv4Flow sAddr dAddr sPort dPort) q =
    C5Tuple { c5tPriority = 0, c5tQueue = q, c5tL3Proto = Just C5TPL3IPv4,
        c5tL4Proto = Just C5TPL4UDP, c5tL3Src = Just $ show sAddr,
        c5tL3Dst = Just $ show dAddr, c5tL4Src = Just sPort,
        c5tL4Dst = Just dPort }

ftfToQueue :: SocketDesc -> QueueID -> E10kPolicyM ()
ftfToQueue sd q = do
    p <- getPRG
    let (f:fs) = e10k5TFUnused p
    putPRG  $ p {
        e10k5TFUnused = fs,
        e10k5TF = M.insert f (Just sid) $ e10k5TF p,
        e10kSockets = M.insert sid (E10kFilter5T f) $ e10kSockets p }
    addEvent (PActHWAction $ E10kPAct5TSet f $ fTo5T (sdFlow sd) q)
    where sid = sdID sd

ftfDrop :: FTupleID -> E10kPolicyM ()
ftfDrop i = do
    p <- getPRG
    let (Just sid) = (e10k5TF p) M.! i
    putPRG $ p {
        e10k5TFUnused = [i] ++ e10k5TFUnused p,
        e10k5TF = M.insert i Nothing $ e10k5TF p,
        e10kSockets = M.delete sid $ e10kSockets p }
    addEvent $ PActHWAction $ E10kPAct5TDel i

fdirSockTuple :: SocketDesc -> QueueID -> CFDirTuple
fdirSockTuple sd q =
    CFDirTuple {
        cfdtQueue = q, cfdtL3Proto = C5TPL3IPv4, cfdtL4Proto = C5TPL4UDP,
        cfdtL3Src = IP4.ipToString sIP, cfdtL3Dst = IP4.ipToString dIP,
        cfdtL4Src = sP, cfdtL4Dst = dP }
    where
        (UDPIPv4Flow sIP dIP sP dP) = sdFlow sd


fdirToQueue :: SocketDesc -> QueueID -> E10kPolicyM ()
fdirToQueue sd q = do
    addEvent $ PActHWAction $ E10kPActFDirAdd $ fdirSockTuple sd q
    p <- getPRG
    putPRG $ p { e10kSockets = M.insert sid E10kFilterFDir $ e10kSockets p }
    where sid = sdID sd

fdirDrop :: SocketDesc -> E10kPolicyM ()
fdirDrop sd = do
    -- Not really elegant, but queue is irrelevant here
    addEvent $ PActHWAction $ E10kPActFDirDel $ fdirSockTuple sd (-1)
    p <- getPRG
    putPRG $ p { e10kSockets = M.delete sid $ e10kSockets p }
    where sid = sdID sd

-- Can free a 5-tuple filter if
--   - it is a flow
--   - and there are no conflicts
tryAndFreeFtf :: SocketDesc -> E10kPolicyM Bool
tryAndFreeFtf sd = do
    s <- get
    p <- getPRG
    let id2sd = ((psSockets s) M.!)
    let sIDs = catMaybes $ M.elems $ e10k5TF p
    let ss = [sd] ++ map id2sd sIDs
    let isClean sd' = (not $ flowIsListen $ sdFlow sd') &&
                        (null $ filter ((`flowCovers` f) . sdFlow) ss)
            where f = sdFlow sd'
    let fts = filter (\(_,Just i) -> isClean $ id2sd i) $ M.toList $
                e10k5TF p
    case fts of
        [] -> return False
        ((i,ft):_) -> do
            let sd' = id2sd i
            ftfDrop i
            --addEvent $ PActHWAction $ E10kPAct5TDel i
            --putPRG $ p { e10k5TF = M.insert i Nothing $ e10k5TF p }
            sd' `fdirToQueue` (sdQueue sd')
            return True

filterForSocket :: SocketDesc -> E10kPolicyM (Maybe E10kFilterRef)
filterForSocket sd = do
    p <- getPRG
    return $ M.lookup (sdID sd) (e10kSockets p)
        
    


e10kAddSocket :: SocketDesc -> QueueID -> E10kPolicyM QueueID
e10kAddSocket sd qid = do
    enoughFtfs <- ftfAvailable 1
    if enoughFtfs then do
        sd `ftfToQueue` qid
        return qid
    else
        if isConnection sd then do
            cConflicts <- conflictingListens sd
            if null cConflicts then do
                sd `fdirToQueue` qid
                return qid
            else do
                success <- tryAndFreeFtf sd
                if success then do
                    sd `ftfToQueue` qid
                    return qid
                else do
                    q <- findMatchingQueue sd
                    return q
        else do
            lConflicts <- coveredSockets sd
            success <- tryAndFreeFtf sd
            if success then do
                sd `ftfToQueue` qid
                return qid
            else do
                q <- findMatchingQueue sd
                return q

e10kDelSocket :: SocketDesc -> E10kPolicyM ()
e10kDelSocket sd = do
    -- TODO: Need to handle listening sockets that are closed but have conflicts
    f <- filterForSocket sd
    case f of
        Nothing -> return ()
        Just E10kFilterFDir -> fdirDrop sd
        Just (E10kFilter5T i) -> ftfDrop i

e10kRebalance :: QueueID -> QueueID -> E10kPolicyM Bool
e10kRebalance s d = return False
    {-s <- get
    let id2sd = ((psSockets s) M.!)
    let isCandidate sid = 
    let cs = filter isCandidate $ map fst $ filter ((== s) . snd) $
                M.toList $ psFlows s-}





------------------------------------------------------------------------------
-- Config generation monad to simplify things

type ConfigGenM = MR.RandT R.StdGen (MW.WriterT [((String,String),[PolicyAction E10kPAction])] E10kPolicyM)

cgmP :: E10kPolicyM a -> ConfigGenM a
cgmP = MT.lift . MT.lift

cgmStackState :: ConfigGenM [SocketDesc]
cgmStackState = cgmP $ pSockets

cgmNewSocket :: Flow -> ConfigGenM SocketID
cgmNewSocket f = cgmP $ policyAddSocket e10kAddSocket f

cgmDelSocket :: SocketID -> ConfigGenM ()
cgmDelSocket sock = cgmP $ policyRemoveSocket e10kDelSocket e10kRebalance sock

cgmStep :: String -> String -> ConfigGenM ()
cgmStep pref title = do
    ps <- get
    MW.tell [((pref,title), psEvents ps)]
    put $ ps { psEvents = [] }


cgmRun :: Int -> Int -> Int -> ConfigGenM () -> [((String,String),[PolicyAction E10kPAction])]
cgmRun queues n5tuples seed c = ev
    where
        wm = MW.execWriterT $ MR.evalRandT c $ R.mkStdGen seed
        ev = MS.evalState wm $ policyStateInit queues $ e10kPStateInit n5tuples

rndStep :: ConfigGenM String
rndStep = do
    socks <- cgmP pSockets
    let dests = map flowDest $ filter flowIsListen $ map sdFlow socks

    range <- MR.getRandomR (0 :: Int,2)
    case range of
        -- New listening socket
        0 -> do
            hasIP <- MR.getRandom
            dIP <- if hasIP then rndIP else return 0
            dPort <- rndPort
            s <- cgmNewSocket $ UDPIPv4Listen dIP dPort
            return ("New listening socket (" ++ IP4.ipToString dIP ++ "," ++
                        show dPort ++ ") = " ++ show s)

        -- New flow
        1 -> do
            exDest <- MR.getRandom
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
        rndPick :: [a] -> ConfigGenM a
        rndPick l = do { i <- MR.getRandomR (0, (length l) - 1); return (l !! i) }

        rndIP :: ConfigGenM Word32
        rndIP = MR.getRandom

        rndPort :: ConfigGenM Word16
        rndPort = MR.getRandomR (1,1024)

        rndEP :: ConfigGenM (Word32,Word16)
        rndEP = do
            ip <- rndIP
            port <- rndPort
            return (ip,port)


rndScenario :: Int -> ConfigGenM ()
rndScenario steps = do
    cgmStep (toLbl 0) ""
    mapM_ (\i -> do { s <- rndStep ; cgmStep (toLbl i) s }) [1..steps]
    where
        toLbl :: Int -> String
        toLbl i
            | steps < 10 = show i
            | i < 10     = "0" ++ show i
            | otherwise  =  show i


main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    writeFile "prgU.dot" $ toDot prg
    let queues = 3
        n5tuples = 3
    let rndS = rndScenario 10
    let events = cgmRun queues n5tuples 42 rndS
    putStrLn $ show events
    where
        config = [("CSynFilter", "false"), ("CSynOutput","drop")]

