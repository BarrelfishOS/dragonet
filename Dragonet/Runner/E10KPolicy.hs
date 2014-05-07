module Runner.E10KPolicy (
    E10kPAction(..),
    E10kPState,
    e10kPolicy,
    e10kPStateInit
) where

import Dragonet.Incremental
import Runner.E10KControl

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

type FTupleID = Int
data E10kPAction =
    E10kPAct5TSet FTupleID FTuple |
    E10kPAct5TDel FTupleID |
    E10kPActFDirAdd FDirTuple |
    E10kPActFDirDel FDirTuple
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

fTo5T :: Flow -> QueueID -> FTuple
fTo5T (UDPIPv4Listen 0 port) q =
    FTuple { ftPriority = 1, ftQueue = fromIntegral q, ftL3Proto = Just L3IPv4,
        ftL4Proto = Just L4UDP, ftL3Src = Nothing, ftL3Dst = Nothing,
        ftL4Src = Nothing, ftL4Dst = Just port }
fTo5T (UDPIPv4Listen ip port) q =
    FTuple { ftPriority = 2, ftQueue = fromIntegral q, ftL3Proto = Just L3IPv4,
        ftL4Proto = Just L4UDP, ftL3Src = Nothing,
        ftL3Dst = Just ip, ftL4Src = Nothing, ftL4Dst = Just port }
fTo5T (UDPIPv4Flow sAddr dAddr sPort dPort) q =
    FTuple { ftPriority = 3, ftQueue = fromIntegral q, ftL3Proto = Just L3IPv4,
        ftL4Proto = Just L4UDP, ftL3Src = Just sAddr, ftL3Dst = Just dAddr,
        ftL4Src = Just sPort, ftL4Dst = Just dPort }

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

fdirSockTuple :: SocketDesc -> QueueID -> FDirTuple
fdirSockTuple sd q =
    FDirTuple {
        fdtQueue = fromIntegral q, fdtL3Proto = L3IPv4, fdtL4Proto = L4UDP,
        fdtL3Src = sIP, fdtL3Dst = dIP, fdtL4Src = sP, fdtL4Dst = dP }
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
        ((i,_):_) -> do
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
            --lConflicts <- coveredSockets sd
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
e10kRebalance _ _ = return False
    {-s <- get
    let id2sd = ((psSockets s) M.!)
    let isCandidate sid =
    let cs = filter isCandidate $ map fst $ filter ((== s) . snd) $
                M.toList $ psFlows s-}


e10kPolicy :: Policy E10kPState E10kPAction
e10kPolicy = Policy {
        pPRGAddSocket = e10kAddSocket,
        pPRGRemoveSocket = e10kDelSocket,
        pPRGRebalance = e10kRebalance
    }

