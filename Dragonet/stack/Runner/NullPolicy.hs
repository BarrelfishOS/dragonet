module Runner.NullPolicy (
    NullPAction(..),
    NullPState,
    nullPolicy,
    nullPStateInit
) where

import Dragonet.Incremental
import Runner.NullControl

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

type FTupleID = Int
data NullPAction =
    NullPAct5TSet FTupleID FTuple |
    NullPAct5TDel FTupleID |
    NullPActFDirAdd FDirTuple |
    NullPActFDirDel FDirTuple
    deriving (Show)

data NullFilterRef = NullFilterFDir | NullFilter5T FTupleID
    deriving (Show,Eq,Ord)

data NullPState = NullPState {
    null5TF :: M.Map FTupleID (Maybe SocketID),
    null5TFUnused :: [FTupleID],
    nullSockets :: M.Map SocketID NullFilterRef
} deriving (Show)

nullPStateInit :: Int -> NullPState
nullPStateInit nFtf = NullPState {
    null5TF = M.fromList $ zip ids $ repeat Nothing,
    null5TFUnused = ids,
    nullSockets = M.empty }
    where ids = [0..(nFtf - 1)]

type NullPolicyM = PolicyM NullPState NullPAction

ftfAvailable :: Int -> NullPolicyM Bool
ftfAvailable n = do
    s <- getPRG
    return ((length $ null5TFUnused s) >= n)

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

ftfToQueue :: SocketDesc -> QueueID -> NullPolicyM ()
ftfToQueue sd q = do
    p <- getPRG
    let (f:fs) = null5TFUnused p
    putPRG  $ p {
        null5TFUnused = fs,
        null5TF = M.insert f (Just sid) $ null5TF p,
        nullSockets = M.insert sid (NullFilter5T f) $ nullSockets p }
    addEvent (PActHWAction $ NullPAct5TSet f $ fTo5T (sdFlow sd) q)
    where sid = sdID sd

ftfDrop :: FTupleID -> NullPolicyM ()
ftfDrop i = do
    p <- getPRG
    let (Just sid) = (null5TF p) M.! i
    putPRG $ p {
        null5TFUnused = [i] ++ null5TFUnused p,
        null5TF = M.insert i Nothing $ null5TF p,
        nullSockets = M.delete sid $ nullSockets p }
    addEvent $ PActHWAction $ NullPAct5TDel i

fdirSockTuple :: SocketDesc -> QueueID -> FDirTuple
fdirSockTuple sd q =
    FDirTuple {
        fdtQueue = fromIntegral q, fdtL3Proto = L3IPv4, fdtL4Proto = L4UDP,
        fdtL3Src = sIP, fdtL3Dst = dIP, fdtL4Src = sP, fdtL4Dst = dP }
    where
        (UDPIPv4Flow sIP dIP sP dP) = sdFlow sd


fdirToQueue :: SocketDesc -> QueueID -> NullPolicyM ()
fdirToQueue sd q = do
    addEvent $ PActHWAction $ NullPActFDirAdd $ fdirSockTuple sd q
    p <- getPRG
    putPRG $ p { nullSockets = M.insert sid NullFilterFDir $ nullSockets p }
    where sid = sdID sd

fdirDrop :: SocketDesc -> NullPolicyM ()
fdirDrop sd = do
    -- Not really elegant, but queue is irrelevant here
    addEvent $ PActHWAction $ NullPActFDirDel $ fdirSockTuple sd (-1)
    p <- getPRG
    putPRG $ p { nullSockets = M.delete sid $ nullSockets p }
    where sid = sdID sd

-- Can free a 5-tuple filter if
--   - it is a flow
--   - and there are no conflicts
tryAndFreeFtf :: SocketDesc -> NullPolicyM Bool
tryAndFreeFtf sd = do
    s <- get
    p <- getPRG
    let id2sd = ((psSockets s) M.!)
    let sIDs = catMaybes $ M.elems $ null5TF p
    let ss = [sd] ++ map id2sd sIDs
    let isClean sd' = (not $ flowIsListen $ sdFlow sd') &&
                        (null $ filter ((`flowCovers` f) . sdFlow) ss)
            where f = sdFlow sd'
    let fts = filter (\(_,Just i) -> isClean $ id2sd i) $ M.toList $
                null5TF p
    case fts of
        [] -> return False
        ((i,_):_) -> do
            let sd' = id2sd i
            ftfDrop i
            --addEvent $ PActHWAction $ NullPAct5TDel i
            --putPRG $ p { null5TF = M.insert i Nothing $ null5TF p }
            sd' `fdirToQueue` (sdQueue sd')
            return True

filterForSocket :: SocketDesc -> NullPolicyM (Maybe NullFilterRef)
filterForSocket sd = do
    p <- getPRG
    return $ M.lookup (sdID sd) (nullSockets p)

nullAddSocket :: SocketDesc -> QueueID -> NullPolicyM QueueID
nullAddSocket sd qid = do
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

nullDelSocket :: SocketDesc -> NullPolicyM ()
nullDelSocket sd = do
    -- TODO: Need to handle listening sockets that are closed but have conflicts
    f <- filterForSocket sd
    case f of
        Nothing -> return ()
        Just NullFilterFDir -> fdirDrop sd
        Just (NullFilter5T i) -> ftfDrop i

nullRebalance :: QueueID -> QueueID -> NullPolicyM Bool
nullRebalance _ _ = return False
    {-s <- get
    let id2sd = ((psSockets s) M.!)
    let isCandidate sid =
    let cs = filter isCandidate $ map fst $ filter ((== s) . snd) $
                M.toList $ psFlows s-}


nullPolicy :: Policy NullPState NullPAction
nullPolicy = Policy {
        pPRGAddSocket = nullAddSocket,
        pPRGRemoveSocket = nullDelSocket,
        pPRGRebalance = nullRebalance
    }

