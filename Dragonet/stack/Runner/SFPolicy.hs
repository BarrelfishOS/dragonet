-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Runner.SFPolicy (
    SFPAction(..),
    SFPState,
    sfPolicy,
    sfPStateInit
) where

import Dragonet.Incremental
import Runner.SFControl

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

type FTupleID = Int
data SFPAction =
    SFPAct5TSet FTupleID FTuple |
    SFPAct5TDel FTupleID |
    SFPActFDirAdd FDirTuple |
    SFPActFDirDel FDirTuple
    deriving (Show)

data SFFilterRef = SFFilterFDir | SFFilter5T FTupleID
    deriving (Show,Eq,Ord)

data SFPState = SFPState {
    sf5TF :: M.Map FTupleID (Maybe SocketID),
    sf5TFUnused :: [FTupleID],
    sfSockets :: M.Map SocketID SFFilterRef
} deriving (Show)

sfPStateInit :: Int -> SFPState
sfPStateInit nFtf = SFPState {
    sf5TF = M.fromList $ zip ids $ repeat Nothing,
    sf5TFUnused = ids,
    sfSockets = M.empty }
    where ids = [0..(nFtf - 1)]

type SFPolicyM = PolicyM SFPState SFPAction

ftfAvailable :: Int -> SFPolicyM Bool
ftfAvailable n = do
    s <- getPRG
    return ((length $ sf5TFUnused s) >= n)

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

ftfToQueue :: SocketDesc -> QueueID -> SFPolicyM ()
ftfToQueue sd q = do
    p <- getPRG
    let (f:fs) = sf5TFUnused p
    putPRG  $ p {
        sf5TFUnused = fs,
        sf5TF = M.insert f (Just sid) $ sf5TF p,
        sfSockets = M.insert sid (SFFilter5T f) $ sfSockets p }
    addEvent (PActHWAction $ SFPAct5TSet f $ fTo5T (sdFlow sd) q)
    where sid = sdID sd

ftfDrop :: FTupleID -> SFPolicyM ()
ftfDrop i = do
    p <- getPRG
    let (Just sid) = (sf5TF p) M.! i
    putPRG $ p {
        sf5TFUnused = [i] ++ sf5TFUnused p,
        sf5TF = M.insert i Nothing $ sf5TF p,
        sfSockets = M.delete sid $ sfSockets p }
    addEvent $ PActHWAction $ SFPAct5TDel i

fdirSockTuple :: SocketDesc -> QueueID -> FDirTuple
fdirSockTuple sd q =
    FDirTuple {
        fdtQueue = fromIntegral q, fdtL3Proto = L3IPv4, fdtL4Proto = L4UDP,
        fdtL3Src = sIP, fdtL3Dst = dIP, fdtL4Src = sP, fdtL4Dst = dP }
    where
        (UDPIPv4Flow sIP dIP sP dP) = sdFlow sd


fdirToQueue :: SocketDesc -> QueueID -> SFPolicyM ()
fdirToQueue sd q = do
    addEvent $ PActHWAction $ SFPActFDirAdd $ fdirSockTuple sd q
    p <- getPRG
    putPRG $ p { sfSockets = M.insert sid SFFilterFDir $ sfSockets p }
    where sid = sdID sd

fdirDrop :: SocketDesc -> SFPolicyM ()
fdirDrop sd = do
    -- Not really elegant, but queue is irrelevant here
    addEvent $ PActHWAction $ SFPActFDirDel $ fdirSockTuple sd (-1)
    p <- getPRG
    putPRG $ p { sfSockets = M.delete sid $ sfSockets p }
    where sid = sdID sd

-- Can free a 5-tuple filter if
--   - it is a flow
--   - and there are no conflicts
tryAndFreeFtf :: SocketDesc -> SFPolicyM Bool
tryAndFreeFtf sd = do
    s <- get
    p <- getPRG
    let id2sd = ((psSockets s) M.!)
    let sIDs = catMaybes $ M.elems $ sf5TF p
    let ss = [sd] ++ map id2sd sIDs
    let isClean sd' = (not $ flowIsListen $ sdFlow sd') &&
                        (null $ filter ((`flowCovers` f) . sdFlow) ss)
            where f = sdFlow sd'
    let fts = filter (\(_,Just i) -> isClean $ id2sd i) $ M.toList $
                sf5TF p
    case fts of
        [] -> return False
        ((i,_):_) -> do
            let sd' = id2sd i
            ftfDrop i
            --addEvent $ PActHWAction $ SFPAct5TDel i
            --putPRG $ p { sf5TF = M.insert i Nothing $ sf5TF p }
            sd' `fdirToQueue` (sdQueue sd')
            return True

filterForSocket :: SocketDesc -> SFPolicyM (Maybe SFFilterRef)
filterForSocket sd = do
    p <- getPRG
    return $ M.lookup (sdID sd) (sfSockets p)

sfAddSocket :: SocketDesc -> QueueID -> SFPolicyM QueueID
sfAddSocket sd qid = do
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

sfDelSocket :: SocketDesc -> SFPolicyM ()
sfDelSocket sd = do
    -- TODO: Need to handle listening sockets that are closed but have conflicts
    f <- filterForSocket sd
    case f of
        Nothing -> return ()
        Just SFFilterFDir -> fdirDrop sd
        Just (SFFilter5T i) -> ftfDrop i

sfRebalance :: QueueID -> QueueID -> SFPolicyM Bool
sfRebalance _ _ = return False
    {-s <- get
    let id2sd = ((psSockets s) M.!)
    let isCandidate sid =
    let cs = filter isCandidate $ map fst $ filter ((== s) . snd) $
                M.toList $ psFlows s-}


sfPolicy :: Policy SFPState SFPAction
sfPolicy = Policy {
        pPRGAddSocket = sfAddSocket,
        pPRGRemoveSocket = sfDelSocket,
        pPRGRebalance = sfRebalance
    }

