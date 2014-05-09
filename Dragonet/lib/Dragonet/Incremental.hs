module Dragonet.Incremental (
    IPv4Address,
    UDPPort,
    QueueID,
    Flow(..),
    SocketID,
    SocketDesc(..),
    Policy(..),
    PolicyAction(..),
    PolicyState(..),
    PolicyM,

    flowDest,
    flowIsListen,
    flowCovers,
    destCovers,
    isConnection,
    conflictingListens,
    findMatchingQueue,

    getPRG,
    putPRG,
    addEvent,

    policyStateInit,
    policyMRun,
    policyAddSocket,
    policyRemoveSocket
) where




import Control.Monad.State
import Control.Arrow
import Data.Maybe
import Data.Word
import Data.Function
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Graph.Inductive as DGI


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


------------------------------------------------------------------------------
-- Policies


data Policy a b = Policy {
    --pLPGSocketQueue :: SocketDesc -> PolicyM a b QueueID,
    pPRGAddSocket :: SocketDesc -> QueueID -> PolicyM a b QueueID,
    pPRGRemoveSocket :: SocketDesc -> PolicyM a b (),
    pPRGRebalance :: QueueID -> QueueID -> PolicyM a b Bool
}

instance (Show (Policy a b)) where
    show _ = "Policy"

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
    psEvents :: [PolicyAction b],
    psPolicy :: Policy a b
} deriving (Show)

policyStateInit :: Int -> a -> Policy a b -> PolicyState a b
policyStateInit nQ p policy = PolicyState {
    psSockets = M.empty,
    psFlows = M.empty,
    psQueues = M.fromList $ zip ids $ repeat 0,
    psPRGSt = p,
    psEvents = [],
    psPolicy = policy }
    where ids = [0..(nQ - 1)]


type PolicyM a b = State (PolicyState a b)

policyMRun :: PolicyState a b -> PolicyM a b c
        -> (PolicyState a b, [PolicyAction b], c)
policyMRun s a = (s' { psEvents = [] }, psEvents s', ret)
    where
        (ret,s') = runState a $ s { psEvents = [] }

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

policyAddSocket :: Flow -> PolicyM a b SocketDesc
policyAddSocket f = do
    s <- get
    let sm = psSockets s
    let sid = if M.null sm then 0 else ((fst $ M.findMax sm) + 1)
    let sd = SocketDesc { sdID = sid, sdFlow = f, sdNodes = [], sdQueue = -1 }
    let (qmin,_) = L.minimumBy (compare `on` snd) $ M.toList $ psQueues s
    q <- (pPRGAddSocket $ psPolicy s) sd qmin
    let sd' = sd { sdQueue = q }
    addEvent $ PActLPGAddSocket sd' q
    s' <- get
    put $ s' {
        psSockets = M.insert sid sd' $ psSockets s',
        psFlows = M.insert sid q $ psFlows s',
        psQueues = M.adjust (+1) q $ psQueues s' }
    return sd'

policyRemoveSocket :: SocketID -> PolicyM a b ()
policyRemoveSocket sid = do
    s <- get
    let sd = (psSockets s) M.! sid
    (pPRGRemoveSocket $ psPolicy s) sd
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
        _ <- (pPRGRebalance $ psPolicy s) qM qid
        return ()
    else
        return ()

