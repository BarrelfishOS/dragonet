{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Graphs.E10k (
    C5TL3Proto(..),
    C5TL4Proto(..),
    C5TPort,
    C5Tuple(..),
    CFDirTuple(..),
    prepareConf,

    parse5tCFG, c5tString, c5tFullString, parse5t,
    parseFDirCFG, cFDtString, cFDtFullString, parseFDT,

    strToC5t,

    ConfChange(..),
    mk5TupleFromFl, mkFDirFromFl,
    insert5tFromFl, insertFdirFromFl,
    replaceCcFromFl,
    ccQueue, ccIs5t,

    rx5tFilterTableFull,rxCfdFilterTableFull,
    rx5tFilterTableLen, rxCfdFilterTableLen,
    flowMatchesCc,

    cfgEmpty, cfgStr,
    graphH_, graphH,
    ftCount,
    fdirCount,

    ccString,
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Predicate           as PR
import qualified Dragonet.Configuration       as C
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Semantics           as SEM

import Dragonet.Flows (Flow (..))
import Dragonet.Conventions (QueueId)

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS

import Data.Word
import Data.Maybe
import qualified Data.List as L
import qualified Util.Misc as UM
import Control.Monad
import Data.Function (on)
import Data.Functor ((<$>))
import Data.String (fromString)
import Data.Char (isDigit)
import Debug.Trace (trace)

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTBV

import Graphs.Helpers

import Control.Exception (assert)
import Text.Show.Pretty (ppShow)

tr a b  = trace b a
trN a b = a

cfgEmpty = [
    ("RxC5TupleFilter", PG.CVList []),
    ("RxCFDirFilter", PG.CVList [])
 ]


ftCount = 128

-- NOTE: fdirCount is based on a value in prgE10kImpl for CFDirFilter
-- TODO: Get this value by parsing NIC prg instead of hardcoding it
fdirCount = 2048


rx5tFilterTableFull :: C.Configuration -> Bool
rx5tFilterTableSize  = ftCount
rx5tFilterTableFull cnf = case L.lookup "RxC5TupleFilter" cnf of
    Nothing            -> False
    Just (PG.CVList l) -> length l >= rx5tFilterTableSize

rx5tFilterTableLen cnf = case L.lookup "RxCC5tFilter" cnf of
    Nothing -> 0
    Just (PG.CVList l) -> length l


rxCfdFilterTableFull :: C.Configuration -> Bool
rxCfdFilterTableSize = fdirCount
rxCfdFilterTableFull cnf = case L.lookup "RxCFDirFilter" cnf of
    Nothing            -> False
    Just (PG.CVList l) -> length l >= rxCfdFilterTableSize

rxCfdFilterTableLen cnf = case L.lookup "RxCFDirFilter" cnf of
    Nothing -> 0
    Just (PG.CVList l) -> length l


addCfgFun :: PG.Node -> C.ConfFunction
addCfgFun n
    | l == "RxC5TupleFilter" = config5tuple
    | l == "RxCFDirFilter"   = configFDir
    | l == "RxQueues"        = configRxQueues
    | l == "TxQueues"        = configTxQueues -- not a real configuration, but helpful for building PRGs
    | otherwise = error $ "Unknown PRG CNode: '" ++ l ++ "'"
    where l = PG.nLabel n

addIncrCfgFun :: PG.Node -> C.ConfFunction
addIncrCfgFun n
    | l == "RxC5TupleFilter" = incrConfig5tuple
    | l == "RxCFDirFilter"   = incrConfigFDir
    | otherwise              = error $ "Unknown PRG CNode: '" ++ l ++ "'"
    where l = PG.nLabel n

addIncrCfgCounter :: PG.Node -> Int
addIncrCfgCounter n
    | l == "RxC5TupleFilter" = rx5tFilterTableSize
    | l == "RxCFDirFilter"   = rxCfdFilterTableSize
    | otherwise              = error $ "Unknown PRG CNode: '" ++ l ++ "'"
    where l = PG.nLabel n

-------------------------------------------------------------------------------
-- Implementation of configuration of 5-tuple filters

data C5TL3Proto = C5TPL3IPv4 | C5TPL3IPv6
    deriving (Eq)
instance Show C5TL3Proto where
    show C5TPL3IPv4 = "IPv4"
    show C5TPL3IPv6 = "IPv6"

data C5TL4Proto = C5TPL4TCP | C5TPL4UDP | C5TPL4SCTP | C5TPL4Other
    deriving (Eq)
instance Show C5TL4Proto where
    show C5TPL4TCP = "TCP"
    show C5TPL4UDP = "UDP"
    show C5TPL4SCTP = "SCTP"
    show C5TPL4Other = "Other"

type C5TPort = Word16
type C5TIP   = Word32

data C5Tuple = C5Tuple {
    c5tPriority :: Int,
    c5tQueue    :: QueueId,
    c5tL4Proto  :: Maybe C5TL4Proto,
    c5tL3Src    :: Maybe C5TIP,
    c5tL3Dst    :: Maybe C5TIP,
    c5tL4Src    :: Maybe C5TPort,
    c5tL4Dst    :: Maybe C5TPort
} deriving (Eq,Show)

c5tString :: C5Tuple -> String
c5tString c = "5T("++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l4p = fromMaybe "*" $ liftM show $ c5tL4Proto c
        showIP = IP4.ipToString
        l3s = maybe "*" showIP $ c5tL3Src c
        l3d = maybe "*" showIP $ c5tL3Dst c
        l4s = fromMaybe "*" $ liftM show $ c5tL4Src c
        l4d = fromMaybe "*" $ liftM show $ c5tL4Dst c

c5tFullString :: C5Tuple -> String
c5tFullString c = (c5tString c) ++ " -> Q" ++ (show q)
    where
        q = c5tQueue c

-- http://rosettacode.org/wiki/Tokenize_a_string#Haskell
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

-- NB: This is only used in E10k.flowQueue, which is deprecated
strToC5t :: String -> C5Tuple
strToC5t str = assert (check1 && check2) ret
    where check1 = (take 3 str) == "5T("
          check2 = (L.last str) == ')'
          len = length str
          x = take (len -3 -1) $ drop 3 str
          xl = splitBy (==',') x
          ret = C5Tuple {
                  -- we do not care about the priority and the queue, this
                  -- infromation should be encoded in the graph
                  c5tPriority  = 999
                , c5tQueue     = 999
                , c5tL4Proto   = c5prot (xl !! 0)
                , c5tL3Src     = c5Ip   (xl !! 1)
                , c5tL3Dst     = c5Ip   (xl !! 2)
                , c5tL4Src     = c5Port (xl !! 3)
                , c5tL4Dst     = c5Port (xl !! 4)
          }
          c5prot :: String -> Maybe C5TL4Proto
          c5prot s
            | s == "*"   = Nothing
            | s == "UDP" = Just C5TPL4UDP
          c5Ip :: String -> Maybe C5TIP
          c5Ip s
            | s == "*"   = Nothing
            | otherwise  = IP4.ipFromString s
          c5Port :: String -> Maybe C5TPort
          c5Port s
            | s == "*"  = Nothing
            | otherwise = Just $ read s

c5tAttr :: C5Tuple -> [PG.NAttribute]
c5tAttr c =
    if null constr then [] else [aTrue, aFalse]
    where
        constr = catMaybes $ [
                do {p <- c5tL4Proto c; return (show p)},
                do {s <- c5tL3Src c; return ("SourceIP=" ++ showIP s)},
                do {d <- c5tL3Dst c; return ("DestIP=" ++ showIP d)},
                do {s <- c5tL4Src c; return ("SourcePort=" ++ show s)},
                do {d <- c5tL4Dst c; return ("DestPort=" ++ show d)} ]
        cT = foldl1 (\a b -> a ++ "&" ++ b) constr
        aTrue = PG.NAttrCustom $ "C.true:" ++ cT
        aFalse = PG.NAttrCustom $ "C.false:!(" ++ cT ++ ")"
        showIP = IP4.ipToString

parse5tCFG :: C.ConfValue -> [C5Tuple]
parse5tCFG (C.CVList l) = map parse5t l

parse5t :: C.ConfValue -> C5Tuple
parse5t cv = case parse5t_ cv of
    Just x -> x
    Nothing -> error $ "this 5 tuple format not not supported: "  ++ (show cv)

-- parse a replace configuration variable
parse5tReplace_ :: C.ConfValue -> Maybe (C5Tuple, C5Tuple)
parse5tReplace_ (C.CVTuple [cnf1, cnf2]) =
    case (parse5t_ cnf1, parse5t_ cnf2) of
        (Just c5t1, Just c5t2) -> Just (c5t1, c5t2)
        _ -> Nothing
parse5tReplace_ _ = Nothing

parse5t_ :: C.ConfValue -> Maybe C5Tuple
parse5t_ (C.CVTuple
           [C.CVMaybe mSIP,
            C.CVMaybe mDIP,
            C.CVMaybe mProto,
            C.CVMaybe mSPort,
            C.CVMaybe mDPort,
            C.CVInt prio,
            C.CVInt queue]) =
    Just $ C5Tuple {
        c5tPriority = fromIntegral $ prio,
        c5tQueue = fromIntegral $ queue,
        c5tL4Proto = convProto <$> mProto,
        c5tL3Src = convInt <$> mSIP,
        c5tL3Dst = convInt <$> mDIP,
        c5tL4Src = convInt <$> mSPort,
        c5tL4Dst = convInt <$> mDPort
    }
    where
        convProto (C.CVEnum 0) = C5TPL4TCP
        convProto (C.CVEnum 1) = C5TPL4UDP
        convProto (C.CVEnum 2) = C5TPL4SCTP
        convProto (C.CVEnum 3) = C5TPL4Other
        convInt (C.CVInt i) = fromIntegral i
-- w/o priority
parse5t_ (C.CVTuple
           [C.CVMaybe mSIP,
            C.CVMaybe mDIP,
            C.CVMaybe mProto,
            C.CVMaybe mSPort,
            C.CVMaybe mDPort,
            C.CVInt queue]) =
    Just $ C5Tuple {
        c5tPriority = 1,
        c5tQueue = fromIntegral $ queue,
        c5tL4Proto = convProto <$> mProto,
        c5tL3Src = convInt <$> mSIP,
        c5tL3Dst = convInt <$> mDIP,
        c5tL4Src = convInt <$> mSPort,
        c5tL4Dst = convInt <$> mDPort
    }
    where
        convProto (C.CVEnum 0) = C5TPL4TCP
        convProto (C.CVEnum 1) = C5TPL4UDP
        convProto (C.CVEnum 2) = C5TPL4SCTP
        convProto (C.CVEnum 3) = C5TPL4Other
        convInt (C.CVInt i) = fromIntegral i
parse5t_ _ = Nothing

{-|
 - Returns a PG.Node based on given 5Tuple configuration.
 -  - It creates a empty node with `c5tString c` as label and [true, false] as
 -      outgoing ports
 -  - It then modifies this node with proper attributes, semantics and
 -      predicates
 -}
nodeL5Tuple :: C5Tuple -> PG.Node
nodeL5Tuple c = (PG.baseFNode (c5tString c) bports) {
                    PG.nAttributes = c5tAttr c,
                    PG.nSemantics = [("true",tSems),("false",fSems)],
                    PG.nPredicates = [
                        ("true",  c5TuplePredT c),
                        ("false", c5TuplePredF c)
                    ]
                    }
    where
        bports = ["true","false"]
        tSems = foldl1 SMTC.and $ (catMaybes  [
                ipSems "src" <$> c5tL3Src c,
                ipSems "dst" <$> c5tL3Dst c,
                portSems "src" <$> c5tL4Src c,
                portSems "dst" <$> c5tL4Dst c
                ]) ++ protoSems
        fSems = SMTC.not tSems
        pkt = SMT.app "pkt" []
        ipSems n i = SMTBV.bv (fromIntegral i) 32 SMTC.===
            SMT.app (fromString $ "IP4." ++ n) [pkt]
        portSems n i = SMTBV.bv (fromIntegral i) 16 SMTC.===
                    SMT.app (fromString $ "UDP." ++ n) [pkt]
        protoSems = [
            SMT.app "L3.Proto" [pkt] SMTC.=== SMT.app "L3P.IP4" [],
            SMT.app "L4.Proto" [pkt] SMTC.=== SMT.app "L4P.UDP" []
            ]

isRxQValidN :: Int -> PG.PGNode -> Bool
isRxQValidN i (_,n) =
    (PG.nLabel n == "Q" ++ show i ++ "Valid") ||
        (PG.nLabel n == "RxQ" ++ show i ++ "Valid")


--
-- Configuring Filter nodes
--

-- configure filter node helper #1:
--   get the node id of a specific queue from the out edges
filterQueueNid :: [(PG.PGNode, PG.Edge)] -> QueueId -> DGI.Node
filterQueueNid outEs qid = case L.find (isRxQValidN qid . fst) outEs of
        Just ((x,_),_) -> x
        Nothing        -> error errmsg
    where errmsg = "filterQueueNid: no RxQValid for queue=" ++ (show qid)
                 ++ " Does the queue exist?"

-- configure filter node helper #2:
--   get the node id of the default queue
filterDefaultNid :: [(PG.PGNode, PG.Edge)] -> DGI.Node
filterDefaultNid outEs = case L.find ((== "default") . PG.ePort . snd) outEs of
      (Just ((x,_),_)) -> x
      Nothing          -> error "filterDefaultNid: could not find default queue nid"

-- adds a single filter incrementally (i.e., C-node is re-added)
-- Notes:
--  - filter nodes expect a single in edge
doConfigFilterInc
  ::  (C.ConfValue -> a) -- filter from conf value
  ->  (a -> PG.Node)     -- get filter node
  ->  (a -> QueueId)     -- get queue id for this filter
  -> C.ConfFunction
doConfigFilterInc parseConf mkFiltLabel getQueue
                  _ (_, cnodeLabel_@(PG.CNode {}))
                  (inE:[]) outEs cfg = do

    let filter = parseConf cfg
        count = PG.nIncrCounter cnodeLabel_
        count' = count - 1
        cnodeLabel = trN cnodeLabel_ ("IncrCounter=" ++ (ppShow count'))
    -- add a filter node and a C-node node
    (filterNid,_) <- C.confMNewNode $ mkFiltLabel filter
    nextNid <- case count' of
            0 -> return $ filterDefaultNid outEs
            _ -> do (nid,_) <- C.confMNewNode
                               $ cnodeLabel {PG.nIncrCounter = count'}
                    return nid

    let filterQNid = filterQueueNid outEs $ getQueue filter
        ((inNid,_), inEdgeLabel) = inE
        -- in edge
        inEdge    = (inNid, filterNid, inEdgeLabel)
        -- out edges to Queue OR node (we do  not include the false edge)
        tEdgeOR   = (filterNid, filterQNid, PG.Edge "true")
        fEdgeOR   = (filterNid, filterQNid, PG.Edge "false")
        -- false edge to (new) CNode
        nextIn   = (filterNid, nextNid, PG.Edge "false")
        cnodeOuts = case count' of
                0 -> []
                _ -> [ (nextNid, xid, xedge) | ((xid,_),xedge) <- outEs ]
        newEdges  = inEdge:tEdgeOR:nextIn:cnodeOuts
    return newEdges


doConfigFilterInc _ _ _ _ (_,node) inE _ _ = do
    return $ error $ "Error: doConfigFilterInc called on node with >1 in edges:" ++ (PG.nLabel node) ++ " " ++ (ppShow inE)

-- Common helper for configuring 5-tuple and FD configuration nodes
--  - filter nodes expect a *single* in edge
doConfigFilter :: (C.ConfValue -> [a]) -- configuration value to filters
               -> (a -> PG.Node)       -- get filter node
               -> (a -> QueueId)       -- get queue for this filter
               -> C.ConfFunction
doConfigFilter parseConf mkFiltLabel getQueue _ _ (inE:[]) outEs cfg = do
    -- inEs  :: [(PG.PGNode, PG.Edge)] -- CNode in edges
    -- outEs :: [(PG.PGNode, PG.Edge)] -- CNode out edges
    let filters = parseConf cfg
        -- start: node and edge labe (i.e., port) for the first incoming edge
        start :: (DGI.Node, PG.Edge)
        start = (fst $ fst $ inE, snd inE)
        -- initial state for the addFilter fold.
        -- State holds:
        --   . start node/edge labe -> where to add the (next) filter node
        --   . list of edges to add to the graph
        state0 = (start, [])

    ((endN,endP),edges) <- foldM addFilter state0 filters

    -- connect the last edge to the default node and return all new edges
    let defaultNid = filterDefaultNid outEs
        lastEdge = (endN,defaultNid,endP)

    return (edges ++ [lastEdge])

    where
      -- add a filter to the graph
      addFilter ((iN,iE),es) filter = do
         -- create a new node for the filter
         (filterNid,_) <- C.confMNewNode $ mkFiltLabel filter
         let filterQNid = filterQueueNid outEs $ getQueue filter
             -- in edge
             inEdge = (iN, filterNid,iE)
             -- out edges to Queue OR node
             tEdge = (filterNid, filterQNid, PG.Edge "true")
             fEdge = (filterNid, filterQNid, PG.Edge "false")
         -- NB: We do not include the false edge to the OR queue node, to make
         -- it easier to reason about where flows end up in things like
         -- flowQueue
         -- return ((filterNid,Edge "false"), es ++ [inEdge,tEdge,fEdge])
         return ((filterNid, PG.Edge "false"), es ++ [inEdge,tEdge])
doConfigFilter a b c d e@(_, cnode) (x:xs) f g = trN ret msg
    where ret = doConfigFilter a b c d e [x] f g
          msg = "FIXME: Configuration node: " ++ (PG.nLabel cnode)
              ++ " has more than one incoming edges."
              ++ " Only using first!"
-- incrementally configure a 5-tuple
-- XXX: this does not consider 5-tuple priorities (The oracle does not generate
-- 5t filters with priorities, so it's good for now)
incrConfig5tuple :: C.ConfFunction
incrConfig5tuple a b c d cfgV =
    case parse5t_ cfgV of
        Just _  -> doConfigFilterInc parse5t nodeL5Tuple c5tQueue a b c d cfgV
        Nothing -> error "incrConfig5tuple: NYI!"

-- fully configure a 5-tuple
config5tuple :: C.ConfFunction
config5tuple = doConfigFilter parseConf nodeL5Tuple c5tQueue
    where parseConf c = trN cfgs_ (ppShow cfgs_)
            where cmpPrio = compare `on` c5tPriority
                  cfgs_ = reverse $ L.sortBy cmpPrio $ parse5tCFG c

-- incrementally configure a FD filter
incrConfigFDir :: C.ConfFunction
incrConfigFDir = doConfigFilterInc parseFDT nodeLFDir cfdtQueue

-- fully configure a FD filter
configFDir :: C.ConfFunction
configFDir = doConfigFilter parseFDirCFG nodeLFDir cfdtQueue

-------------------------------------------------------------------------------
-- Implementation of configuration of the flow director filters


data CFDirTuple = CFDirTuple {
    cfdtQueue    :: QueueId,
    cfdtL4Proto  :: C5TL4Proto,
    cfdtL3Src    :: C5TIP,
    cfdtL3Dst    :: C5TIP,
    cfdtL4Src    :: C5TPort,
    cfdtL4Dst    :: C5TPort
} deriving (Eq,Show)

cFDtString :: CFDirTuple -> String
cFDtString c = "FDir("++l4p++","++l3s++","++l3d++","++l4s++","++l4d++")"
    where
        l4p = show $ cfdtL4Proto c
        l3s = IP4.ipToString $ cfdtL3Src c
        l3d = IP4.ipToString $ cfdtL3Dst c
        l4s = show $ cfdtL4Src c
        l4d = show $ cfdtL4Dst c

cFDtFullString :: CFDirTuple -> String
cFDtFullString c = (cFDtString c) ++ " -> Q" ++ (show q)
    where
         q = cfdtQueue c

cFDtAttr :: CFDirTuple -> [PG.NAttribute]
cFDtAttr c =
    [aTrue, aFalse]
    where
        constr :: [String]
        constr = [show $ cfdtL4Proto c,
                  ("SourceIP=" ++ (IP4.ipToString $ cfdtL3Src c)),
                  ("DestIP=" ++ (IP4.ipToString $ cfdtL3Dst c)),
                  ("SourcePort=" ++ (show $ cfdtL4Src c)),
                  ("DestPort=" ++ (show $ cfdtL4Dst c)) ]
        cT = constr `UM.joinBy` "&"
        aTrue = PG.NAttrCustom $ "C.true:" ++ cT
        aFalse = PG.NAttrCustom $ "C.false:!(" ++ cT ++ ")"

parseFDirCFG :: C.ConfValue -> [CFDirTuple]
parseFDirCFG (C.CVList l) = map parseFDT l

{-|
 - Parses the configuration and returns FDirTuple filter
 -}
parseFDT :: C.ConfValue -> CFDirTuple
parseFDT (C.CVTuple
           [C.CVMaybe mSIP,
            C.CVMaybe mDIP,
            C.CVMaybe mProto,
            C.CVMaybe mSPort,
            C.CVMaybe mDPort,
            C.CVInt queue]) =
    CFDirTuple {
            cfdtQueue     = fromIntegral queue,
            cfdtL4Proto   = convProto  mProto,
            cfdtL3Src     = convInt  mSIP,
            cfdtL3Dst     = convInt  mDIP,
            cfdtL4Src     = convInt  mSPort,
            cfdtL4Dst     = convInt  mDPort
    }
    where
        convProto (Just (C.CVEnum 0)) = C5TPL4TCP
        convProto (Just (C.CVEnum 1)) = C5TPL4UDP
        convProto (Just (C.CVEnum 2)) = C5TPL4SCTP
        convProto (Just (C.CVEnum 3)) = C5TPL4Other
        convProto (Just x) = error ("Don't know the value "  ++ (show x))
        convInt (Just (C.CVInt i)) = fromIntegral i
        convInt (Just x) = error ("Don't know the value "  ++ (show x))
        convInt x = error ("Don't know the value ---"  ++ (show x))

parseFDTOld :: C.ConfValue -> CFDirTuple
parseFDTOld (C.CVTuple
           [sIP,
            dIP,
            proto,
            sPort,
            dPort,
            C.CVInt queue]) =
    CFDirTuple {
        cfdtQueue = fromIntegral queue,
        cfdtL4Proto = convProto proto,
        cfdtL3Src = convInt sIP,
        cfdtL3Dst = convInt dIP,
        cfdtL4Src = convInt sPort,
        cfdtL4Dst = convInt dPort
    }
    where
        convProto (C.CVEnum 0) = C5TPL4TCP
        convProto (C.CVEnum 1) = C5TPL4UDP
        convProto (C.CVEnum 2) = C5TPL4SCTP
        convProto (C.CVEnum 3) = C5TPL4Other
        convInt (C.CVInt i) = fromIntegral i



{-|
 - Returns a PG.Node based on given FDir configuration.
 -  - It creates a empty node with `cFDtString c` as label and [true, false] as
 -      outgoing ports
 -  - It then modifies this node with proper attributes, semantics and
 -      predicates
 -}
nodeLFDir :: CFDirTuple -> PG.Node
nodeLFDir c = (PG.baseFNode (cFDtString c) bports) {
                    PG.nAttributes = cFDtAttr c,
                    PG.nSemantics = [("true",tSems),("false",fSems)],
                    PG.nPredicates = [
                        ("true",  cFDirPredT c),
                        ("false", cFDirPredF c)
                    ]
                    }
    where
        bports = ["true","false"]
        tSems = foldl1 SMTC.and $ (catMaybes  [
                ipSems "src" <$> (Just $ cfdtL3Src c),
                ipSems "dst" <$> (Just $ cfdtL3Dst c),
                portSems "src" <$> (Just $ cfdtL4Src c),
                portSems "dst" <$> (Just $ cfdtL4Dst c)
                ]) ++ protoSems
        fSems = SMTC.not tSems
        pkt = SMT.app "pkt" []
        ipSems n i = SMTBV.bv (fromIntegral i) 32 SMTC.===
            SMT.app (fromString $ "IP4." ++ n) [pkt]
        portSems n i = SMTBV.bv (fromIntegral i) 16 SMTC.===
                    SMT.app (fromString $ "UDP." ++ n) [pkt]
        protoSems = [
            SMT.app "L3.Proto" [pkt] SMTC.=== SMT.app "L3P.IP4" [],
            SMT.app "L4.Proto" [pkt] SMTC.=== SMT.app "L4P.UDP" []
            ]


duplicateDFS :: PG.PGraph -> PG.PGNode -> DGI.Node -> (String -> String)
             -> C.ConfMonad [PG.PGEdge]
duplicateDFS g (nid,nlbl) nid' dupname = do
    let dfs = DFS.dfsWith getPgN [nid] g
        getPgN ctx@(ins,nid,nlbl,outs) = ((nid,nlbl),outs)
        nodes    = map fst dfs
        nodesDup = drop 1 nodes -- nodes to be duplicated (do not include first node)
        oEdges = map snd dfs

        dupNode :: PG.Node -> PG.Node
        dupNode n = n { PG.nLabel = (dupname $ PG.nLabel n) }

    dupNodes <- forM (map (dupNode . snd) nodesDup) C.confMNewNode

    let nodeMap :: [(DGI.Node,  DGI.Node)]
        nodeMap_ = [(nid, dupNid) | ((nid,_), (dupNid,_)) <- zip nodesDup dupNodes]
        nodeMap  = (nid,nid'):nodeMap_

        getOutEs ctx@(ins,nid,nlbl,outs) = outs

        mapOutNodeEdges :: DGI.Node -> PG.PGAdj -> [(DGI.Node, DGI.Node, PG.Edge)]
        mapOutNodeEdges nid adjs = [ (getDup nid, getDup nid_o, e) | (e,nid_o) <- adjs ]
            where getDup :: DGI.Node -> DGI.Node
                  getDup nid = case L.lookup nid nodeMap of
                    Just x -> x
                    Nothing -> error "duplicateDFS: this is not supposed to happen"

        outEdges = L.concat [mapOutNodeEdges nid adjs | ((nid,_),adjs) <- dfs]
        -- filter self edges to the first node
        f_fn (nid1, nid2, _) = not (nid1 == nid' && nid2 == nid')
        outEdges' = filter f_fn outEdges

    return outEdges'


-- Add an OR node in front of each Queue
-- All nodes (and edges) after the queue are duplicated for each queue
-- Incomming edges to the configuration queue node
configRxQueues :: C.ConfFunction
configRxQueues g (cfgnid,cfgn) inE outE (C.CVInt qs) = do

    -- first (default) queue
    ret <- forM [0..qs-1] addNode

    return $ concat $ map snd ret

    where addNode n = do
            -- first create queue copies
            let name = "RxQueue" ++ (show n)
                attrs = PG.nAttributes cfgn
                ports = PG.nPorts cfgn
                qimpl = PG.NImplFunction "E10kRxQueue"
                node = (PG.baseFNode name ports) { PG.nAttributes = attrs,
                                                   PG.nImplementation = qimpl}
                oname = "RxQ" ++ (show n) ++ "Valid"
                onode = PG.baseONode oname ["true","false"] PG.NOpOr {}
                defQ = 0

            (q_nid, _) <- C.confMNewNode node
            (o_nid, _) <- C.confMNewNode onode

            let edge_to_self :: (PG.PGNode, PG.Edge) -> Bool
                edge_to_self ((_, PG.CNode {PG.nLabel = x}), _) = x == PG.nLabel cfgn
                edge_to_self _ = False


                (inSelf, inOther)   = L.partition edge_to_self inE
                (outSelf, outOther) = L.partition edge_to_self outE

                self :: [PG.Edge] -- self edges
                self_in  = L.sort $ [ e | (_, e) <- inSelf ]
                self_out = L.sort $ [ e | (_, e) <- outSelf ]
                self = case (self_in == self_out) of
                    True -> self_in
                    False -> error "in and out self edges do not match"

                edge_default :: (PG.PGNode, PG.Edge) -> Bool
                edge_default (_, PG.Edge { PG.ePort = p }) = L.isPrefixOf "default" p
                (inDef, inQueues) = L.partition edge_default inOther

                -- default edges are included only in queue 1
                iE    = case n == defQ of
                    True  -> [ (fst x, o_nid, e) | (x, e) <- inOther]
                    False -> [ (fst x, o_nid, e)  | (x, e) <- inQueues]
                -- out Edges
                oE =  [ (q_nid, fst x, e)  | (x, e) <- outOther]
                -- self edges
                sE  = [ (q_nid, q_nid, e)    | e <- self ]
                -- or edges (just the true port)
                orE = [ (o_nid, q_nid, PG.Edge { PG.ePort = "true" }) ]

            -- duplciate all the nodes after the queue for all the queues but
            -- the first onde
            dupEs  <- case n == defQ of
                       True  -> return oE
                       False -> duplicateDFS g (cfgnid,cfgn) q_nid (++ "__RxQ" ++ (show n))

            let newEdges = iE ++ sE ++ orE ++ dupEs
            return ((q_nid,o_nid),newEdges)

configTxQueues :: C.ConfFunction
configTxQueues _ (_,cfgn) inE outE (C.CVInt qs) = do
    ret <- foldM addNode [] [0..qs-1]
    return ret
    where addNode prev n = do
            -- first create queue copies
            let name = "TxQueue" ++ (show n)
                attrs = PG.nAttributes cfgn
                ports = PG.nPorts cfgn
                qimpl = PG.NImplFunction "E10kTxQueue0"
                node = (PG.baseFNode name ports) { PG.nAttributes = attrs,
                                                   PG.nImplementation = qimpl}
                onode = PG.baseONode name ["true","false"] PG.NOpOr {}

            (q_nid, _) <- C.confMNewNode node
            let edge_to_self :: (PG.PGNode, PG.Edge) -> Bool
                edge_to_self ((_, PG.CNode { PG.nLabel = xlbl }), _) = xlbl == PG.nLabel cfgn
                edge_to_self _ = False
                self :: [PG.Edge] -- self edges
                self_in  = L.sort $ [ e | (_, e) <- filter edge_to_self inE]
                self_out = L.sort $ [ e | (_, e) <- filter edge_to_self outE]
                self = case (self_in == self_out) of
                    True -> self_in
                    False -> error "incomming and outgoing self edges do not match"
                -- in edges
                iE  = [ (fst x, q_nid, e)  | (x, e) <- filter (not . edge_to_self) inE]
                -- out edges
                oE  = [ (q_nid, fst x, e)  | (x, e) <- filter (not . edge_to_self) outE]
                -- self edges
                sE  = [ (q_nid, q_nid, e)    | e <- self ]
            return $ prev ++ iE ++ oE ++ sE


prepareConf :: PG.PGraph -> PG.PGraph
prepareConf g = C.replaceIncrConf addIncrCfgCounter addIncrCfgFun $
                C.replaceConfFunctions addCfgFun g

----
-- Try to figure out at  which queue a flow will end up.
-- Very quick-n-dirty for the moment
--

{-|
 - Creates a layer-5 full tuple based on given values.
 -  NOTE: currently only works with UDP protocol
 -}
fullL5Pred
  :: (Show a, Show a1, Show a2, Show a3)
  => Maybe C5TL4Proto   -- ^ Maybe Protocol type
  -> Maybe a3           -- ^ Maybe src IP address
  -> Maybe a2           -- ^ Maybe dst IP address
  -> Maybe a1           -- ^ Maybe src port
  -> Maybe a            -- ^ Maybe dst port
  -> PR.PredExpr        -- ^ Returns Predicate expressing condition matching
                        --     the values (or wildcards)
fullL5Pred cProt cSrcIp cDstIp cSrcPort cDstPort
 | cProt == Just C5TPL4UDP = ret
       where protPreds = [PR.PredAtom "EthType" "IPv4", PR.PredAtom "IpProt" "UDP"]
             srcIpPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "SrcIp" ("p" ++ show x)))
                               cSrcIp
             dstIpPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "DstIp" ("p" ++ show x)))
                               cDstIp
             srcPortPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "SrcPort" ("p" ++ show x)))
                               cSrcPort
             dstPortPred = maybe PR.PredTrue
                               (\x -> (PR.PredAtom "DstPort" ("p" ++ show x)))
                               cDstPort
             ret = xand $ protPreds ++
                                [srcIpPred,dstIpPred,srcPortPred,dstPortPred]
             bld = PR.predBuildFold
             xand = (PR.buildAND bld)



cFDirPredT :: CFDirTuple -> PR.PredExpr
cFDirPredT (CFDirTuple {
                        cfdtL4Proto = cProt,
                        cfdtL3Src   = cSrcIp,
                        cfdtL3Dst   = cDstIp,
                        cfdtL4Src   = cSrcPort,
                        cfdtL4Dst   = cDstPort}) =
      fullL5Pred (Just cProt) (Just cSrcIp) (Just cDstIp)
            (Just cSrcPort) (Just cDstPort)

{-| Prepending the cFDirTuple Predicate with False
 -}
cFDirPredF :: CFDirTuple -> PR.PredExpr
cFDirPredF fdir = (PR.buildNOT bld) (cFDirPredT fdir)
    where bld = PR.predBuildFold


c5TL4ProtoPred :: C5TL4Proto -> PR.PredExpr
c5TL4ProtoPred C5TPL4UDP = PR.PredAnd [PR.PredAtom "EthType" "IPv4",
                                       PR.PredAtom "IpProt" "UDP"]


c5TuplePredT :: C5Tuple -> PR.PredExpr
c5TuplePredT (C5Tuple {c5tL4Proto = cProt,
                      c5tL3Src   = cSrcIp,
                      c5tL3Dst   = cDstIp,
                      c5tL4Src   = cSrcPort,
                      c5tL4Dst   = cDstPort}) =
      fullL5Pred cProt cSrcIp cDstIp cSrcPort cDstPort

{-| Prepending the cTuple Predicate with False
 -}
c5TuplePredF :: C5Tuple -> PR.PredExpr
c5TuplePredF t5 = (PR.buildNOT bld) (c5TuplePredT t5)
    where bld = PR.predBuildFold


--
-- ConfChange
--

{-|
  The 'E10kConfChange' type holds all the legal changes in the configuration
  of the 82599 NIC.
  This type is an instance of generic ConfChange.
   TODO: It should also include E10KInsertFdir, and RemoveFilter,
   Update filters, as we are introducing them
 -}
type FilterId = Int -- we do not use it right now, but it might be helpful
data ConfChange = Insert5T   (FilterId, PG.ConfValue)
                | InsertFDir (FilterId, PG.ConfValue)
                | InsertSYN  PG.ConfValue
                | CcReplace  ConfChange PG.ConfValue
    deriving (Eq, Ord, Show)

ccString :: ConfChange -> String
ccString (Insert5T (fid,cnf))   = c5tFullString  $ parse5t cnf
ccString (InsertFDir (fid,cnf)) = cFDtFullString $ parseFDT cnf
ccString (CcReplace (Insert5T (fid,cnf)) cnf') = "REPLACE: OLD:" ++ s1 ++ " NEW:" ++ s2
    where s1 =  c5tFullString  $ parse5t cnf
          s2 =  c5tFullString  $ parse5t cnf'
ccString (CcReplace (InsertFDir (fid,cnf)) cnf') = "REPLACE: OLD:" ++ s1 ++ " NEW:" ++ s2
    where s1 =  cFDtFullString $ parseFDT cnf
          s2 =  cFDtFullString $ parseFDT cnf'

addToCVL :: PG.ConfValue -> PG.ConfValue -> PG.ConfValue
addToCVL (PG.CVList l) v = PG.CVList $ v:l

replaceCVL :: PG.ConfValue -> (PG.ConfValue,  PG.ConfValue) -> PG.ConfValue
replaceCVL (PG.CVList l) (old,new) = PG.CVList $ doReplace l
    where doReplace :: [PG.ConfValue] -> [PG.ConfValue]
          doReplace (x:xs) = case x == old of
                               True  -> new:xs
                               False -> x:(doReplace xs)
          doReplace [] = error $ "replaceCVL: value to replace not found!\n"
                               ++ " list: " ++ (ppShow l) ++ "\n"
                               ++ " old:  " ++ (ppShow old) ++ "\n"
                               ++ " new: " ++ (ppShow new) ++ "\n"

ccIs5t (Insert5T _) = True
ccIs5t _  = False

-- Overwrite the old value(s) with new value
overwriteCVL :: PG.ConfValue -> PG.ConfValue -> PG.ConfValue
overwriteCVL (PG.CVList l) v = PG.CVList $ [v]

instance C.ConfChange ConfChange where
    {-|
      The function 'applyConfChange' will add the given 5tuple filter into
      the list if existing 5tuple fitlers, while keeping everything else
      in the configuration same.
      TODO: Add code to add other type of actions as well (fdir, delete, update)
     -}
    --applyConfChange :: C.Configuration -> E10kConfChange -> C.Configuration
    emptyConfig _ = cfgEmpty
    showConfig _ = cfgStr
    ccShow = ccString

    -- 5t filters
    applyConfChange conf (Insert5T (_,c5t)) = ("RxC5TupleFilter", new5t):rest
        where new5t :: PG.ConfValue
              new5t = addToCVL old5t c5t
              old5t :: PG.ConfValue
              old5t = case L.lookup "RxC5TupleFilter" conf of
                        Just l -> l
                        Nothing -> error "Insert5t: Did not find RxC5TupleFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxC5TupleFilter") . fst) conf

    applyConfChange conf (CcReplace (Insert5T (_,c5t)) c5t') =
       ("RxC5TupleFilter", new5t):rest
        where new5t :: PG.ConfValue
              new5t = replaceCVL old5t (c5t,c5t')
              old5t :: PG.ConfValue
              old5t = case L.lookup "RxC5TupleFilter" conf of
                        Just l -> l
                        Nothing -> error "Replace5t: Did not find RxC5TupleFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxC5TupleFilter") . fst) conf

    -- cf filters
    applyConfChange conf (InsertFDir (_,cFdir)) = ("RxCFDirFilter", newFdir):rest
        where newFdir :: PG.ConfValue
              newFdir = addToCVL oldFdir cFdir
              oldFdir :: PG.ConfValue
              oldFdir = case L.lookup "RxCFDirFilter" conf of
                        Just l -> l
                        Nothing -> error "addFDirToConf: Did not find RxCFDirFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxCFDirFilter") . fst) conf

    applyConfChange conf (CcReplace (InsertFDir (_,cFdir)) cFdir') =
       ("RxCFDirFilter", newFdir):rest
        where newFdir :: PG.ConfValue
              newFdir = replaceCVL oldFdir (cFdir, cFdir')
              oldFdir :: PG.ConfValue
              oldFdir = case L.lookup "RxCFDirFilter" conf of
                        Just l -> l
                        Nothing -> error "addFDirToConf: Did not find RxCFDirFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxCFDirFilter") . fst) conf

    -- SYN filter
    applyConfChange conf (InsertSYN cSyn) = ("RxCSynFilter", newSyn):rest
        where newSyn :: PG.ConfValue
              -- Overwriting old filter value with new one
              newSyn = overwriteCVL oldSyn cSyn
              oldSyn :: PG.ConfValue
              oldSyn = case L.lookup "RxCSynFilter" conf of
                        Just l -> l
                        Nothing -> error "addSynToConf: Did not find RxCSynFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxCSynFilter") . fst) conf

    -- incrmental configuration values: This is what gets passed to
    -- nIncrConfFunction
    incrConf (Insert5T c5t)     = [("RxC5TupleFilter", snd $ c5t)]
    incrConf (InsertFDir cFdir) = [("RxCFDirFilter", snd $ cFdir)]

    -- ARGH :(
    show = Prelude.show

    ccIsReplace (Insert5T (_,_))   = False
    ccIsReplace (InsertFDir (_,_)) = False
    ccIsReplace (InsertSYN _)      = False
    ccIsReplace (CcReplace _ _)    = True

    ccReplacedCc (CcReplace cc _)  = Just cc
    ccReplacedCc _ = Nothing

    ccReplaceNewCc (CcReplace (Insert5T (fid,oldval)) newval)   = Insert5T (fid,newval)
    ccReplaceNewCc (CcReplace (InsertFDir (fid,oldval)) newval) = InsertFDir (fid,newval)
    ccReplaceNewCc _ = error "ccReplaceNewCc undefined"

    ccReplace (CcReplace (Insert5T (_,oldCnf)) newCnf) nodes = case nodes of
        [(nid,oldLbl)] -> case parse5t_ newCnf of
                            Nothing  -> error $ "E10k: ccReplace: Insert5T: cannot parse:"++ (ppShow oldCnf)
                            Just c5t -> [Just $ nodeL5Tuple c5t]
        otherwise -> error $ "E10k: ccReplace: c5t: expecting single node, got:" ++ (ppShow nodes)

    ccReplace (CcReplace (InsertFDir (_,oldCnf)) newCnf) nodes = case nodes of
        [(nid,oldLbl)] -> [ Just $ nodeLFDir $ parseFDT newCnf ]
        otherwise -> error $ "E10k: ccReplace: fdir: expecting single node, got:" ++ (ppShow nodes)


cvMInt :: Integral a => Maybe a -> PG.ConfValue
cvMInt mi =  PG.CVMaybe $ (PG.CVInt . fromIntegral) <$> mi

ccQueue :: ConfChange -> QueueId
ccQueue (Insert5T (_,c5t)) = case c5t of
                             PG.CVTuple (_:_:_:_:_:_:(PG.CVInt qid):[]) -> (fromIntegral qid)
                             _ -> error $ "ccQueue: could not match c5t=" ++ (ppShow c5t)
ccQueue (InsertFDir (_,cFdir)) = case cFdir of
                             PG.CVTuple (_:_:_:_:_:(PG.CVInt qid):[]) -> (fromIntegral qid)
                             _ -> error "ccQueue: cfdir"

ccQueue (CcReplace (Insert5T (_,_)) c5t)
    = case c5t of
        PG.CVTuple (_:_:_:_:_:_:(PG.CVInt qid):[]) -> (fromIntegral qid)
        _ -> error $ "ccQueue: could not match replace c5t=" ++ (ppShow c5t)

ccQueue (CcReplace (InsertFDir (_,_)) cFdir)
    = case cFdir of
        PG.CVTuple (_:_:_:_:_:(PG.CVInt qid):[]) -> (fromIntegral qid)
        _ -> error "ccQueue: replacement cfdir"

--ccQueue (InsertSYN cSyn)

insert5tFromFl :: Int -> Flow -> QueueId -> ConfChange
insert5tFromFl fid fl qid = Insert5T (fid, (mk5TupleFromFl fl qid))

flowMatchesCc :: Flow -> ConfChange -> Bool
flowMatchesCc fl cc = ret
    where ccQ = ccQueue cc
          ret = case cc of
                  Insert5T   (_,c5t) -> (mk5TupleFromFl fl ccQ) == c5t
                  InsertFDir (_,cfd) -> case mkFDirFromFl fl ccQ of
                                                 Nothing -> False
                                                 Just x  -> x == cfd

replaceCcFromFl :: ConfChange -> Flow -> Maybe ConfChange
replaceCcFromFl cc@(Insert5T (_,_)) fl = Just $ CcReplace cc c5t'
    where c5t' = mk5TupleFromFl fl qid
          qid = ccQueue cc -- XXX: queue has to be the same for now
replaceCcFromFl cc@(InsertFDir (_,_)) fl =
    case mkFDirFromFl fl qid of
           Nothing ->  Nothing
           Just cfdir'  -> Just $ CcReplace cc cfdir'
 where qid = ccQueue cc -- XXX: queue has to be the same for now

mk5TupleFromFl :: Flow -> QueueId -> PG.ConfValue
mk5TupleFromFl fl@(FlowUDPv4 {}) q =
    PG.CVTuple [ cvMInt $ sIP,
                 cvMInt $ dIP,
                 PG.CVMaybe $ Just $ PG.CVEnum 1,
                 cvMInt $ sP,
                 cvMInt $ dP,
                 PG.CVInt prio,
                 PG.CVInt $ fromIntegral q]
    where
       sIP  = flSrcIp   fl
       dIP  = flDstIp   fl
       sP   = flSrcPort fl
       dP   = flDstPort fl
       prio = 1

insertFdirFromFl :: Int -> Flow -> QueueId -> Maybe ConfChange
insertFdirFromFl fid fl qid =
    case mkFDirFromFl fl qid of
        Nothing -> Nothing
        Just  x -> Just $ InsertFDir (fid, x)


-- TODO: we might want to use CVInt instead of CVMaybe for {src,dst}/{ip,port}
-- for these filters since they do not support wildcards
mkFDirFromFl :: Flow -> QueueId -> Maybe PG.ConfValue
mkFDirFromFl fl@(FlowUDPv4 { flSrcIp   = Just srcIp
                           , flDstIp   = Just dstIp
                           , flSrcPort = Just srcPort
                           , flDstPort = Just dstPort }) q =
    Just $ PG.CVTuple [ cvMInt $ Just $ fromIntegral srcIp,
                        cvMInt $ Just $ fromIntegral dstIp,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        cvMInt $ Just  $ fromIntegral srcPort,
                        cvMInt $ Just  $ fromIntegral dstPort,
                        PG.CVInt  $ fromIntegral q ]
-- flow director filters do not support wildcards
mkFDirFromFl (FlowUDPv4 {}) _ = Nothing




cfgStr :: C.Configuration -> String
cfgStr cnf_ = ret
    where  cnf :: [(String, PG.ConfValue)]
           cnf = cnf_
           ret = "CONF:\n" ++ L.intercalate "\n" (c5t ++ cfdt)
           c5t = case L.lookup "RxC5TupleFilter" cnf of
               Nothing -> []
               Just c  -> map (((++) " ") . c5tFullString)
                          $ parse5tCFG c
           cfdt = case L.lookup "RxCFDirFilter" cnf of
               Nothing -> []
               Just c  -> map (((++) " ") . cFDtFullString)
                          $ parseFDirCFG c


graphH_ :: FilePath -> IO (PG.PGraph, SEM.Helpers)
graphH_ fname = do
    (pg, helpers) <- parseGraph fname
    let pg' = prepareConf pg
    return (pg', helpers)

graphH :: IO (PG.PGraph,SEM.Helpers)
graphH = graphH_ "Graphs/E10k/prgE10kImpl.unicorn"

