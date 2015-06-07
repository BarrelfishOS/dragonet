-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Graphs.Null (
    C5TL3Proto(..),
    C5TL4Proto(..),
    C5TPort,
    C5Tuple(..),
    CFDirTuple(..),
    prepareConf,
    parse5tCFG, c5tString, c5tFullString,
    parseFDirCFG, cFDtString,

    flowQueue,
    graphH
) where


import Dragonet.ProtocolGraph
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Semantics as SEM
import Dragonet.Flows (Flow (..))

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
import qualified Util.GraphHelpers as GH

import Control.Exception (assert)
import Text.Show.Pretty (ppShow)


type QueueID = Int


isRxQValidN :: Int -> PGNode -> Bool
isRxQValidN i (_,n) =
    (nLabel n == "Q" ++ show i ++ "Valid") ||
        (nLabel n == "RxQ" ++ show i ++ "Valid")

addCfgFun :: Node -> ConfFunction
addCfgFun n
    | l == "RxC5TupleFilter" = config5tuple
    | l == "RxCFDirFilter"   = configFDir
    | l == "RxQueues" || l == "TxQueues" = configQueues -- not a real configuration, but helpful for building PRGs
    | otherwise = error $ "Unknown LPG CNode: '" ++ l ++ "'"
    where l = nLabel n


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
    c5tQueue    :: QueueID,
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

strToC5t :: String -> C5Tuple
strToC5t str = assert (check1 && check2) ret
    where check1 = (take 3 str) == "5T("
          check2 = (L.last str) == ')'
          len = length str
          x = take (len -3 -1) $ drop 3 str
          xl = splitBy (==',') x
          ret = C5Tuple {
                  -- we do not care about the priority and the queue, this
                  -- infromation should be encoded in the grap
                  c5tPriority  = 999 -- the priority should be encoded in the graph
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

c5tAttr :: C5Tuple -> [NAttribute]
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
        aTrue = NAttrCustom $ "C.true:" ++ cT
        aFalse = NAttrCustom $ "C.false:!(" ++ cT ++ ")"
        showIP = IP4.ipToString

parse5tCFG :: ConfValue -> [C5Tuple]
parse5tCFG (CVList l) = map parse5t l

parse5t :: ConfValue -> C5Tuple
parse5t (CVTuple
           [CVMaybe mSIP,
            CVMaybe mDIP,
            CVMaybe mProto,
            CVMaybe mSPort,
            CVMaybe mDPort,
            CVInt prio,
            CVInt queue]) =
    C5Tuple {
        c5tPriority = fromIntegral $ prio,
        c5tQueue = fromIntegral $ queue,
        c5tL4Proto = convProto <$> mProto,
        c5tL3Src = convInt <$> mSIP,
        c5tL3Dst = convInt <$> mDIP,
        c5tL4Src = convInt <$> mSPort,
        c5tL4Dst = convInt <$> mDPort
    }
    where
        convProto (CVEnum 0) = C5TPL4TCP
        convProto (CVEnum 1) = C5TPL4UDP
        convProto (CVEnum 2) = C5TPL4SCTP
        convProto (CVEnum 3) = C5TPL4Other
        convInt (CVInt i) = fromIntegral i



nodeL5Tuple c = (baseFNode (c5tString c) bports) {
                    nAttributes = c5tAttr c,
                    nSemantics = [("true",tSems),("false",fSems)]
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

config5tuple :: ConfFunction
config5tuple _ _ inE outE cfg = do
    ((endN,endP),edges) <- foldM addFilter (start,[]) cfgs
    let lastEdge = (endN,defaultN,endP)
    return (edges ++ [lastEdge])
    where
        -- Node and Port for the incoming edge for the first node
        start = (fst $ fst $ head inE, snd $ head inE)
        -- Node for default queue
        (Just ((defaultN,_),_)) = L.find ((== "default") . ePort . snd) outE
        -- Lookup node id for specified queue
        queue i = queueN
            where (Just ((queueN,_),_)) = L.find (isRxQValidN i . fst) outE

        -- Get filter configurations ordered by priority
        cmpPrio = compare `on` c5tPriority
        cfgs = reverse $ L.sortBy cmpPrio $ parse5tCFG cfg

        -- Generate node and edges for one filter
        addFilter ((iN,iE),es) c = do
            (n,_) <- confMNewNode $ nodeL5Tuple c
            let inEdge = (iN,n,iE)
            let tEdge = (n,queue $ c5tQueue c,Edge "true")
            let fEdge = (n,queue $ c5tQueue c,Edge "false")
            return ((n,Edge "false"), es ++ [inEdge,tEdge,fEdge])



-------------------------------------------------------------------------------
-- Implementation of configuration of the flow director filters


data CFDirTuple = CFDirTuple {
    cfdtQueue    :: QueueID,
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

cFDtAttr :: CFDirTuple -> [NAttribute]
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
        aTrue = NAttrCustom $ "C.true:" ++ cT
        aFalse = NAttrCustom $ "C.false:!(" ++ cT ++ ")"

parseFDirCFG :: ConfValue -> [CFDirTuple]
parseFDirCFG (CVList l) = map parseFDT l

parseFDT :: ConfValue -> CFDirTuple
parseFDT (CVTuple
           [sIP,
            dIP,
            proto,
            sPort,
            dPort,
            CVInt queue]) =
    CFDirTuple {
        cfdtQueue = fromIntegral queue,
        cfdtL4Proto = convProto proto,
        cfdtL3Src = convInt sIP,
        cfdtL3Dst = convInt dIP,
        cfdtL4Src = convInt sPort,
        cfdtL4Dst = convInt dPort
    }
    where
        convProto (CVEnum 0) = C5TPL4TCP
        convProto (CVEnum 1) = C5TPL4UDP
        convProto (CVEnum 2) = C5TPL4SCTP
        convProto (CVEnum 3) = C5TPL4Other
        convInt (CVInt i) = fromIntegral i

configFDir :: ConfFunction
configFDir _ _ inE outE cfg = do
    ((endN,endP),edges) <- foldM addFilter (start,[]) cfgs
    let lastEdge = (endN,defaultN,endP)
    return (edges ++ [lastEdge])
    where
        -- Node and Port for the incoming edge for the first node
        start = (fst $ fst $ head inE, snd $ head inE)
        -- Node for default queue
        (Just ((defaultN,_),_)) = L.find ((== "default") . ePort . snd) outE
        -- Lookup node id for specified queue
        queue i = queueN
            where (Just ((queueN,_),_)) = L.find (isRxQValidN i . fst) outE

        -- Get filter configurations
        cfgs = parseFDirCFG cfg

        -- Generate node and edges for one filter
        bports = ["true","false"]
        nodeL c = (baseFNode (cFDtString c) bports) {
                    nAttributes = cFDtAttr c
                    }
        addFilter ((iN,iE),es) c = do
            (n,_) <- confMNewNode $ nodeL c
            let inEdge = (iN,n,iE)
            let tEdge = (n,queue $ cfdtQueue c,Edge "true")
            let fEdge = (n,queue $ cfdtQueue c,Edge "false")
            return ((n,Edge "false"), es ++ [inEdge,tEdge,fEdge])

configQueues :: ConfFunction
configQueues _ (_,cfgn) inE outE (CVInt qs) = do
    ret <- foldM addNode [] [1..qs]
    return ret
    where addNode prev n = do
            let name = case (nLabel cfgn) of
                         "RxQueues" -> "RxQueue" ++ (show n)
                         "TxQueues" -> "TxQueue" ++ (show n)
                attrs = nAttributes cfgn
                ports = nPorts cfgn
                node = (baseFNode name []) { nAttributes = attrs
                                           , nPorts = ports }

            (nid, _) <- confMNewNode node

            let edge_to_self :: (PGNode, Edge) -> Bool
                edge_to_self ((_, CNode { nLabel = xlbl }), _) = xlbl == nLabel cfgn
                edge_to_self _ = False
                self :: [Edge]
                self_in  = L.sort $ [ e | (_, e) <- filter edge_to_self inE]
                self_out = L.sort $ [ e | (_, e) <- filter edge_to_self outE]
                self = case (self_in == self_out) of
                    True -> self_in
                    False -> error "incomming and outgoing self edges do not match"
                iE  = [ (fst x, nid, e)  | (x, e) <- filter (not . edge_to_self) inE]
                oE  = [ (nid, fst x, e)  | (x, e) <- filter (not . edge_to_self) outE]
                sE  = [ (nid, nid, e)    | e <- self ]
            return $ prev ++ iE ++ oE ++ sE


prepareConf :: PGraph -> PGraph
prepareConf = replaceConfFunctions addCfgFun

----
-- Try to figure out at  which queue a flow will end up.
-- Very quick-n-dirty for the moment
--

maybeMatch Nothing _ = True
maybeMatch _ Nothing = True
maybeMatch (Just x1) (Just x2) = x1 == x2

flowMatches5TF :: Flow -> C5Tuple -> Bool
flowMatches5TF (FlowUDPv4 {flSrcIp = srcIp,
                           flDstIp = dstIp,
                           flSrcPort = srcPort,
                           flDstPort = dstPort })
                (C5Tuple {c5tL4Proto = cProt,
                          c5tL3Src   = cSrcIp,
                          c5tL3Dst   = cDstIp,
                          c5tL4Src   = cSrcPort,
                          c5tL4Dst   = cDstPort}) = ret
 where maybeT = maybe True
       ip_match Nothing _ = True
       ip_match _ Nothing = True
       ip_match (Just ip1) (Just ip2) = ip1 == ip2
       ret =   maybeMatch srcIp cSrcIp
            && maybeMatch dstIp cDstIp
            && maybeMatch srcPort cSrcPort
            && maybeMatch dstPort cDstPort

flowGetPort :: Flow -> PG.PGNode -> PG.NPort
flowGetPort fl (_, nlbl)
    | (take 2 name) == "5T" = case flowMatches5TF fl (strToC5t name) of
                                   True  -> "true"
                                   False -> "false"
    | otherwise = error $ "flowGetPort:"  ++ (PG.nLabel nlbl)
    where name = nLabel nlbl

reachedQueue :: PG.PGNode -> Maybe QueueID
reachedQueue (_,PG.ONode {PG.nLabel = name}) = ret
    where n = filter isDigit name
          ret = case length n of
                  0 -> Nothing
                  _ -> case "RxQ" ++ n ++ "Valid" == name of
                            True  -> Just $ read n
                            False -> Nothing
reachedQueue (_,PG.FNode {PG.nLabel = name})
    | name == "RxToDefaultQueue" =  Just 0
    | otherwise  = Nothing

doFlowQueue :: PG.PGraph -> PG.PGNode -> Flow -> QueueID
doFlowQueue g node fl
    | Just q <- reachedQueue node = q
    | otherwise = ret
    where ret = doFlowQueue g next fl
          port = flowGetPort fl node
          isOnode (PG.ONode {}) = True
          isOnode _             = False
          next = case [ n | (n,e) <- PGU.edgeSucc g node,
                        PGU.edgePort e == port,
                        port == "true" || (not $ isOnode $ snd n)] of
                   [x] -> x
                   l  -> error $ "More than one connection matches for node:"
                                 ++ (PG.nLabel . snd) node ++ "port:" ++ port
                                 ++ (ppShow l)

flowQueue :: PG.PGraph -> Flow -> QueueID
flowQueue prgC flow = ret
    where ret = doFlowQueue prgC node1 flow
          node0_ = "RxL2EtherClassifyL3_"
          port0 = "other"
          node0 = case GH.filterNodesByL (\x -> (PG.nLabel x) == node0_) prgC of
            [x] -> x
            _   -> error $ "More than one matches for node:" ++ node0_
          node1 = case [ n | (n,e) <- PGU.edgeSucc prgC node0,
                          PGU.edgePort e == port0] of
            [x] -> x
            _   -> error $ "More than one connection matches for node:"
                           ++ node0_ ++ " port:" ++ port0

graphH :: IO (PGraph,SEM.Helpers)
graphH = do
    (pg,helpers) <- parseGraph "Graphs/Null/prgNullImpl.unicorn"
    let pg' = prepareConf pg
    return (pg',helpers)


