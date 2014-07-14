{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module E10k (
    C5TL3Proto(..),
    C5TL4Proto(..),
    C5TPort,
    C5Tuple(..),
    CFDirTuple(..),

    configFDir,
    config5tuple
) where

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.Implementation.IPv4 as IP4

import Data.Word
import Data.Maybe
import qualified Data.List as L
import qualified Util.Misc as UM
import Control.Monad
import Data.Function (on)
import Data.Functor ((<$>))
import Data.String (fromString)

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTBV



type QueueID = Int


isRxQValidN :: Int -> PGNode -> Bool
isRxQValidN i (_,n) =
    (nLabel n == "Q" ++ show i ++ "Valid") ||
        (nLabel n == "RxQ" ++ show i ++ "Valid")

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
        tSems = foldl1 SMTC.and $ catMaybes [
                ipSems "src" <$> c5tL3Src c,
                ipSems "dst" <$> c5tL3Dst c,
                portSems "src" <$> c5tL4Src c,
                portSems "dst" <$> c5tL4Dst c
                ]
        fSems = SMTC.not tSems
        ipSems n i = SMTBV.bv (fromIntegral i) 32 SMTC.===
            SMT.app
                (fromString $ "IP4." ++ n)
                [SMT.app "pkt" []]
        portSems n i = SMTBV.bv (fromIntegral i) 16 SMTC.===
                    SMT.app
                        (fromString $ "UDP." ++ n)
                        [SMT.app "pkt" []]

config5tuple :: ConfFunction
config5tuple _ inE outE cfg = do
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
        cfgs = L.sortBy cmpPrio $ parse5tCFG cfg

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
configFDir _ inE outE cfg = do
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



