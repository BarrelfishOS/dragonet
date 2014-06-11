{-# LANGUAGE QuasiQuotes #-}
--module SFtx (
--    prg,
--    prgClusters,
--
--    C5TL3Proto(..),
--    C5TL4Proto(..),
--    C5TPort,
--    C5Tuple(..),
--    CFDirTuple(..),
--) where

import Dragonet.ProtocolGraph
import qualified Dragonet.ProtocolGraph as PG
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.Configuration as CONF
import Dragonet.Implementation.IPv4 as IP4

import qualified Dragonet.DotGenerator as DOT

import Data.Word
import Data.Maybe
import qualified Data.List as L
import qualified Util.Misc as UM
import Control.Monad



[unicorn|
graph prg {

    node HWDrop {}
    node ToMacBlock {
        attr "sink"
    }

    node TXQueue0 {
        attr "software"
        attr "source"
        port out[BuffMNGGetDesc]
    }

    node TXQueue1 {
        attr "software"
        attr "source"
        port out[BuffMNGGetDesc]
    }


    node TXQueue2 {
        attr "software"
        attr "source"
        port out[BuffMNGGetDesc]
    }


    cluster BuffMNG {
        node GetDesc {
            port out[configIsBufferMode]
        }

        config configIsBufferMode {
            port true[configGetBuffDesc]
            port false[configGetPhysicalDesc]
        }

        config configGetBuffDesc {
            // Gets the next buf descripor which can be used.
            // These values are configured by the driver.
            port true[ValidateBufferIndex]
            port false[.HWDrop]
        }

        boolean ValidateBufferIndex {
            port true[ValidateBufferOwnership]
            port false[.HWDrop]
        }

        boolean ValidateBufferOwnership {
            port true[PacketDMA]
            port false[.HWDrop]
        }

        config configGetPhysicalDesc {
            port true[PacketDMA]
            port false[.HWDrop]
        }

        node PacketDMA {
            port queueID[.TXPacerAddQueue]
        }
    } // end cluster: BuffMNG


    cluster TXPacer {

        node AddQueue {
            port queueID[ConfigBinSelector]
        }

        config ConfigBinSelector {
            port toWorkBin[WorkBin]
            port toFastBin[FastBin]
            port toSlowBin[SlowBin]
        }

        config WorkBin {
            port forward[ProcessQueue]
            port wait[]
        }

        config FastBin {
            port forward[ProcessQueue]
            port wait[]
        }

        config SlowBin {
            port forward[ProcessQueue]
            port wait[]
        }

        or ProcessQueue {
            port true[.FilterValidatePacket]
            port false[]
        }

    } // end cluster: TXPacer

    // This cluster can be also called as Validation
    cluster Filter {
        node ValidatePacket {
            port queueID[IsQueueETHFilter]
        }

        config IsQueueETHFilter {
            port true[ApplyETHFilter]
            port false[ClassifyL3]
        }

        node ApplyETHFilter {
            port out[IsIETHWildcardFilterMatch]
        }

        config IsIETHWildcardFilterMatch {
            port true[.OffloadEngine]
            port false[IsETHFullFilterMatch]
        }

        config IsETHFullFilterMatch {
            port true[.OffloadEngine]
            port false[ClassifyL3]
        }

        node ClassifyL3 {
            port ipv4[IsQueueIPv4Filter]
            //port ipv6[IsQueueNonIPv4Allowed]
            port ipv6[IsQueueNonIPv4DropDisabled]
            port other[.HWDrop]
        }

        config IsQueueIPv4Filter {
            port true[ApplyIPv4Filter]
            port false[.HWDrop]
        }

        node ApplyIPv4Filter {
            port out[IsIPv4WildcardFilterMatch]
        }

        config IsIPv4WildcardFilterMatch {
            port true[.OffloadEngine]
            port false[IsIPv4FullFilterMatch]
        }

        config IsIPv4FullFilterMatch {
            port true[.OffloadEngine]
            port false[.HWDrop]
        }

        // config IsQueueNonIPv4Allowed {
        config IsQueueNonIPv4DropDisabled {
            port true[.OffloadEngine] // send to offload engine
            port false[ClassifyL4]
        }

        node ClassifyL4 {
            port tcp[.HWDrop]
            port udp[.HWDrop]
            port other[.OffloadEngine]
        }

    } // end cluster: Filter

    cluster Offload {
        node Engine {
            port out[]
        }

        node ClassifyL3 {
            port ipv4[IsQueueIPv4ChecksumDisabled]
            port ipv6[ClassifyL4]
        }

        config IsQueueIPv4ChecksumDisabled {
            port true[ClassifyL4]
            port false[CalculateIPv4Checksum]
        }

        node CalculateIPv4Checksum {
            port out[ClassifyL4]
        }

        node ClassifyL4 {
            port TcpUdp[IsQueueTcpUdpChecksumDisabled]
            port iscsi[IsQueueISCSIDigestEnabled]
            port other[.ToMacBlock]
        }

        config IsQueueISCSIDigestEnabled {
            port true[CalculateISCSIDigest]
            port false[.ToMacBlock]
        }

        node CalculateISCSIDigest {
            port out[.ToMacBlock]
        }

        config IsQueueTcpUdpChecksumDisabled {
            port true[.ToMacBlock]
            port false[ClassifyTcpUdp]
        }

        node ClassifyTcpUdp {
            port tcp[CalculateTCPChecksum]
            port udp[CalculateUDPChecksum]
        }

        node CalculateTCPChecksum {
            port out[.ToMacBlock]
        }

        node CalculateUDPChecksum {
            port out[.ToMacBlock]
        }

    } // end cluster: Offload

} // end PRG: SFtx
|]

type QueueID = Int


-------------------------------------------------------------------------------
-- Implementation of configuration of 5-tuple filters

data C5TL3Proto = C5TPL3IPv4 | C5TPL3IPv6
    deriving (Eq)
instance Show C5TL3Proto where
    show C5TPL3IPv4 = "IPv4"
    show C5TPL3IPv6 = "IPv6"

data C5TL4Proto = C5TPL4TCP | C5TPL4UDP
    deriving (Eq)
instance Show C5TL4Proto where
    show C5TPL4TCP = "TCP"
    show C5TPL4UDP = "UDP"

type C5TPort = Word16


data C5Tuple = C5Tuple {
    c5tPriority :: Int,
    c5tQueue    :: QueueID,
    c5tL3Proto  :: Maybe C5TL3Proto,
    c5tL4Proto  :: Maybe C5TL4Proto,
    c5tL3Src    :: Maybe String,
    c5tL3Dst    :: Maybe String,
    c5tL4Src    :: Maybe C5TPort,
    c5tL4Dst    :: Maybe C5TPort
} deriving (Eq,Show)

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

configEtherFilter :: ConfFunction
configEtherFilter _ inE outE cfg = do
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
    cfdtQueue    :: QueueID,
    cfdtL3Proto  :: C5TL3Proto,
    cfdtL4Proto  :: C5TL4Proto,
    cfdtL3Src    :: String,
    cfdtL3Dst    :: String,
    cfdtL4Src    :: C5TPort,
    cfdtL4Dst    :: C5TPort
} deriving (Eq,Show)

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


getGraphDirName :: String
getGraphDirName = "./graphsGen/"

myWriteFile :: String -> String -> IO()
myWriteFile fnamefile contents = do
    let fname = (getGraphDirName  ++ fnamefile)
    putStrLn ("Generating " ++ fname ++ " files...")
    writeFile fname contents



main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    myWriteFile "prgSFtx.dot" $ DOT.toDot prg
    myWriteFile "prgSFtxClustered.dot" $ DOT.toDotClustered prg prgClusters
--    myWriteFile "prgSFtxConf.dot" $ DOT.toDot prgTConf
    where
        prgTConf = PG.pgSetType PG.GTPrg prgConfigured
        prgConfigured = CONF.applyConfig config prg
        config = []

