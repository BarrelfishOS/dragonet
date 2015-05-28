{-# LANGUAGE OverloadedStrings #-}
module Graphs.LPG (
    lpgConfig,
    graphH, graphH_
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Configuration as C
import qualified Dragonet.Semantics as SEM

import Dragonet.NetState (EndpointDesc(..))

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTBV

import Control.Monad (forM)
import Data.Functor ((<$>))
import qualified Data.List as L
import Data.Maybe
import Dragonet.Implementation.IPv4 as IP4
import Data.String (fromString)
import Util.Misc (mapT2)

import Graphs.Helpers


-- generates the necessary LPG configuration based on the limited number of
--      endpoints in socket state
lpgConfig :: [EndpointDesc] -> C.Configuration
lpgConfig eps = [("RxL4UDPCUDPSockets", PG.CVList cUdpSockets)]
    where
        cUdpSockets = map (PG.CVTuple . cUdpSocket) eps
        cUdpSocket ep = [ PG.CVList $ map buildSock socks,
                          PG.CVMaybe rIP,
                          PG.CVMaybe rPort,
                          PG.CVMaybe lIP,
                          PG.CVMaybe lPort]
            where
                socks = epSockets ep
                rIP = PG.CVInt <$> fromIntegral <$> epRemoteIp ep
                lIP = PG.CVInt <$> fromIntegral <$> epLocalIp ep
                rPort = PG.CVInt <$> fromIntegral <$> epRemotePort ep
                lPort = PG.CVInt <$> fromIntegral <$> epLocalPort ep
        buildSock sock@(sid,aid) = PG.CVTuple [PG.CVInt $ fromIntegral sid,
                                   PG.CVInt $ fromIntegral aid]


configLPGUDPSockets :: PG.ConfFunction
configLPGUDPSockets _ _ inE outE cfg = concat <$> mapM addEndpoint
                                                       (zip [1..] epTuples)
    where
        esa_attrs = [PG.ESAttrPredicate "and(pred(EthType,IPv4),pred(IpProt,UDP))"] -- spawn edge attributes
        PG.CVList epTuples = cfg
        hasL l n = l == PG.nLabel n
        findN ps l = (fst . fst) <$> L.find (hasL l . snd . fst) ps
        Just vN = findN inE "RxL4UDPValid"
        --Just vhN = findN inE "RxL4UDPValidHeaderLength"
        -- ugly hack to support both lpg versions
        irN = case findN outE "TxL4UDPStart" of
            Just x  -> x
            Nothing -> fromJust $ findN outE "TxL4UDPInitiateResponse"
        Just cN  = findN outE "RxL4UDPUnusedPort"

        socketId (PG.CVTuple [PG.CVInt i, _]) = i
        socketAppId (PG.CVTuple [_, PG.CVInt i]) = i

        addSocket (inN,inP) outN socket = do
            let sid = socketId socket
            let appAttr = PG.NAttrCustom $ "appid=" ++ show (socketAppId socket)
            -- Create ToSocket Node
            (tsN,_) <- C.confMNewNode $
                       PG.nAttrAdd appAttr $ PGU.toSocketNode sid
            -- Create FromSocket Node
            (fsN,_) <- C.confMNewNode $
                       PG.nAttrAdd appAttr $ PGU.fromSocketNode sid
            return [(inN,tsN,PG.Edge inP),
                    (fsN,fsN,PG.ESpawn "send" esa_attrs),
                    (fsN,outN,PG.Edge "true")]

        addEndpoint (eid,(PG.CVTuple [
                          PG.CVList sockets,
                          PG.CVMaybe msIP,
                          PG.CVMaybe msPort,
                          PG.CVMaybe mdIP,
                          PG.CVMaybe mdPort])) = do
            -- Filter/demultiplexing node
            (dxN,_) <- C.confMNewNode $ addFAttrs $ addFSemantics $
                        PG.baseFNode filterS bports
            -- Add socket nodes
            sEdges <- case sockets of
                [] -> error "Endpoint without sockets"
                [sock] -> addSocket (dxN,"true") irN sock
                _ -> do
                    let sports = map (show . socketId) sockets
                    -- If we have more than one socket, add balance node
                    (bN,_) <- C.confMNewNode
                              $ PGU.balanceNodeEndpoint filterS eid sports
                    -- Add sockets
                    es <- forM sockets $ \s ->
                        addSocket (bN,show $ socketId s) irN s
                    return $ concat es ++ [(dxN,bN,PG.Edge "true")]
            let dfEdges = map (\(a,b,p) -> (a,b,PG.Edge p)) [
                    (vN,dxN,"true"),  -- RxL4UDPValid -> Filter
                    (dxN,cN,"false"),  -- Filter -> Collect
                    (dxN,cN,"true")]   -- Filter -> Collect
            return $ dfEdges ++ sEdges
            where
                bports = ["false","true"]
                uncvi (PG.CVInt i) = fromIntegral i
                -- Parse IP and Ports
                (msIP',mdIP') = mapT2 (uncvi <$>) (msIP, mdIP)
                (msP,mdP) = mapT2 (uncvi <$>) (msPort, mdPort)
                -- Build Label
                mbPort = maybe "*" show
                mbIP = maybe "*" (IP4.ipToString . fromIntegral)
                filterS = "RxL4UDP(" ++ mbIP msIP' ++ ":" ++ mbPort msP
                            ++ " / " ++ mbIP mdIP' ++ ":" ++ mbPort mdP ++ ")"

                -- Build filter Semantics
                ipSems n i = SMTBV.bv i 32 SMTC.===
                            SMT.app
                                (fromString $ "IP4." ++ n)
                                [SMT.app "pkt" []]
                portSems n i = SMTBV.bv i 16 SMTC.===
                            SMT.app
                                (fromString $ "UDP." ++ n)
                                [SMT.app "pkt" []]
                --   true port semantics
                tSems = foldl1 SMTC.and $ catMaybes [
                        ipSems "src" <$> msIP',
                        portSems "src" <$> msP,
                        ipSems "dst" <$> mdIP',
                        portSems "dst" <$> mdP]
                --   false port semantics
                fSems = SMTC.not tSems
                -- Add semantics to filter node
                addFSemantics n = n {
                    PG.nSemantics = [("true",tSems),("false",fSems) ] }
                -- Add attributes to filter node
                fAttrs = map PG.NAttrCustom $ catMaybes [
                    Just "udpdemux",
                    do { i <- msIP' ; return $ "srcip=" ++ show i },
                    do { i <- mdIP' ; return $ "dstip=" ++ show i },
                    do { i <- msP ; return $ "srcport=" ++ show i },
                    do { i <- mdP ; return $ "dstport=" ++ show i }]
                addFAttrs n = PG.nAttrsAdd fAttrs n


addCfgFun n
    | l == "RxL4UDPCUDPSockets" = configLPGUDPSockets
    | otherwise = error $ "Unknown LPG CNode: '" ++ l ++ "'"
    where l = PG.nLabel n

graphH_ :: FilePath -> IO (PG.PGraph, SEM.Helpers)
graphH_ fname = do
    (pg,helpers) <- parseGraph fname
    let pg' = C.replaceConfFunctions addCfgFun pg
    return (pg',helpers)

graphH :: IO (PG.PGraph,SEM.Helpers)
graphH = graphH_ "Graphs/LPG/lpgConfImpl.unicorn"
