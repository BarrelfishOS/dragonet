-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Scenarios.S3 (
    priorityCost'
    , sortedRealFlows
    , realFlows
    , sortFlows
    , hardcodedOracleMemcachedSF
    , hardcodedOracleMemcachedIntel
) where

import qualified Data.Word as DW

import Dragonet.Flows(Flow (..), flowPred)
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Search as S
import qualified Dragonet.Configuration as C
import qualified Graphs.E10k as E10k
import qualified Graphs.E10k as SF
import qualified Data.Maybe  as DM
import qualified Data.List as DL
import qualified Dragonet.ProtocolGraph       as PG

import Control.Applicative ((<$>))

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)



--fs2 = [ FlowUDPv4 {
--     flDstIp    = Just 127
--   , flDstPort  = Just $ fromIntegral $ 7777
--   , flSrcIp    = IP4.ipFromString "10.113.4.51"
--   , flSrcPort  = Just $ fromIntegral $ 8000 + i } | i <- [0..30] ]
--
---- These two should be the gold flows
---- ziger1 : 10.113.4.51:8000
---- ziger2 : 10.113.4.57:8000

myFromMaybe (Just x) = x
myFromMaybe _ = error "No IP address"

--hpm1 = "10.113.4.51"  -- ziger1
--hpm2 = "10.113.4.57"  -- ziger2


hpm1 = "10.113.4.26"  -- sbrinz1
hpm2 = "10.113.4.29"  -- sbrinz2

goldFlPerQ = 1
isGoldFl fPerApp FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip} = ans
    where
        ans
            | (sport >= 8000) && (sport < (8000 + (fromIntegral fPerApp))) && (sip == (myFromMaybe $ IP4.ipFromString hpm1)) =  True
            | (sport >= 8000) && (sport < (8000 + (fromIntegral fPerApp))) && (sip == (myFromMaybe $ IP4.ipFromString hpm2)) =  True
            | otherwise = False
isGoldFl _ _ = False


sortFlows isImp fl = sortedFlows
    where
        allFlows = fl
        impList = filter isImp $ allFlows
        otherList = filter (not . isImp) allFlows
        sortedFlows = impList ++  otherList

sortedRealFlows serverIP fpApp clients = sortFlows (isGoldFl fpApp) (realFlows serverIP fpApp clients)

generateFlows dstIP dstPort srcIP_list start_srcP conc = ff
    where
    clients :: [DW.Word32]
    clients = srcIP_list
    startPort :: DW.Word16
    startPort = start_srcP
    cllist = take (conc * length clients) $ concat $ repeat clients
--    f :: [(DW.Word32, DW.Word16)]
    f = map (\x-> (cllist!!x, ((toInteger startPort) + toInteger (length $ filter ( == cllist!!x)  $ take x cllist))))
        [0..length cllist - 1]
    ff = map (\(x,y) -> FlowUDPv4 {
             flDstIp    = Just dstIP
           , flDstPort  = Just dstPort
           , flSrcIp    = Just  x
           , flSrcPort  = Just (fromIntegral y) }) f

getClientList_single clcount =  take clcount $ concat $ repeat clList
    where
    clList = [
          "10.113.4.57"  --  ziger2:
--          , "10.113.4.51"  --  ziger1:
--          , "10.113.4.26"  --  sbrinz1:
--          , "10.113.4.29"  --  sbrinz2:
--          , "10.113.4.20"  --  gruyere:
--          , "10.113.4.71"  --  appenzeller:
      ]


getClientList_real clcount =  take clcount $ concat $ repeat clList
    where
    clList = [
          "10.113.4.51"  --  ziger1:
          , "10.113.4.57"  --  ziger2:
          , "10.113.4.26"  --  sbrinz1:
          , "10.113.4.29"  --  sbrinz2:
--          , "10.113.4.20"  --  gruyere:
--          , "10.113.4.71"  --  appenzeller:
      ]

getClientList clcount = getClientList_real clcount

--defaultQueue = [0]
defaultQueue = []

asiagoSF_server = DM.fromJust $ IP4.ipFromString
                "10.113.4.195"   -- Using asiago SF as server

asiagoIntel_server = DM.fromJust $ IP4.ipFromString
                "10.113.4.95"   -- Using asiago Intel as server

generateFlowsWrapper serverIP fperApp clcount = generateFlows serverIP srvPort  clients_ip_addr clientStartPort fperApp
    where
    srvPort = 7777
    clients_ip_addr = map ( DM.fromJust . IP4.ipFromString ) $ getClientList clcount
    clientStartPort = 8000

-- FIXME: This code is repeated with  allFlowsForConfigPrority
--                                       flow, queue-id, filtertype
flowsToQueue :: [Flow] -> Int -> Int -> [(Flow, Int, Int)]
flowsToQueue flist fpApp nq =  glFLows1  ++  glFLows2 ++ otherQmap'
    where
    pflows = DL.sort $ filter (isGoldFl fpApp) flist
    halfway = ((length pflows) `div` 2)
    glFLows1 = map (\x -> (x, 1, 1) ) $ take halfway pflows
    glFLows2 = map (\x -> (x, 2, 1) ) $ drop halfway pflows

    npflows = DL.sort $ filter (not . (isGoldFl fpApp)) flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([3..nq] ++ defaultQueue))
    otherQmap' = map (\(a, b) -> (a, b, 1)) otherQmap

-- FIXME: This code is repeated with allFlowsForConfigBalance
--                                       flow, queue-id, filtertype
flowsToQueueBalance :: [Flow] -> Int -> Int -> [(Flow, Int, Int)]
flowsToQueueBalance flist fpApp nq =  otherQmap'
    where
    npflows = flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([1..nq] ++ defaultQueue))
    otherQmap' = map (\(a, b) -> (a, b, 1)) otherQmap

allFlowsForConfigOld flist fpApp nq = allFlows
    where
    allFlows = glFLows1  ++  glFLows2 ++ otherQmap'
    toFilterType = E10k.mkFDirFromFl
    pflows = DL.sort $ filter (isGoldFl fpApp) flist
    halfway = ((length pflows) `div` 2)
    glFLows1 = map (\x -> toFilterType x 1) $ take halfway pflows
    glFLows2 = map (\x -> toFilterType x 2) $ drop halfway pflows

    npflows = DL.sort $ filter (not . (isGoldFl fpApp)) flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([3..nq] ++ defaultQueue))
    otherQmap' = map (\(a, b) -> toFilterType a b) otherQmap

allFlowsForConfigPrority flist fpApp nq = allFlows
    where
    allFlows = glFLows1  ++  glFLows2 ++ otherQmap
    pflows = DL.sort $ filter (isGoldFl fpApp) flist
    halfway = ((length pflows) `div` 2)
    glFLows1 = map (\x -> (x, 1)) $ take halfway pflows
    glFLows2 = map (\x -> (x, 2)) $ drop halfway pflows

    npflows = DL.sort $ filter (not . (isGoldFl fpApp)) flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([3..nq] ++ defaultQueue))

allFlowsForConfigBalance flist fpApp nq = allFlows
    where
    allFlows = otherQmap
    npflows =  flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([1..nq] ++ defaultQueue))


flowsToConfigIntel :: [(Flow, Int)] -> Int -> Int -> C.Configuration
flowsToConfigIntel flist fpApp nq =  [
        ("RxC5TupleFilter", PG.CVList t5confChanges),
        ("RxCFDirFilter", PG.CVList fdirflowsChanges)
    ]
    where
    --allflows = allFlowsForConfig flist fpApp nq
    allflows = flist
    t5flows = take 100  allflows
    fdirflows = drop 100  allflows
    t5confChanges = map (\(a, b) -> DM.fromJust $ E10k.mkFDirFromFl a b) t5flows
    fdirflowsChanges = map (\(a, b) -> DM.fromJust $ E10k.mkFDirFromFl a b) fdirflows


flowsToConfigSF :: [(Flow, Int)] -> Int -> Int -> C.Configuration
flowsToConfigSF flist fpApp nq =  [
        ("RxC5TupleFilter", PG.CVList allflowsChanges)
        --("RxCFDirFilter", PG.CVList allFlowsFdir)
    ]
    where
    --allflows = allFlowsForConfig flist fpApp nq
    allflows = flist
    --allflowsChanges = map (\(a, b) -> DM.fromJust $ SF.mk5TupleFromFl a b)  allflows
    allflowsChanges = map (\(a, b) -> SF.mk5TupleFromFl a b)  allflows

-- All flows are:
realFlows serverIP fpApp clients = generateFlowsWrapper serverIP fpApp clients

priorityCost' fpApp  = S.priorityCost (isGoldFl fpApp) goldFlPerQ

e10kT = E10k.graphH
e10kU = fst <$> e10kT

hardcodedOracleMemcachedIntel fpApp clients nq costfn prgU =
    hardcodedOracleMemcached' oracle serverIP cnf0 fpApp clients nq  costfn prgU
    where
        flowMapperFns = [("balance", allFlowsForConfigBalance),
                         ("priority", allFlowsForConfigPrority)]
        flowMapperFn    = case lookup costfn flowMapperFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        flows = sortedRealFlows serverIP fpApp clients
        flowsMap = flowMapperFn flows fpApp nq
        serverIP = asiagoIntel_server

        cnf0 = flowsToConfigIntel flowsMap fpApp nq
        --oracle   = S.E10kOracleHardCoded {S.nnHardcoded = (nq, qmap)}
        oracle a   = S.E10kOracleHardCoded {S.nnHardcoded = (nq, a)}
        --oracle   = S.E10kOracleSt {S.nQueues = nq}


hardcodedOracleMemcachedSF fpApp clients nq costfn prgU =
    hardcodedOracleMemcached' oracle serverIP cnf0 fpApp clients nq  costfn prgU
    where

        flowMapperFns = [("balance", allFlowsForConfigBalance),
                         ("priority", allFlowsForConfigPrority)]
        flowMapperFn    = case lookup costfn flowMapperFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        flows = sortedRealFlows serverIP fpApp clients
        flowsMap = flowMapperFn flows fpApp nq
        serverIP = asiagoSF_server

        --cnf0 = SF.cfgEmpty
        --cnf0 = flowsToConfig (sortedRealFlows serverIP fpApp clients) fpApp nq
        cnf0 = flowsToConfigSF flowsMap fpApp nq

        oracle a   = S.SFOracleHardCoded {S.nnHardcodedSF = (nq, a)}
        --oracle   = S.SFOracleSt {S.nQueuesSF = nq}


hardcodedOracleMemcached' oracle' serverIP cnf0 fpApp clients nq  costfn prgU = params
    where

        flowMapperFns = [("balance", flowsToQueueBalance), ("priority", flowsToQueue)]
        flowMapperFn    = case lookup costfn flowMapperFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn

        qmap = flowMapperFn (sortedRealFlows serverIP fpApp clients) fpApp nq
        oracle = oracle' qmap

        priFn        = (priorityCost' fpApp) nq
        balFn        = S.balanceCost nq
        costFns    = [("balance", balFn), ("priority", priFn)]
        costFn     = case lookup costfn costFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn

        --searchFNGreedy        = S.searchGreedyFlows

        searchFNHardcoded      = S.hardcodedSearch cnf0

        params = S.initSearchParams {  S.sOracle = oracle
                                   , S.sPrgU   = prgU
                                   , S.sCostFn = priFn
                                   , S.sStrategy =  searchFNHardcoded
                                   }



test = do
    -- unconfigured PRG
    prgU <- e10kU
    --prgU <- e10kU_simple
    let nq = 5
        serverIP = asiagoSF_server
        fpApp = 2 --16
        clients = 20
        qmap_for_debug = flowsToQueue (sortedRealFlows serverIP fpApp clients) fpApp nq

        params = hardcodedOracleMemcachedIntel fpApp clients nq "priority" prgU

        conflows = sortedRealFlows serverIP fpApp clients
        conf = S.runSearch params  conflows
        prgC = C.applyConfig conf prgU

    --putStrLn $ "connected flows : " ++ (ppShow conflows)
    mapM_ (\x -> putStrLn $ show x) $ qmap_for_debug
    putStrLn $ ("done!: " ++ (show $ length qmap_for_debug))

    --mapM_ (\x -> putStrLn $ show x) $ conf
    --putStrLn $ show $ map (\(_,x) -> ppShow x) $ conf

    mapM_ (\(_,x) -> putStrLn $ ppShow x) $ conf
    putStrLn $ ("done!: " ++ (show $ length $ conf))

    --putStrLn $ "conf is : " ++ (ppShow conf)
    --putStrLn $ "Configuration: " ++ (S.showConf oracle conf)

