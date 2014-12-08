module Scenarios.S3 (
    priorityCost'
    , sortedRealFlows
    , realFlows
    , sortFlows
    , hardcodedOracleMemcached
) where

import qualified Data.Word as DW

import Dragonet.Flows(Flow (..), flowPred)
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Search as S
import qualified Dragonet.Configuration as C
import qualified Graphs.E10k as E10k
import qualified Data.Maybe  as DM
import qualified Data.List as DL

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


goldFlPerQ = 1
isGoldFl fPerApp FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip} = ans
    where
        ans
            | (sport >= 8000) && (sport < (8000 + (fromIntegral fPerApp))) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.51")) =  True
            | (sport >= 8000) && (sport < (8000 + (fromIntegral fPerApp))) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.57")) =  True
            | otherwise = False
isGoldFl _ _ = False


sortFlows isImp fl = sortedFlows
    where
        allFlows = fl
        impList = filter isImp $ allFlows
        otherList = filter (not . isImp) allFlows
        sortedFlows = impList ++  otherList

sortedRealFlows fpApp clients = sortFlows (isGoldFl fpApp) (realFlows fpApp clients)

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


getClientList clcount =  take clcount $ concat $ repeat clList
    where
    clList = [
          "10.113.4.51"  --  ziger1:
          , "10.113.4.57"  --  ziger2:
          , "10.113.4.26"  --  sbrinz1:
          , "10.113.4.29"  --  sbrinz2:
          , "10.113.4.20"  --  gruyere:
          , "10.113.4.71"  --  appenzeller:
      ]


generateFlowsWrapper fperApp clcount = generateFlows serverIP srvPort  clients_ip_addr clientStartPort fperApp
    where
    serverIP =  DM.fromJust $ IP4.ipFromString "10.113.4.95"

    srvPort = 7777
    clients_ip_addr = map ( DM.fromJust . IP4.ipFromString ) $ getClientList clcount
    clientStartPort = 8000

flowsToQueue :: [Flow] -> Int -> Int -> [(Flow, Int, Int)]
flowsToQueue flist fpApp nq =  glFLows1  ++  glFLows2 ++ otherQmap'
    where
    pflows = DL.sort $ filter (isGoldFl fpApp) flist
    halfway = ((length pflows) `div` 2)
    glFLows1 = map (\x -> (x, 1, 1) ) $ take halfway pflows
    glFLows2 = map (\x -> (x, 2, 1) ) $ drop halfway pflows

    npflows = DL.sort $ filter (not . (isGoldFl fpApp)) flist
    otherQmap = zip npflows $ take (length npflows) $ concat (map (\x-> take fpApp $ repeat x) $ concat $ repeat ([3..nq] ++ [0]))
    otherQmap' = map (\(a, b) -> (a, b, 1)) otherQmap

-- All flows are:
realFlows fpApp clients = generateFlowsWrapper fpApp clients

priorityCost' fpApp  = S.priorityCost (isGoldFl fpApp) goldFlPerQ

e10kT = E10k.graphH
e10kU = fst <$> e10kT


hardcodedOracleMemcached fpApp clients nq  costfn prgU = params
    where
        qmap = flowsToQueue (sortedRealFlows fpApp clients) fpApp nq

        oracle   = S.E10kOracleHardCoded {S.nnHardcoded = (nq, qmap)}
        --oracle   = S.E10kOracleSt {S.nQueues = nq}
        priFn        = (priorityCost' fpApp) nq
        balFn        = S.balanceCost nq
        costFns    = [("balance", balFn), ("priority", priFn)]
        costFn     = case lookup costfn costFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        params = S.initSearchParams {  S.sOracle = oracle
                                   , S.sPrgU   = prgU
                                   , S.sCostFn = priFn
                                   , S.sStrategy = S.searchGreedyFlows}



test = do
    -- unconfigured PRG
    prgU <- e10kU
    --prgU <- e10kU_simple
    let nq = 5
        fpApp = 2 --16
        clients = 20
        qmap_for_debug = flowsToQueue (sortedRealFlows fpApp clients) fpApp nq

        params = hardcodedOracleMemcached fpApp clients nq "priority" prgU

        conflows = sortedRealFlows fpApp clients
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

