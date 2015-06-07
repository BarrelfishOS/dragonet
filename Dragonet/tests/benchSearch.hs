-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE BangPatterns #-}

import qualified Graphs.E10k as E10k
import qualified Graphs.Cfg as Cfg
import qualified Dragonet.Configuration as C
import qualified Dragonet.Search as S
import Graphs.Cfg (prgCfg)

import Dragonet.Flows (Flow(..))
import qualified Dragonet.Flows as FL

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S

import Control.Applicative ((<$>))
import Control.Monad (forM, foldM)

import Criterion.Main (defaultMainWith, defaultMain, nf, whnf, bench, bgroup, nfIO)
import Criterion.Config

import Text.Show.Pretty (ppShow)

import Util.XTimeIt (doTimeIt, dontTimeIt)
import Debug.Trace (trace)

import qualified Control.Monad.ST        as ST
import qualified System.Random as R

tr a b  = trace b a
trN a b = a

e10kT = E10k.graphH
e10kU = fst <$> e10kT
e10kH = snd <$> e10kT
e10kC = (C.applyConfig prgCfg) <$> e10kU

e10kT_simple = E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
e10kU_simple = fst <$> e10kT_simple
e10kH_simple = snd <$> e10kT_simple
e10kC_simple = (C.applyConfig prgCfg) <$> e10kU_simple

-- Let us for now consider that only new connections apper. For handling
-- connectiong going away, we can add a property to the endpoint description
-- about wheter a connection was added or removed so that the oracle can act
-- accordingly.
fs = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ 1000 + i
   , flSrcIp    = Nothing
   , flSrcPort  = Nothing } | i <- [1..40] ]

connectFlows :: Int -> [Flow]
connectFlows start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just 123
   , flSrcPort  = Just 7777 } | i <- [1..] ]

flAddedFlows :: [Flow] -> FL.FlowsSt
flAddedFlows flows = foldl FL.fsAddFlow FL.flowsStInit flows

hpIp = 222
beIp = 100
hpFlowsPerQ = 2
isHp FlowUDPv4 {flSrcIp = Just srcIp} = srcIp == hpIp
priorityCost' = S.priorityCost isHp hpFlowsPerQ
prioritySort' = S.prioritySort isHp

staticCost' = S.staticCost isHp 4

beFlows_ :: Int -> [Flow]
beFlows_ start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just beIp
   , flSrcPort  = Just 7777 } | i <- [1..] ]

hpFlows_ :: Int -> [Flow]
hpFlows_ start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just hpIp
   , flSrcPort  = Just 7777 } | i <- [1..] ]

data TestIncrSearch = TestIncrSearch {
      tisHpPort   :: Int
    , tisBePort   :: Int
    , tisHpCount  :: Int
    , tisBeCount  :: Int
    , tisFlowsSt  :: FL.FlowsSt
    , tisRg       :: R.StdGen
}

initTestIncrSearch rg = TestIncrSearch {
      tisHpPort   = 6000
    , tisBePort   = 1000
    , tisHpCount  = 0
    , tisBeCount  = 0
    , tisFlowsSt  = FL.flowsStInit
    , tisRg       = rg}

data TestIncrSearchOp = TisAddHpFlows Int
                      | TisAddBeFlows Int
                      | TisRemFlows [Int]
                      | TisRemRand Int
    deriving (Show)

tisOpFlowsSt :: TestIncrSearch
             -> TestIncrSearchOp
             -> (TestIncrSearch, FL.FlowsSt)

randUnique :: R.StdGen -> (Int,Int) -> Int -> ([Int], R.StdGen)
randUnique rg range len = (S.toList s', rg')
    where   (s',rg') = gen (S.empty, rg)
            --gen :: (S.Set a, R.StdGen) -> (S.Set a, R.StdGen)
            gen (s,g) | S.size s == len = (s,g)
                      | otherwise =
                            let (elem_, g') = R.randomR range g
                                elem = trN elem_ ("NEW ELEMENT:" ++ (show elem_))
                                s' = S.insert elem s
                            in gen (s',g')


tisOpFlowsSt tis (TisAddHpFlows x) = (tis', flst')
    where hpPort = tisHpPort tis
          hpFs = take x $ hpFlows_ hpPort
          flst' = foldl FL.fsAddFlow (tisFlowsSt tis) hpFs
          tis' = tis { tisHpPort = hpPort + x,
                       tisFlowsSt = flst'}

tisOpFlowsSt tis (TisAddBeFlows x) = (tis', flst')
    where bePort = tisBePort tis
          beFs = take x $ beFlows_ bePort
          flst' = foldl FL.fsAddFlow (tisFlowsSt tis) beFs
          tis' = tis { tisBePort = bePort + x,
                       tisFlowsSt = flst'}

tisOpFlowsSt tis (TisRemFlows []) = (tis, tisFlowsSt tis)
tisOpFlowsSt tis (TisRemFlows (x:xs)) = tisOpFlowsSt tis' (TisRemFlows xs)
    where flst = tisFlowsSt tis
          flows = S.toList $ FL.fsCurrent flst
          flow = flows !! x
          flst' = FL.fsRemFlow flst flow
          tis' = tis { tisFlowsSt = flst' }

tisOpFlowsSt tis (TisRemRand nelems) = tisOpFlowsSt tis' (TisRemFlows rmFlowsIdx)
    where flst = tisFlowsSt tis
          rg    = tisRg tis
          flows = FL.fsCurrent flst
          nflows_ = S.size flows
          nflows = trN nflows_ ("NFLOWS=" ++ (show nflows_))
          (rmFlowsIdx_, rg') = randUnique rg (0, nflows-1) nelems
          rmFlowsIdx = trN rmFlowsIdx_ ("REMOVING: " ++ (show rmFlowsIdx_))
          tis' = tis { tisRg = rg' }

sAddPr p str = L.unlines $ [ p ++ s | s <- L.lines str ]

tisDoOp (ss, tis) op = do
    (c, s) <- tisDoOpRet (ss,tis) op
    return s

tisDoOpRet (ss, tis) op = do
    let (st', flst) = tisOpFlowsSt tis op
        st'' = st' { tisFlowsSt = FL.fsReset (tisFlowsSt st') }
    (newSS, newSt, newConf) <- do
        (conf, ic, ss') <- S.runIncrSearchIO ss flst
        return (ss', st'', conf)
    --putStrLn $ "tisDoOP result:\n" ++ (sAddPr "| " $ ppShow newConf)
    --qmap <- ST.stToIO $ incrSearchQmap newSS
    --putStrLn $ qmapStr qmap
    return (newConf, (newSS, newSt))

tisDoOpConf (ss, tis) op = do
    (c, s) <- tisDoOpRet (ss,tis) op
    return c

test_incr_var = do
    putStrLn $ "Running incremental search!"
    r <- R.getStdGen
    prgU <- e10kU_simple
    let nq = 10
        hpPort   = 6000
        bePort   = 1000
        e10kOracle = S.initE10kOracle nq
        pri = (priorityCost' nq, prioritySort')
        bal = (S.balanceCost nq, id)
        sta = (staticCost' nq, id)

        fns = sta
        params = S.initIncrSearchParams {  S.isOracle = e10kOracle
                                       ,   S.isPrgU   = prgU
                                       ,   S.isCostFn = fst fns
                                       ,   S.isOrderFlows = snd fns}

    ss0 <- S.initIncrSearchIO params
    let tis0 = initTestIncrSearch r

    (ss,tis) <- foldM tisDoOp (ss0, tis0) $
                  [TisAddBeFlows 400, TisAddHpFlows 100, TisRemRand 1]
                   --TisAddHpFlows 1,
                   --TisRemFlows [250]]
    --qmap <- ST.stToIO $ S.incrSearchQmap ss
    --putStrLn $ S.qmapStr qmap

    return ()

initTis params nflows = do
    r <- R.getStdGen
    ss0 <- S.initIncrSearchIO params
    let tis0 = initTestIncrSearch r
    tisDoOp (ss0,tis0) $ TisAddBeFlows nflows

getCostFns nq costf = case costf of
    "bal" -> (S.balanceCost nq, id)
    "pri" -> (priorityCost' nq, prioritySort')
    "sta" -> (staticCost' nq, id)

defSearchParams nq costf = do
    let costFns = getCostFns nq costf
        oracle = S.initE10kOracle nq
    prgU <- e10kU_simple
    return $ S.initSearchParams { S.sOracle     = oracle
                                , S.sPrgU       = prgU
                                , S.sCostFn     = fst costFns
                                , S.sOrderFlows = snd costFns}

defIncrSearchParams nq costf = do
    let costFns = getCostFns nq costf
        oracle = S.initE10kOracle nq
    prgU <- e10kU_simple
    return $ S.initIncrSearchParams {  S.isOracle = oracle
                                    ,   S.isPrgU   = prgU
                                    ,   S.isCostFn = fst costFns
                                    ,   S.isOrderFlows = snd costFns}


main = do
    let nq = 10 -- number of queues
        --nflowsl = [1,5,10,20,40,80]
        nflowsl     = [10, 20, 50, 100, 250, 500]
        nflowslIncr = nflowsl
        samples = 3
        getFlows nflows = take nflows $ connectFlows 1000

    prgU <- e10kU_simple

    searchParams     <- defSearchParams nq "bal"
    incrSearchParams <- defIncrSearchParams nq "bal"

    -- initialize all tis initial states
    tisTs0 <- forM nflowslIncr (initTis incrSearchParams)

    let benchBal nflows = S.runSearch searchParams (getFlows nflows)
        benchNf nflows  = bench ("flows:" ++ (show nflows)) $ nf benchBal nflows
        -- incremental
        benchIncBal nflows = S.runIncrSearch incrSearchParams (flAddedFlows $ getFlows nflows)
        benchIncNf nflows  = bench ("flows:" ++ (show nflows)) $ nf benchIncBal nflows
        -- incremental ops
        bgroups = [   bgroup "search"
                        [benchNf x | x <- nflowsl]
                    , bgroup "incr. search"
                             [benchIncNf x | x <- nflowslIncr]
                    , bgroup "incr. search/OPs:"
                            [ bench desc (nfIO $ tisDoOpConf (ss,tis) op)
                                    | (ss,tis) <- tisTs0,
                                      op <- [TisAddBeFlows 1,
                                             TisAddBeFlows 5,
                                             TisAddBeFlows 10,
                                             TisRemRand 1,
                                             TisRemRand 5,
                                             TisRemRand 10],
                                    let nflows = S.size $ FL.fsCurrent $ tisFlowsSt tis,
                                    let desc = "flows:" ++ (show nflows) ++ " op:" ++ (show op)]
                  ]


    defaultMainWith
                defaultConfig { cfgSamples = ljust samples }
                (return ())
                bgroups
