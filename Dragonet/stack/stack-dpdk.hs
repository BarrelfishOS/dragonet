{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}


-- TODO: As most of the code is moved to DpdkImpl.hs many of these imports
--      are not needed anymore.  So, do a cleanup here
import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.E10k as E10k
import qualified Runner.E10KControl as CTRL

import qualified ReadArgs as RA

import Stack
import qualified Dragonet.Search as Search
import qualified Stack as SS

-- For DPDK based implementation of E10K NIC
import qualified DpdkImpl as DPDKIMPL

import Control.Monad (forever, forM_)
import Control.Applicative ((<$>))
import Control.Concurrent(forkIO, ThreadId)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Word

import qualified MachineDetails as MD

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import qualified Scenarios.S1 as S1
import qualified Scenarios.S3 as S3

tr a b = trace b a
trN a b = a

putStrLnDbg x = putStrLn x
--putStrLnDbgN x = return ()
putStrLnDbgN x = putStrLn x


main = do
    ((nq',costfn,oraclefn,concurrency,clients) :: (Int,String,String,Int,Int)) <- RA.readArgs

    -- to avoid off by one error as most of the code assumes
    let nq = nq' - 1

    print $ "Number of queues used: " ++ show nq
    print $ "Cost function: " ++ show costfn
    print $ "Oracle function: " ++ show oraclefn
    print $ "Concurrency (only for hardcoded oracle) : " ++ show concurrency
    print $ "clients (only for hardcoded oracle) : " ++ show clients

    -- Prepare graphs and so on
    prgH@(prgU,_) <- E10k.graphH
    implFn <- DPDKIMPL.dpdkImplFunction

    --let e10kOracle = Search.E10kOracleSt {Search.nQueues = nq}
    let e10kOracle = Search.initE10kOracle nq

        --priFn      = S1.priorityCost nq
        priFn      = (S1.priorityCost' (fromIntegral concurrency)) nq
        balFn      = Search.balanceCost nq
        strategy   = Search.searchGreedyFlows
        costFns    = [("balance", balFn), ("priority", priFn)]
        costFn     = case lookup costfn costFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        greedyParams    = Search.runSearch $ Search.initSearchParams {
                                               Search.sOracle = e10kOracle
                                               , Search.sPrgU   = prgU
                                               , Search.sCostFn = costFn
                                               , Search.sStrategy = strategy }

        -- FIXME: pass the argument "priority" to hardcodedOracleMemcached
        hardcodedParams = Search.runSearch $ S3.hardcodedOracleMemcachedIntel
                        concurrency clients nq  costfn prgU    -- fpApp clients nq prgU

        searchFns = [
                        ("greedy", (greedyParams, 0)),
                        ("hardcoded", (hardcodedParams, (concurrency * clients) ))
                    ]
        (searchFn, trigger) = case lookup oraclefn searchFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown oracle function:" ++ oraclefn


    -- TODO: Create the driver-thread before you go and start flows
    --      This should speedup the initialization as driver will be ready
    --      before applications starts connecting in
    instantiateFlows
        searchFn                -- ^ returns configurations to evaluate
        prgH                    -- ^ PRG
        DPDKIMPL.llvm_helpers            -- ^ llvm helpers
        implFn
        -- (implCfg tcstate chan)  -- ^ Function to implement given conf
        DPDKIMPL.plAssignMerged          -- ^ classifies graph-nodes into pipelines based on node-tag

