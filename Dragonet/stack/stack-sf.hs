{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Search                   as  SE
import qualified Dragonet.Flows                    as  FL


import qualified Graphs.SF as SF
import qualified Runner.SFControl as CTRL

import qualified ReadArgs as RA

import Stack
import qualified Dragonet.Search as Search
import qualified Stack as SS

import qualified Options.Applicative as OA

-- For onload based implementation of SF NIC
import qualified OnloadImpl as ONLOADIMPL


import Control.Monad (forever, forM_)
import Control.Applicative ((<$>),(<*>))
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

tr a b = trace b a
trN a b = a

putStrLnDbg x = putStrLn x
--putStrLnDbgN x = return ()
putStrLnDbgN x = putStrLn x



onloadPrgArgs :: IO SS.StackPrgArgs
onloadPrgArgs = do
    prgH <- SF.graphH
    implFn <- ONLOADIMPL.onloadImplFunction
    return SS.StackPrgArgs { SS.stPrgH = prgH
                           , SS.stLlvmH = ONLOADIMPL.llvm_helpers
                           , SS.stCfgImpl = implFn }

data StackSFOpts = StackSFOpts {
      optNq          :: Int
    , optCostFn      :: (Integer -> Int -> SE.CostQueueFn, [FL.Flow] -> [FL.Flow])
    , optPrgArgs     :: (String, IO SS.StackPrgArgs)
    , optFlowsPerApp :: Integer
    , optIncremental :: Bool
} deriving (Show)

instance Show (String, IO SS.StackPrgArgs) where
    show (x,_) = show x

runStackSF :: StackSFOpts -> IO ()
runStackSF opts@(StackSFOpts {optIncremental = incremental}) = do
    case incremental of
        True  -> doRunStackSFIncremental opts
        False -> doRunStackSF opts

doRunStackSFIncremental :: StackSFOpts -> IO ()
doRunStackSFIncremental opts = do
    -- PRG (use simple graph. Hope this works!)
    --(prgU,_) <- SF.graphH
    (prgU,_) <- SF.graphH_ "Graphs/SF/prgSFImpl-simple.unicorn"
    -- FIXME: Using full graph did not worked (atleast with incremental run)
    --prgH@(prgU,_) <- SF.graphH

    -- get options
    let nq       = optNq opts
        fpa      = optFlowsPerApp opts
        costFn   = (fst $ optCostFn opts) fpa nq
        sortFn   = snd $ optCostFn opts

    -- search parameters
    let sParams = SE.initIncrSearchParams {
                          SE.isOracle = SE.initSFOracle nq
                        , SE.isPrgU = prgU
                        , SE.isCostFn = costFn
                        , SE.isOrderFlows = sortFn }

    -- PRG
    prgArgs <- (snd $ optPrgArgs opts)

    se0 <- SE.initIncrSearchIO sParams
    SS.instantiateIncrFlowsIO_ SE.runIncrSearchIO se0 ONLOADIMPL.plAssignMerged prgArgs

doRunStackSF :: StackSFOpts -> IO ()
doRunStackSF opts = do
    -- PRG
    (prgU,_) <- SF.graphH
    -- get options
    let nq       = optNq opts
        fpa      = optFlowsPerApp opts
        costFn   = (fst $ optCostFn opts) fpa nq
        sortFn   = snd $ optCostFn opts

    -- search parameters
    let sParams = SE.initSearchParams {
                        SE.sOracle = SE.initSFOracle nq
                      , SE.sPrgU   = prgU
                      , SE.sCostFn = costFn
                      , SE.sOrderFlows = sortFn }

    -- [Flow] -> IO C.Configuration
    getConfIO <- SE.runSearchIO <$> SE.initSearchIO sParams

    -- PRG
    prgArgs <- (snd $ optPrgArgs opts)

    SS.instantiateFlowsIO_ getConfIO ONLOADIMPL.plAssignMerged prgArgs


costFnL fPerq = [
      ("balance",  (\fpa nq qmap -> SE.balanceCost nq qmap, id))
    , ("priority", (S1.priorityCost', (S1.prioritySort' fPerq)))
    , ("static",  (S1.staticCost, (id)))
 ]

costFnParser = OA.str >>= doParse
    where doParse :: String -> OA.ReadM (Integer -> Int -> SE.CostQueueFn, [FL.Flow] -> [FL.Flow])
          doParse x = case L.lookup x (costFnL fPerQ) of
                Nothing -> OA.readerError "Uknown cost function"
                Just y  -> return y

prgArgsL = [
      ("sf", ("sf", onloadPrgArgs))
 ]
prgArgsParser  = OA.str >>= doParse
    where doParse :: String -> OA.ReadM (String, IO SS.StackPrgArgs)
          doParse x = case L.lookup x prgArgsL of
                Nothing -> OA.readerError "Uknown prgArgs option"
                Just y  -> return y

-- Number of flows in single gold queue allowed.
--      Typically it should equal to  lows per application
-- TODO: Read this from commandline args
fPerQ = 1

stackSFParserInfo:: OA.ParserInfo StackSFOpts
stackSFParserInfo = OA.info (OA.helper <*> parser) info
    where parser = StackSFOpts
                <$> OA.argument OA.auto infoNq
                <*> OA.argument costFnParser infoCostF
                <*> OA.argument prgArgsParser infoPrgArgs
                <*> OA.argument OA.auto infoFpA
                <*> OA.switch (OA.short 'i' OA.<> OA.help incrTxt)
          --fPerQ = 2
          info = OA.fullDesc OA.<> OA.header "Instantiate the SF stack"
          infoNq = (OA.metavar "nqueues" OA.<> OA.help "number of queues")
          infoFpA = (OA.metavar "fpa" OA.<> OA.help "flows per App")
          infoCostF = (OA.metavar costMeta OA.<> OA.help costHelp)
          infoPrgArgs = (OA.metavar prgMeta OA.<> OA.help prgHelp)
          incrTxt = "Run Incremental stack"
          costHelp = "cost function"
          costMeta = "CostF (one of:" ++ (show $ map fst (costFnL fPerQ)) ++")"
          prgMeta = "prgArgs (one of:" ++ (show $ map fst prgArgsL) ++")"
          prgHelp = "PRG backend"

main = do
    opts <- OA.execParser stackSFParserInfo
    putStrLn $ show opts
    runStackSF opts

