-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import qualified Dragonet.Incremental as INC
import Util.GraphHelpers (findNodeByL,mergeGraphsBy',delLEdges,updateN,
                            MergeDecision(..))

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Function (on)

import Text.Printf (printf)

import qualified Runner.E10KControl as E10K
import qualified Runner.E10KPolicy as E10KP
import Runner.Common

--import System.Environment (getArgs, getProgName)
--import System.Exit (exitSuccess)


-- Decides whether replicated versions of nodes should be merged
nodesMatch a b
    | nLabel a /= nLabel b = MergeNo
    | otherwise =
        case (both isNoRec a b, both isNoRecOut a b, both isNoRecInOut a b) of
            (True,_,_) -> MergeInOut
            (_,True,_) -> MergeIn
            (_,_,True) -> Merge
            _          -> MergeNo
    where
        both f a b = f a && f b
        isNoRec n = elem "noreplicate" $ nAttributes n
        isNoRecOut n = elem "noreplicateout" $ nAttributes n
        isNoRecInOut n = elem "noreplicateininout" $ nAttributes n

-- Simplistic e10k multi queue embedding
pg4e10k :: Int -> PGraph -> PGraph
pg4e10k nQueues pg =
    appQueuePorts $ untagApp $ foldl1 merge $ map queueGraph queueIDs
    where
        queueIDs = [0..(nQueues - 1)] :: [Int]
        ql i = printf "%03d" i
        queueGraph q = tagNodes ("Queue" ++ ql q) $
            renameQueues ("RxE10kQueue" ++ ql q) ("TxE10kQueue" ++ ql q) pg
        merge = mergeGraphsBy' nodesMatch
        untagApp = DGI.nmap fixAppN
        fixAppN n
            | nLabel n == "RxEchoAPP" = n { nTag = "App" }
            | otherwise = n

-- Divide edges to outgoing queues from application nodes into different ports
appQueuePorts :: PGraph -> PGraph
appQueuePorts pg = foldl appQueuePortsN pg $ map fst appNodes
    where
        appNodes = filter (\(_,l) -> (take 3 $ nLabel l) == "App") $ DGI.labNodes pg

appQueuePortsN :: PGraph -> DGI.Node -> PGraph
appQueuePortsN pg n = DGI.insEdges newE $ updateN addPorts n $ delLEdges oldE pg
    where
        tag m = nTag $ fromJust $ DGI.lab pg m
        sucs = L.sortBy (compare `on` (tag . fst)) $ DGI.lsuc pg n
        oldE = map (\(a,b) -> (n,a,b)) sucs
        newE = map (\(a,_) -> (n,a,tag a)) sucs
        ports = map (\(_,_,p) -> p) newE
        addPorts l = l { PG.nPorts = ports }

-- Translate action from inc embedding policy to hardware interaction
e10kAction :: AppIfState E10KP.E10kPState E10KP.E10kPAction
        -> E10KP.E10kPAction -> IO ()
e10kAction ais (E10KP.E10kPAct5TSet idx ft) = do
    putStrLn $ "E10kPAct5TSet " ++ show idx
    E10K.ftSet (aiStackState ais) (fromIntegral idx) ft
e10kAction ais act = do
    putStrLn $ "Unimplemented e10k policy action: " ++ show act

main :: IO ()
main = do
    (nQ, apps) <- parseDNArgs
    putStrLn $ "Running hardware queues: " ++ show nQ
    putStrLn $ "Running with app slots: " ++ show apps
    let helpers = "llvm-helpers-e10k"
        --nQ = 1
        e10kS = E10KP.e10kPStateInit 128
        pstate = INC.policyStateInit nQ e10kS E10KP.e10kPolicy

    runStackParsed apps (pg4e10k nQ) pstate e10kAction helpers


