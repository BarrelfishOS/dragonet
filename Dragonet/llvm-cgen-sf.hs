--import Dragonet.ProtocolGraph  as PG
--import qualified Dragonet.Incremental as INC
--
--import Runner.Common
--
---- Simulates a basic embedding (replace rx and tx queue by tap specific node)
--pg4tap :: PGraph -> PGraph
--pg4tap pg = tagNodes "" $ renameQueues "TapRxQueue" "TapTxQueue" pg
----pg4tap pg = tagNodes "" $ renameQueues "SFRxQueue" "SFTxQueue" pg
--
--main :: IO ()
--main = do
--    let helpers = "llvm-helpers-sf"
--        pstate = INC.policyStateInit 1 () dummyHwPolicy
--
--    runStack pg4tap pstate dummyHwAction helpers
--



import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import qualified Dragonet.Incremental as INC
import Util.GraphHelpers (findNodeByL,mergeGraphsBy',delLEdges,updateN,
                            MergeDecision(..))

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Function (on)

import Text.Printf (printf)

import qualified Runner.SFControl as SF
import qualified Runner.SFPolicy as SFP
import Runner.Common


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

-- Simplistic sf multi queue embedding
pg4sf :: Int -> PGraph -> PGraph
pg4sf nQueues pg =
    appQueuePorts $ untagApp $ foldl1 merge $ map queueGraph queueIDs
    where
        queueIDs = [0..(nQueues - 1)] :: [Int]
        ql i = printf "%03d" i
        queueGraph q = tagNodes ("Queue" ++ ql q) $
            --renameQueues ("RxSFQueue" ++ ql q) ("TxSFQueue" ++ ql q) pg
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
sfAction :: AppIfState SFP.SFPState SFP.SFPAction
        -> SFP.SFPAction -> IO ()
sfAction ais (SFP.SFPAct5TSet idx ft) = do
    putStrLn $ "SFPAct5TSet " ++ show idx
    SF.ftSet (aiStackState ais) (fromIntegral idx) ft
sfAction ais act = do
    putStrLn $ "Unimplemented sf policy action: " ++ show act


main :: IO ()
main = do
    (nQ, apps) <- parseDNArgs
    putStrLn $ "Running hardware queues: " ++ show nQ
    putStrLn $ "Running with app slots: " ++ show apps
    let helpers = "llvm-helpers-sf"
        sfS = SFP.sfPStateInit 128
        pstate = INC.policyStateInit nQ sfS SFP.sfPolicy
    runStackParsed apps (pg4sf nQ) pstate sfAction helpers

