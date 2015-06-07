-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.Optimization (
    DbgAction(..),
    DbgFunction,
    CostFunction,

    makeGraph,
    makeGraph',
    optimize,

    dbgDummy,
    dbgDotfiles
) where

import qualified Dragonet.Configuration as C
import Dragonet.DotGenerator (toDot, pipelinesDot)
import qualified Dragonet.Embedding as Emb
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Semantics.Simplify as SS

import Control.Monad (forM, forM_)

import Data.Function (on)
import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DGIDFS
import qualified Data.List as L
import qualified Data.Set as S

import qualified System.FilePath as FP
import qualified System.Directory as Dir

import Util.XTimeIt (doTimeIt,dontTimeIt)

data DbgAction a =
    DbgPGraph PG.PGraph |
    DbgPLGraph PL.PLGraph |
    DbgEvaluated PL.PLGraph a

type DbgFunSingle a = String -> DbgAction a -> IO ()
type DbgFunction a = String -> DbgFunSingle a

type CostFunction a = PL.PLGraph -> (a, String)

makeGraph ::
       Sem.Helpers           -- ^ Semantics helpers combined
    -> PG.PGraph             -- ^ Unconfigured PRG
    -> PG.PGraph             -- ^ Configured LPG
    -> [PG.PGraph
        -> PG.PGraph]        -- ^ Implementation transforms
    -> (PG.PGNode -> String) -- ^ Assign nodes to pipelines
    -> DbgFunSingle a        -- ^ Debugging function
    -> C.Configuration       -- ^ Configuration to apply
    -> IO PL.PLGraph
makeGraph helpers prg lpg implTransforms pla debug cfg = do
    -- Configure graph
    let prgC  = C.applyConfig cfg prg
    debug "prg_c" $ DbgPGraph prgC
    makeGraph' helpers prgC lpg Emb.embeddingRxTx implTransforms pla debug

makeGraph' ::
       Sem.Helpers                           -- ^ Semantics helpers combined
    -> PG.PGraph                             -- ^ Configured PRG
    -> PG.PGraph                             -- ^ Configured LPG
    -> (PG.PGraph -> PG.PGraph -> PG.PGraph) -- ^ embedding algorithm
    -> [PG.PGraph -> PG.PGraph]              -- ^ Implementation transforms
    -> (PG.PGNode -> String)                 -- ^ Assign nodes to pipelines
    -> DbgFunSingle a                        -- ^ Debugging function
    -> IO PL.PLGraph
makeGraph' helpers prgC lpg embed_fn implTransforms pla debug = do
    -- Embed graph
    let emb = embed_fn prgC lpg
    debug "embed" $ DbgPGraph emb
    -- Reduce graph
    reduced <- doTimeIt "makeGraph.REDUCE:" $ SS.reducePG emb helpers
    debug "reduced" $ DbgPGraph reduced
    -- Clean-up graph
    let cleanedUp = PGU.cleanupGraph reduced
    --cleanedUp <- doTimeIt "makeGraph.cleanup:" $ PGU.cleanupGraph reduced
    debug "cleanup" $ DbgPGraph cleanedUp
    -- Apply implementation transforms
    let implGraph = PGU.cleanupGraph $ foldl (\g t -> t g) cleanedUp implTransforms
    --implGraph <- doTimeIt "makeGraph.implGraph:" $ PGU.cleanupGraph $ foldl (\g t -> t g) cleanedUp implTransforms
    debug "implT" $ DbgPGraph implGraph
    -- Drop hardware nodes
    let nohwGraph = dropHardwareNodes implGraph
    --nohwGraph <- doTimeIt "makeGraph.nohwGraph:" $ dropHardwareNodes implGraph
    debug "nohw" $ DbgPGraph nohwGraph
    -- Partition graph
    let plg = PL.generatePLG pla nohwGraph
    --plg  <- doTimeIt "makeGraph.generatePLG:" $ PL.generatePLG pla nohwGraph
    debug "pipelines" $ DbgPLGraph plg
    return plg

-- optimize function
-- STEP: this is important step, see if it applies the partial configuration
-- given unconfigured prg, lpg, configurations and cost function, it will
--      pick best configuration, apply it, and return the configured prg
optimize :: Ord a => Show a =>
       Sem.Helpers                     -- ^ Semantics helpers combined
    -> PG.PGraph                       -- ^ Unconfigured PRG
    -> PG.PGraph                       -- ^ Configured LPG
    -> [PG.PGraph -> PG.PGraph]        -- ^ Implementation transforms
    -> (String -> PG.PGNode -> String) -- ^ Assign nodes to pipelines
    -> DbgFunction a                   -- ^ Debugging function
    -> CostFunction a                  -- ^ Cost function
    -> [(String,C.Configuration)]      -- ^ Configurations to evaluate
    -> IO (PL.PLGraph, (String,C.Configuration), PG.PGraph)

optimize hs prg lpg implTransforms pla dbg cf cfgs = do
    evald <- forM cfgs $ \(lbl,cfg) -> do
        let prgC  = C.applyConfig cfg prg
        --(dbg lbl) "prg_c" $ DbgPGraph prgC
        plg <- doTimeIt "makeGraph'" $ makeGraph' hs prgC lpg Emb.embeddingRxTx implTransforms (pla lbl) (dbg lbl)
        --plg <- makeGraph hs prg lpg implTransforms (pla lbl) (dbg lbl) cfg
        let (cost, dbgmsg) = cf plg
        dbg lbl lbl $ DbgEvaluated plg cost
        putStrLn $ "Cost function returned value "  ++ " with msg " ++ dbgmsg
        return (cost, plg, (lbl,cfg), prgC)
    let fst3 (a,_,_,_) = a
        --(minCost,minPlg,lcfg, prgpc) = L.minimumBy (compare `on` fst3) evald
        (minCost,minPlg,lcfg, prgpc) = L.maximumBy (compare `on` fst3) evald
        (selectedConfName, _) = lcfg
    putStrLn $ "Selected PRG conf is " ++ (show lcfg)
            ++ " with cost " ++ (show minCost)
    return ( minPlg, lcfg, prgpc)

dbgDummy :: Show a => String -> DbgFunction a
dbgDummy _ _ _ _ = return ()

dbgDotfiles :: Show a => String -> DbgFunction a
dbgDotfiles bdir cl gl d =
    case d of
        DbgPGraph pg ->
                do
                    --putStrLn $ cl ++ " -> pg type " ++ (show bdir)
                    writeF (gl ++ ".dot") $ toDot pg
        DbgPLGraph plg -> do
            --putStrLn $ cl ++ " -> plg type " ++ (show bdir)
            let linkMap pl = gl FP.</> PL.plLabel pl ++ ".svg"
            writeF (gl ++ ".dot") $ pipelinesDot (Just linkMap) plg
            forM_ (DGI.labNodes plg) $ \(_,pl) ->
                writeF (gl FP.</> PL.plLabel pl  ++ ".dot") $
                    toDot $ PL.plGraph pl
        DbgEvaluated _ cost ->
                do
                    putStrLn $ cl ++ " -> dbgEval type " ++ (show bdir)
                            ++ ", cost -> " ++ show cost
    where
        writeF f c = do
            let path = bdir FP.</> cl FP.</> f
                parent = FP.dropFileName path
            Dir.createDirectoryIfMissing True parent
            writeFile path c


dropHardwareNodes :: PG.PGraph -> PG.PGraph
dropHardwareNodes pg = DGI.delNodes (S.toList dropNodes) pg
    where
        nsepg = PGU.dropSpawnEdges pg
        dropNodes = (rxHwNodes `S.union` txHwNodes) S.\\
                        (S.fromList (rxNodes ++ txNodes))

        rxNodes = [n | (n,l) <- DGI.labNodes pg,
                       "RxQueue" `L.isPrefixOf` PG.nLabel l]
        rxHwNodes = S.fromList $ DGI.dfs rxNodes $ DGI.grev nsepg

        txNodes = [n | (n,l) <- DGI.labNodes pg,
                       "TxQueue" `L.isPrefixOf` PG.nLabel l]
        txHwNodes = S.fromList $ DGI.dfs txNodes nsepg
