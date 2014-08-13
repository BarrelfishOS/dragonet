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

data DbgAction a =
    DbgPGraph PG.PGraph |
    DbgPLGraph PL.PLGraph |
    DbgEvaluated PL.PLGraph a

type DbgFunSingle a = String -> DbgAction a -> IO ()
type DbgFunction a = String -> DbgFunSingle a

type CostFunction a = PL.PLGraph -> a

makeGraph ::
       Sem.Helpers           -- | Semantics helpers combined
    -> PG.PGraph             -- | Unconfigured PRG
    -> PG.PGraph             -- | Configured LPG
    -> [PG.PGraph
        -> PG.PGraph]        -- | Implementation transforms
    -> (PG.PGNode -> String) -- | Assign nodes to pipelines
    -> DbgFunSingle a        -- | Debugging function
    -> C.Configuration       -- | Configuration to apply
    -> IO PL.PLGraph
makeGraph helpers prg lpg implTransforms pla debug cfg = do
    -- Configure graph
    let prgC  = C.applyConfig cfg prg
    debug "prg_c" $ DbgPGraph prgC
    makeGraph' helpers prgC lpg Emb.embeddingRxTx implTransforms pla debug

makeGraph' ::
       Sem.Helpers                           -- | Semantics helpers combined
    -> PG.PGraph                             -- | Configured PRG
    -> PG.PGraph                             -- | Configured LPG
    -> (PG.PGraph -> PG.PGraph -> PG.PGraph) -- | embedding algorithm
    -> [PG.PGraph -> PG.PGraph]              -- | Implementation transforms
    -> (PG.PGNode -> String)                 -- | Assign nodes to pipelines
    -> DbgFunSingle a                        -- | Debugging function
    -> IO PL.PLGraph
makeGraph' helpers prgC lpg embed_fn implTransforms pla debug = do
    -- Embed graph
    let emb = embed_fn prgC lpg
    debug "embed" $ DbgPGraph emb
    -- Reduce graph
    reduced <- SS.reducePG emb helpers
    debug "reduced" $ DbgPGraph reduced
    -- Clean-up graph
    let cleanedUp = cleanupGraph reduced
    debug "cleanup" $ DbgPGraph cleanedUp
    -- Apply implementation transforms
    let implGraph = cleanupGraph $ foldl (\g t -> t g) cleanedUp implTransforms
    debug "implT" $ DbgPGraph implGraph
    -- Drop hardware nodes
    let nohwGraph = dropHardwareNodes implGraph
    debug "nohw" $ DbgPGraph nohwGraph
    -- Partition graph
    let plg = PL.generatePLG pla nohwGraph
    debug "pipelines" $ DbgPLGraph plg
    return plg

optimize :: Ord a =>
       Sem.Helpers                     -- | Semantics helpers combined
    -> PG.PGraph                       -- | Unconfigured PRG
    -> PG.PGraph                       -- | Configured LPG
    -> [PG.PGraph -> PG.PGraph]        -- | Implementation transforms
    -> (String -> PG.PGNode -> String) -- | Assign nodes to pipelines
    -> DbgFunction a                   -- | Debugging function
    -> CostFunction a                  -- | Cost function
    -> [(String,C.Configuration)]      -- | Configurations to evaluate
    -> IO (PL.PLGraph, (String,C.Configuration))
optimize hs prg lpg implTransforms pla dbg cf cfgs = do
    evald <- forM cfgs $ \(lbl,cfg) -> do
        plg <- makeGraph hs prg lpg implTransforms (pla lbl) (dbg lbl) cfg
        let cost = cf plg
        dbg lbl lbl $ DbgEvaluated plg cost
        return (cost, plg, (lbl,cfg))
    let fst3 (a,_,_) = a
        (_,minPlg,lcfg) = L.minimumBy (compare `on` fst3) evald
    return (minPlg, lcfg)

dbgDummy :: DbgFunction a
dbgDummy _ _ _ = return ()

dbgDotfiles :: Show a => String -> DbgFunction a
dbgDotfiles bdir cl gl d =
    case d of
        DbgPGraph pg ->
            writeF (gl ++ ".dot") $ toDot pg
        DbgPLGraph plg -> do
            let linkMap pl = gl FP.</> PL.plLabel pl ++ ".svg"
            writeF (gl ++ ".dot") $ pipelinesDot (Just linkMap) plg
            forM_ (DGI.labNodes plg) $ \(_,pl) ->
                writeF (gl FP.</> PL.plLabel pl  ++ ".dot") $
                    toDot $ PL.plGraph pl
        DbgEvaluated _ cost ->
            putStrLn $ cl ++ " -> " ++ show cost
    where
        writeF f c = do
            let path = bdir FP.</> cl FP.</> f
                parent = FP.dropFileName path
            Dir.createDirectoryIfMissing True parent
            writeFile path c


-- This does not really belong here...
--
-- Remove sources without "source" attribute and sinks without "sink" attribute
cleanupGraph :: PG.PGraph -> PG.PGraph
cleanupGraph g
    | null badNodes = g
    | otherwise = cleanupGraph g'
    where
        hasAttr a n = elem (PG.NAttrCustom a) $ PG.nAttributes n
        srcs = filter (onlySpawnEs . DGI.lpre g . fst) $ DGI.labNodes g
        snks = filter (onlySpawnEs . DGI.lsuc g . fst) $ DGI.labNodes g
        badSrcs = filter (not . hasAttr "source" . snd) srcs
        badSnks = filter (not . hasAttr "sink" . snd) snks
        badNodes = L.nub $ map fst $ badSrcs ++ badSnks
        g' = DGI.delNodes badNodes g
        onlySpawnEs = all isSpawnE
            where isSpawnE (_,PG.ESpawn {}) = True
                  isSpawnE _ = False

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

