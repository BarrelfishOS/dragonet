{-# LANGUAGE OverloadedStrings #-}
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Semantics.Simplify as SS
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, pipelinesDot)
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Embedding as Emb
import Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Configuration as C
import qualified Dragonet.Pipelines as PL

import qualified Graphs.E10k as E10k
import qualified Graphs.LPG as LPG


import qualified Data.Graph.Inductive.Graph as DGI

import Data.Maybe
import qualified Data.List as L
import Data.Functor ((<$>))
import Data.String (fromString)

import Debug.Trace (trace)

import qualified Runner.Dynamic as DYN


-- Parse graph from unicorn file
parseGraph :: FilePath -> IO (PG.PGraph,Sem.Helpers)
parseGraph path = do
    b <- readFile path >>= UnicornAST.parseGraph
    return $ Unicorn.constructGraph' b

--------------------------------------------------------------------------------
-- Graph transformations

-- Remove sources without "source" attribute and sinks without "sink" attribute
-- can also be used to remove hardware parts of the embeded graph
cleanupGraph :: PG.PGraph -> PG.PGraph
cleanupGraph g
    | null badNodes = g
    | otherwise = cleanupGraph g'
    where
        hasAttr a n = elem (PG.NAttrCustom a) $ PG.nAttributes n
        srcs = filter (null . DGI.pre g . fst) $ DGI.labNodes g
        snks = filter (null . DGI.suc g . fst) $ DGI.labNodes g
        badSrcs = filter (not . hasAttr "source" . snd) srcs
        badSnks = filter (not . hasAttr "sink" . snd) snks
        badNodes = L.nub $ map fst $ badSrcs ++ badSnks
        g' = DGI.delNodes badNodes g



pipelineGraph :: PG.PGraph -> PL.PLGraph
pipelineGraph g = PL.generatePLG plAssign g
    where
        plAssign (_,n)
            | take 3 lbl == "App" = lbl
            | take 2 lbl == "Tx" = "Tx" ++ tag
            | otherwise = "Rx" ++ tag
            where
                lbl = PG.nLabel n
                tag = PG.nTag n


--------------------------------------------------------------------------------
-- Configurations

lpgCfg = [
    ("RxL4UDPCUDPSockets", PG.CVList [
            PG.CVTuple [ PG.CVInt 0,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7 ],
            PG.CVTuple [ PG.CVInt 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1,
                PG.CVMaybe $ Just $ PG.CVInt 1337,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [ PG.CVInt 3,
                PG.CVMaybe $ Just $ PG.CVInt 5,
                PG.CVMaybe $ Just $ PG.CVInt 1338,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1234 ],
            PG.CVTuple [ PG.CVInt 4,
                PG.CVMaybe $ Just $ PG.CVInt 6,
                PG.CVMaybe $ Just $ PG.CVInt 2345,
                PG.CVMaybe $ Just $ PG.CVInt 3,
                PG.CVMaybe $ Just $ PG.CVInt 3456 ]
        ])]

prgCfgEmpty = [
    ("RxC5TupleFilter", PG.CVList []),
    ("RxCFDirFilter", PG.CVList [])
    ]

prgCfg = [
    ("RxC5TupleFilter", PG.CVList [
            PG.CVTuple [
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVEnum 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7,
                PG.CVInt 1,
                PG.CVInt 1 ],
            PG.CVTuple [
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVEnum 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8,
                PG.CVInt 1,
                PG.CVInt 2 ]
        ]),
    ("RxCFDirFilter", PG.CVList [])
    ]


--------------------------------------------------------------------------------
-- Cost Function

costFunction :: PL.PLGraph -> Integer
costFunction plg = sum $ map (plCost . snd) $ DGI.labNodes plg

plCost :: PL.Pipeline -> Integer
plCost pl = sum $ map (nodeCost . snd) $ DGI.labNodes $ PL.plGraph pl

nodeCost :: PG.Node -> Integer
nodeCost _ = 1


--------------------------------------------------------------------------------
-- Main


evalGraph helpers prg lpg pref cfg = do
    let write f s = writeFile (pref ++ f) s
    let prgC  = C.applyConfig cfg prg
    -- Dot for configured graphs...
    write "prg_c.dot" $ toDot prgC

    -- Dot for embedded graph...
    let emb = Emb.embeddingRxTx prgC lpg
    write "embed.dot" $ toDot emb

    -- Dot for reduced graph...
    reduced <- SS.reducePG emb helpers
    write "reduced.dot" $ toDot reduced

    -- Dot for cleaned-up graph...
    let cleanedUp = cleanupGraph reduced
    write "cleanup.dot" $ toDot cleanedUp

    -- Dots for pipeline graph...
    let linkMap pl = pref ++ "pl_" ++ PL.plLabel pl ++ ".svg"
    let plg = pipelineGraph cleanedUp
    write "pipelines.dot" $ pipelinesDot (Just linkMap) plg
    mapM_ (\(_,pl) ->
        write ("pl_" ++ PL.plLabel pl ++ ".dot") $ toDot $ PL.plGraph pl
        ) $ DGI.labNodes plg

    return (costFunction plg, plg)


main = do
    let pref = ""
    let write f s = writeFile (pref ++ f) s

    (prgU,prgH) <- E10k.graphH
    (lpgU,lpgH) <- LPG.graphH
    let helpers = prgH `Sem.mergeHelpers` lpgH

    -- Dot for unconfigured graphs...
    writeFile "prg_u.dot" $ toDot prgU
    writeFile "lpg_u.dot" $ toDot lpgU

    -- Configure LPG
    let lpgC  = C.applyConfig lpgCfg lpgU
    writeFile "lpg_c.dot" $ toDot lpgC


    -- Evaluate configurations
    let configs = [("cfg_empty_",prgCfgEmpty), ("cfg_test_", prgCfg)]

    -- Evaluate graph
    results <- mapM (uncurry $ evalGraph helpers prgU lpgC) configs
    putStrLn $ show $ zip (map fst configs) (map fst results)

