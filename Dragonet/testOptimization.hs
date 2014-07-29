{-# LANGUAGE OverloadedStrings #-}
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, pipelinesDot)
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Embedding as Emb
import Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Configuration as C
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Optimization as O

import qualified Graphs.E10k as E10k
import qualified Graphs.LPG as LPG


import qualified Data.Graph.Inductive.Graph as DGI

import Data.Maybe
import qualified Data.List as L
import Data.Functor ((<$>))
import Data.String (fromString)

import qualified System.Directory as Dir



plAssign _ (_,n)
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


main = do
    let pref = ""
    let write f s = writeFile (pref ++ f) s

    Dir.createDirectoryIfMissing True "out/opt-graphs"
    (prgU,prgH) <- E10k.graphH
    (lpgU,lpgH) <- LPG.graphH
    let helpers = prgH `Sem.mergeHelpers` lpgH

    -- Dot for unconfigured graphs...
    writeFile "out/opt-graphs/prg_u.dot" $ toDot prgU
    writeFile "out/opt-graphs/lpg_u.dot" $ toDot lpgU

    -- Configure LPG
    let lpgC  = C.applyConfig lpgCfg lpgU
    writeFile "out/opt-graphs/lpg_c.dot" $ toDot lpgC


    -- Evaluate configurations
    let configs = [("cfg_empty_",prgCfgEmpty), ("cfg_test_", prgCfg)]

    -- Evaluate graph
    plg <- O.optimize helpers prgU lpgC plAssign
            (O.dbgDotfiles "out/opt-graphs") costFunction configs
    return ()


