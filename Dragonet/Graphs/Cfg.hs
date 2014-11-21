module Graphs.Cfg (
    lpgCfg,
    prgCfgEmpty,
    prgCfg,
    e10kCfgEmpty,
    e10kCfgStr,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Configuration as C

import qualified Graphs.E10k as E10k

import qualified Data.List as L

--------------------------------------------------------------------------------
-- Configurations

lpgCfg = [
    ("RxL4UDPCUDPSockets", PG.CVList [
            PG.CVTuple [
                PG.CVList [PG.CVTuple [PG.CVInt 0, PG.CVInt 0]],
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7 ],
            PG.CVTuple [
                PG.CVList [PG.CVTuple [PG.CVInt 1, PG.CVInt 0]],
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [
                PG.CVList [PG.CVTuple [PG.CVInt 2, PG.CVInt 1]],
                PG.CVMaybe $ Just $ PG.CVInt 1,
                PG.CVMaybe $ Just $ PG.CVInt 1337,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [
                PG.CVList [PG.CVTuple [PG.CVInt 3, PG.CVInt 2]],
                PG.CVMaybe $ Just $ PG.CVInt 5,
                PG.CVMaybe $ Just $ PG.CVInt 1338,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1234 ],
            PG.CVTuple [
                PG.CVList [PG.CVTuple [PG.CVInt 4, PG.CVInt 2],
                           PG.CVTuple [PG.CVInt 5, PG.CVInt 3]],
                PG.CVMaybe $ Just $ PG.CVInt 6,
                PG.CVMaybe $ Just $ PG.CVInt 2345,
                PG.CVMaybe $ Just $ PG.CVInt 3,
                PG.CVMaybe $ Just $ PG.CVInt 3456 ]
        ])]

prgCfgEmpty = e10kCfgEmpty
e10kCfgEmpty = [
    ("RxC5TupleFilter", PG.CVList []),
    ("RxCFDirFilter", PG.CVList [])
    ]

e10kCfgStr :: C.Configuration -> String
e10kCfgStr cnf_ = ret
    where  cnf :: [(String, PG.ConfValue)]
           cnf = cnf_
           ret = "CONF:\n" ++ L.intercalate "\n" (c5t ++ cfdt)
           c5t = case L.lookup "RxC5TupleFilter" cnf of
               Nothing -> []
               Just c  -> map (((++) " ") . E10k.c5tFullString)
                          $ E10k.parse5tCFG c
           cfdt = case L.lookup "RxCFDirFilter" cnf of
               Nothing -> []
               Just c  -> map (((++) " ") . E10k.cFDtFullString)
                          $ E10k.parseFDirCFG c

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
