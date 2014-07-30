module Graphs.Cfg (
    lpgCfg,
    prgCfgEmpty,
    prgCfg
) where

import qualified Dragonet.ProtocolGraph as PG

--------------------------------------------------------------------------------
-- Configurations

lpgCfg = [
    ("RxL4UDPCUDPSockets", PG.CVList [
            PG.CVTuple [
                PG.CVList [PG.CVInt 0],
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7 ],
            PG.CVTuple [
                PG.CVList [PG.CVInt 1],
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [
                PG.CVList [PG.CVInt 2],
                PG.CVMaybe $ Just $ PG.CVInt 1,
                PG.CVMaybe $ Just $ PG.CVInt 1337,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [
                PG.CVList [PG.CVInt 3],
                PG.CVMaybe $ Just $ PG.CVInt 5,
                PG.CVMaybe $ Just $ PG.CVInt 1338,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1234 ],
            PG.CVTuple [
                PG.CVList [PG.CVInt 4, PG.CVInt 5],
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
