#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Unicorn
import qualified Operations as OP
import DotGenerator as DG

[unicorn_f|lpgImpl.unicorn|]

main = do
    putStrLn (DG.toDotClustered lpgClusters lpgNodes)
