#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import DragonetDSL
import qualified Operations as OP
import DotGenerator as DG

[dragonet_f|lpgImpl.dragonet|]

main = do
    putStrLn (DG.toDot source)
