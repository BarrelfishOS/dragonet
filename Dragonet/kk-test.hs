{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Dragonet.ProtocolGraph  as DragonetPG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import LPGImpl

import Text.Show.Pretty as Pr
import Dragonet.DotGenerator (toDot)

[unicornImpl_f|lpgImpl.unicorn|]

main :: IO ()
main = do
    --txt <- readFile "test.unicorn"
    txt <- readFile "lpgImpl.unicorn"
    graph <- UnicornAST.parseGraph txt
    writeFile "DELETEME-2.dot" $ toDot $ Unicorn.constructGraph graph
    writeFile "DELETEME-1.dot" $ toDot lpg
