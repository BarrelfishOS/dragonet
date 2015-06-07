-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Unicorn               as U
import qualified Dragonet.ProtocolGraph         as PG
import Dragonet.Embedding (foldAndCleanupAll)
import Dragonet.DotGenerator (toDot, toDotHighlight)

import Control.Monad (forM_)

import Text.RawString.QQ (r)

g1 = U.strToGraph [r|
graph g1 {
    node A {
        port true[A1 _False]
        port false[Drop_]
    }

    node A1 {
        port true false[AValid]
    }

    node _False {
        port true[]
        port false[AValid]
    }

    and AValid {
        port true [A_OK]
        port false [Drop_]
    }

    node A_OK {
    }

    and Drop_ {
        port true[]
        port false[Drop]
    }

    node Drop {}
}
|]

main = do
    let isSink_ x = x `elem` ["Drop", "A_OK"]
        isSource_ x  = x `elem` ["A"]
    let gs = foldAndCleanupAll g1
               (isSource_ . PG.nLabel . snd)
               (isSink_ . PG.nLabel . snd)
    forM_ (zip [1..] gs) $ \(i,g) -> do
        let fname = "tests/cleanup-g1-" ++ (show i) ++ ".dot"
        writeFile fname $ toDot g
    --writeFile "tests/cleanup-g1-before.dot" $ toDot g1
    --writeFile "tests/cleanup-g1-after.dot" $ toDot g1'

    return ()
