{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot)

import Control.Monad (forM_)

import System.Environment (getArgs, getProgName)
import System.IO  (writeFile)

main :: IO ()
main = do

    xargs <- getArgs
    xprog <- getProgName

    if (length xargs) == 0
        then error $ "Usage: " ++ xprog ++ " unicorn_file(s)"
        else forM_ xargs $ \fname -> do

            let outname = fname ++ ".dot"
            putStrLn $ "Generating dot file for: " ++ fname

            txt   <- readFile fname
            graph <- UnicornAST.parseGraph txt
            let pgraph = Unicorn.constructGraph graph

            writeFile outname $ toDot pgraph
            return ()