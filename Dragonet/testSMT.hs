import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.PrettyPrint (render)

--import Util.SMTLibParser
--import SMTLib2

import Dragonet.Semantics.Simplify
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot)


main = do
    [name] <- getArgs
    {-s <- parseScriptFile name
    case s of
        Left l -> do
            putStrLn $ show l
            exitFailure
        Right script -> putStrLn $ render $ pp script-}
    b <- readFile name >>= UnicornAST.parseGraph
    let (pgraph,helpers) = Unicorn.constructGraph' b
    writeFile "before.dot" $ toDot pgraph
    pg' <- reducePG pgraph helpers
    writeFile "after.dot" $ toDot pg'
