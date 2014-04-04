import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn as Unicorn
import Dragonet.ProtocolGraph as PG
import Data.Graph.Inductive.Graph as DGI
import Data.Function
import Data.List as L
import Dragonet.DotGenerator

import Dragonet.Pipelines
import Dragonet.Pipelines.Implementation
import qualified LPGicmp as LPG1
import Control.Concurrent
import Control.Monad

rxtxPart :: PG.PGNode -> String
rxtxPart (_,n)
    | hasPref "Tx" = "SendPath"
    | hasPref "Rx" = "ReceivePath"
    | otherwise    = "Rest"
    where
        l = PG.nLabel n
        hasPref p = (==) p $ take (length p) l

main :: IO ()
main = do
    {-txt <- readFile "lpgImpl.unicorn"
    igraph <- UnicornAST.parseGraph txt
    let graph = Unicorn.constructGraph igraph-}
    --putStrLn $ show graph
    --putStrLn $ show $ partList rxtxPart $ DGI.labNodes graph
    let plg = generatePLG rxtxPart LPG1.lpg
    {-putStrLn $ show plg
    writeFile "pipelines.dot" $ pipelinesDot plg
    mapM_ (\pl ->
        writeFile ("pl_" ++ plLabel pl ++ ".dot") $ toDot $ plGraph pl
        ) $ map snd $ DGI.labNodes plg
    return ()-}
    runPipelines plg
    forever yield

