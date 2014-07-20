module Graphs.Helpers (
    parseGraph
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn

-- Parse graph from unicorn file
parseGraph :: FilePath -> IO (PG.PGraph,Sem.Helpers)
parseGraph path = do
    b <- readFile path >>= UnicornAST.parseGraph
    return $ Unicorn.constructGraph' b

