-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

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

