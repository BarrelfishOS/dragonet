module Dragonet.Semantics (
    PortSemantics,
    Helpers,
    mergeHelpers
) where

import qualified SMTLib2 as SMT

type PortSemantics = SMT.Expr

type Helpers = SMT.Script

mergeHelpers :: Helpers -> Helpers -> Helpers
mergeHelpers (SMT.Script a) (SMT.Script b) = SMT.Script (a ++ b)



