-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

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



