module Util.GraphHelpers(
    filterCtx,
) where

import Data.Graph.Inductive

-- Return all contexts in graph where the specified predicate returned true
filterCtx :: Graph gr => (Context a b -> Bool) -> gr a b -> [Context a b]
filterCtx p g = ufold f [] g
    where
        f ctx l = if p ctx then ctx:l else l



