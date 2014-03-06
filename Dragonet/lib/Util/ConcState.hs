module Util.ConcState(
    ConcSM(..),
    getGS,
    putGS,
    getLS,
    putLS,
    fork,
) where

-- State monad executing operations in multiple threads
-- Includes a common instance of a global state and a per thread local state.
newtype ConcSM g l a = ConcSM { runConcSM :: (g,l) -> (g,[(a,l)]) }

instance Functor (ConcSM g l) where
    fmap f m = do { a <- m ; return (f a) }

instance Monad (ConcSM g l) where
    return a = ConcSM (\(g,l) -> (g, [(a,l)]))

    a >>= f = ConcSM (\(g,l) ->
        let (g',rs) = (runConcSM a) (g,l) in
        foldl (\(h,rs') (b,l') ->
            let (h',rs'') = (runConcSM $ f b) (h,l') in
            (h',rs' ++ rs'')) (g',[]) rs)

getGS :: ConcSM g l g
getGS = ConcSM (\(g,l) -> (g,[(g,l)]))

getLS :: ConcSM g l l
getLS = ConcSM (\(g,l) -> (g,[(l,l)]))

putGS :: g -> ConcSM g l ()
putGS g' = ConcSM (\(_,l) -> (g',[((),l)]))

putLS :: l -> ConcSM g l ()
putLS l' = ConcSM (\(g,_) -> (g,[((),l')]))

fork :: ConcSM g l l -> ConcSM g l ()
fork a = ConcSM (\(g,l) ->
    let (g',rs) = (runConcSM a) (g,l) in
    (g', concatMap (\(b,c) -> [((),b),((),c)]) rs))



