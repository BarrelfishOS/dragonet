-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Dragonet.Semantics.Solver (
    SolverM(..),
    Satisfiability(..),

    exprAndL,
    exprOrL
) where

import qualified Dragonet.Semantics as Sem

import Control.Monad.IO.Class (MonadIO)

data Satisfiability =
    Satisfiable |
    Unsatisfiable |
    Unknown
    deriving (Eq,Show)

class (Monad m, MonadIO m) => SolverM m e | m -> e where
    parseSem :: Sem.PortSemantics -> m e
    loadHelpers :: Sem.Helpers -> m ()
    isolated :: m a -> m a

    checkSat :: e -> m Satisfiability

    exprAnd :: e -> e -> m e
    exprOr :: e -> e -> m e
    exprNot :: e -> m e
    exprTrue :: m e
    exprFalse :: m e

exprAndL :: SolverM m e => [e] -> m e
exprAndL [] = error "exprAndL applied to empty list"
exprAndL [e] = return e
exprAndL (e:es) = do
    e' <- exprAndL es
    exprAnd e e'

exprOrL :: SolverM m e => [e] -> m e
exprOrL [] = error "exprAndL applied to empty list"
exprOrL [e] = return e
exprOrL (e:es) = do
    e' <- exprOrL es
    exprOr e e'


