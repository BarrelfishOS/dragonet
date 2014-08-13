{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, UndecidableInstances #-}
module Dragonet.Semantics.Cache (
    Solver,
    runCacheSolver
) where

import qualified Dragonet.Semantics.Solver as S
import qualified Dragonet.Semantics.HsmtlibSolver as HS
import qualified Dragonet.Semantics as Sem

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTB

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.State.Class as ST
import qualified Control.Monad.State.Strict as STS
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad (forM_)



data State = State {
        stCache :: M.Map SMT.Expr S.Satisfiability
    }

data Expr e = Expr {
        eSMT  :: SMT.Expr,
        eBase :: e
    }

instance Show e => Show (Expr e) where
    show Expr { eSMT = s, eBase = e } = "Expr " ++ show (SMT.pp s) ++ " " ++ show e

newtype Solver m a = Solver (STS.StateT State m a)
    deriving (Monad, MonadIO)

instance (S.SolverM m e) => ST.MonadState State (Solver m) where
    get = Solver ST.get
    put s = Solver $ ST.put s


liftS :: (S.SolverM m e) => m a -> Solver m a
liftS act = Solver $ MT.lift act
debug _ = return ()
--debug s = liftIO $ putStrLn s

instance (S.SolverM m e) => S.SolverM (Solver m) (Expr e) where
    parseSem ps = do
        e <- liftS $ S.parseSem ps
        return $ Expr { eSMT = ps, eBase = e }

    loadHelpers scr = do
        st <- ST.get
        ST.put $ st { stCache = M.empty }
        liftS $ S.loadHelpers scr

    isolated (Solver a) = do
        st <- ST.get
        liftS $ S.isolated $ do
            (x,_) <- STS.runStateT a st
            return x

    checkSat Expr { eSMT = smt, eBase = e }= do
        debug $ "checkSat " ++ show smt

        st <- ST.get
        res <- case M.lookup smt $ stCache st of
            Nothing -> do
                debug "  Cache miss"
                sat <- liftS $ S.checkSat e
                ST.put $ st { stCache = M.insert smt sat $ stCache st }
                return sat
            Just sat -> do
                debug "  Cache hit"
                return sat
        debug $ "checkSat " ++ show res ++ "  ->  " ++ show (SMT.pp smt)
        return res

    exprAnd Expr { eSMT = sA, eBase = eA }
            Expr { eSMT = sB, eBase = eB } = do
        debug $ "exprAnd (" ++ show sA ++ ") (" ++ show sB ++ ")"
        let s = HS.sAnd sA sB
        e <- liftS $ S.exprAnd eA eB
        return $ Expr { eSMT = s, eBase = e }

    exprOr Expr { eSMT = sA, eBase = eA }
           Expr { eSMT = sB, eBase = eB } = do
        debug $ "exprOr (" ++ show sA ++ ") (" ++ show sB ++ ")"
        let s = HS.sOr sA sB
        e <- liftS $ S.exprOr eA eB
        return $ Expr { eSMT = s, eBase = e }

    exprNot Expr { eSMT = s, eBase = e } = do
        e' <- liftS $ S.exprNot e
        return $ Expr { eSMT = SMTC.not s, eBase = e' }

    exprTrue = do
        let s = SMTC.true
        e <- liftS $ S.exprTrue
        return $ Expr { eSMT = s, eBase = e }

    exprFalse = do
        let s = SMTC.false
        e <- liftS $ S.exprFalse
        return $ Expr { eSMT = s, eBase = e }


runCacheSolver :: (S.SolverM n f) => Solver n a -> n a
runCacheSolver (Solver act) = do
    let st = State { stCache =  M.empty }
    (a,_) <- STS.runStateT act st
    return a

