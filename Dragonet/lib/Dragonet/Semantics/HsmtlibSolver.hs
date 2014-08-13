{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings #-}
module Dragonet.Semantics.HsmtlibSolver (
    Solver,
    runHsmtlibSolver,
    sAnd,
    sOr
) where

import qualified Dragonet.Semantics.Solver as S
import qualified Dragonet.Semantics as Sem

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTB
import qualified Hsmtlib as H
import qualified Hsmtlib.Solver as HS
import qualified Hsmtlib.Parsers.Syntax as HPS

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Control.Monad.IO.Class (liftIO,MonadIO)

data HSState = HSState {
        hsSolver :: HS.Solver
    }

newtype Solver a = Solver (ST.StateT HSState IO a)
    deriving (Monad,MonadIO)

instance ST.MonadState HSState Solver where
    get = Solver ST.get
    put s = Solver $ ST.put s

debug :: String -> Solver ()
debug _ = return ()

instance S.SolverM Solver SMT.Expr where
    parseSem ps = return ps

    loadHelpers (SMT.Script cmds) = do
        sol <- ST.gets hsSolver
        liftIO $ mapM_ (expectSuccess . runCmd sol) cmds

    isolated a = do
        sol <- ST.gets hsSolver
        HS.CGR HPS.Success <- liftIO $ HS.push sol 1
        x <- a
        HS.CGR HPS.Success <- liftIO $ HS.pop sol 1
        return x

    checkSat e = do
        debug $ "checkSat " ++ show e
        sol <- ST.gets hsSolver
        HS.CGR HPS.Success <- liftIO $ HS.push sol 1
        HS.CGR HPS.Success <- liftIO $ HS.assert sol e
        HS.CCS res <- liftIO $ HS.checkSat sol
        HS.CGR HPS.Success <- liftIO $ HS.pop sol 1
        let res' = trSatRes res
        debug $ "checkSat " ++ show res' ++ "  ->  " ++ show (SMT.pp e)
        return res'

    exprAnd a b = do
        debug $ "exprAnd (" ++ show a ++ ") (" ++ show b ++ ")"
        return $ sAnd a b
    exprOr a b = do
        debug $ "exprOr (" ++ show a ++ ") (" ++ show b ++ ")"
        return $ sOr a b
    exprNot a = return $ SMTC.not a
    exprTrue = return SMTC.true
    exprFalse = return SMTC.false


sAnd a b
    | SMTC.false `S.member` parts = SMTC.false
    | S.null cleaned = SMTC.true
    | otherwise = foldl1 SMTC.and $ S.toList cleaned
    where
        parts = fList "and" a `S.union` fList "and" b
        cleaned = parts S.\\ S.singleton SMTC.true

sOr a b
    | SMTC.true `S.member` parts = SMTC.true
    | S.null cleaned = SMTC.false
    | otherwise = foldl1 SMTC.or $ S.toList cleaned
    where
        parts = fList "or" a `S.union` fList "or" b
        cleaned = parts S.\\ S.singleton SMTC.false

fList :: SMT.Ident -> SMT.Expr -> S.Set SMT.Expr
fList f e@(SMT.App af Nothing es)
    | f == af = S.unions $ map (fList f) es
    | otherwise = S.singleton e
fList _ e = S.singleton e


trSatRes :: HPS.CheckSatResponse -> S.Satisfiability
trSatRes HPS.Sat = S.Satisfiable
trSatRes HPS.Unsat = S.Unsatisfiable
trSatRes _ = S.Unknown


runCmd :: HS.Solver -> SMT.Command -> IO HS.Result
runCmd s (SMT.CmdDeclareType n i)    = HS.declareType s n i
runCmd s (SMT.CmdDefineType n ps t)  = HS.defineType s n ps t
runCmd s (SMT.CmdDeclareFun n ps t)  = HS.declareFun s n ps t
runCmd s (SMT.CmdDefineFun n bs t e) = HS.defineFun s n bs t e
runCmd s (SMT.CmdAssert e)           = HS.assert s e

expectSuccess :: IO HS.Result -> IO ()
expectSuccess run = do
    HS.CGR HPS.Success <- run
    return ()


runHsmtlibSolver :: Solver a -> IO a
runHsmtlibSolver (Solver sol) = do
    liftIO $ putStrLn "Hsmtlib"
    hs <- H.startSolver HS.Z3 HS.Online HS.QF_AUFBV Nothing Nothing
    let hss = HSState { hsSolver = hs }
    (a,_) <- ST.runStateT sol hss
    HS.exit hs
    return a

