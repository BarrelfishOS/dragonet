{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings #-}
module Dragonet.Semantics.Z3Solver (
    Solver,
    runZ3Solver
) where

import qualified Dragonet.Semantics.Solver as S
import qualified Dragonet.Semantics as Sem

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTB

import qualified Z3.Monad as Z3

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.State.Class as ST
import qualified Control.Monad.State.Strict as STS
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad (forM_)

data Context = Context {
    ctxSorts :: M.Map String Z3.Sort,
    ctxFunctions :: M.Map String FuncInfo
    }

emptyCtx = Context {
        ctxSorts = M.empty,
        ctxFunctions = M.empty
    }

data State = State {
    stContexts :: [Context],
    stExTrue :: Z3.AST,
    stExFalse :: Z3.AST,
    stBitVecs :: M.Map Int Z3.Sort
    }

data FuncInfo =
    Uninterpreted Z3.FuncDecl |
    Interpreted {
        ifiParams :: [String],
        ifiExpr :: SMT.Expr
        } |
    Builtin ([Z3.AST] -> Solver Z3.AST) |
    BuiltinUn (Z3.AST -> Solver Z3.AST) |
    BuiltinBin (Z3.AST -> Z3.AST -> Solver Z3.AST) |
    Constant Z3.AST

newtype Solver a = Solver (STS.StateT State Z3.Z3 a)
    deriving (Monad, MonadIO)

instance ST.MonadState State Solver where
    get = Solver ST.get
    put s = Solver $ ST.put s

instance Z3.MonadZ3 Solver where
    getSolver = Solver $ MT.lift $ Z3.getSolver
    getContext = Solver $ MT.lift $ Z3.getContext

instance S.SolverM Solver Z3.AST where
    parseSem = parseSMTExpr

    loadHelpers (SMT.Script cmds) =
        mapM_ runCmd cmds

    isolated a = do
        st <- ST.get
        ST.put st { stContexts = emptyCtx:(stContexts st) }
        x <- Z3.local $ a
        st' <- ST.get
        ST.put st { stContexts = tail $ stContexts st' }
        return x

    checkSat e = S.isolated $ do
        Z3.assertCnstr e
        res <- Z3.check
        return $ trSatRes res

    exprAnd a b = Z3.mkAnd [a, b]
    exprOr a b = Z3.mkOr [a, b]
    exprNot = Z3.mkNot
    exprTrue = ST.gets stExTrue
    exprFalse = ST.gets stExFalse


parseSMTExpr :: SMT.Expr -> Solver Z3.AST
parseSMTExpr (SMT.Lit (SMT.LitBV val width)) = do
    -- Can we do this in one step?
    i <- Z3.mkInt val
    Z3.mkInt2bv (fromIntegral width) i
parseSMTExpr (SMT.Lit (SMT.LitNum i)) = Z3.mkInt i
parseSMTExpr (SMT.App (SMT.I (SMT.N n) []) Nothing ps) = do
    mfi <- getFunDef n
    case mfi of
        Just fi -> do
            ps' <- mapM parseSMTExpr ps
            applyFunction fi ps'
        Nothing -> error $ "Function definition not found: " ++ n

applyFunction :: FuncInfo -> [Z3.AST] -> Solver Z3.AST
applyFunction (Uninterpreted fd) ps = Z3.mkApp fd ps
applyFunction (Constant e) [] = return e
applyFunction (Builtin f) ps = f ps
applyFunction (BuiltinUn f) [p] = f p
applyFunction (BuiltinBin f) [a,b] = f a b
applyFunction (Interpreted {
                    ifiParams = pns,
                    ifiExpr = expr }) ps =
    S.isolated $ do
        -- Add constants to context for parameter values
        forM_ (zip pns ps) $ \(n,e) ->
            addFunDef n $ Constant e
        -- Evaluate expression given this context
        parseSMTExpr expr

-- Lookup function definition by name
getFunDef :: String -> Solver (Maybe FuncInfo)
getFunDef name = do
    st <- ST.get
    let findFunDef (Just fd) _ = Just fd
        findFunDef Nothing ctx = M.lookup name $ ctxFunctions ctx
    return $ foldl findFunDef Nothing $ stContexts st

-- Lookup sort by name
getSort :: String -> Solver (Maybe Z3.Sort)
getSort name = do
    st <- ST.get
    let findSort (Just s) _ = Just s
        findSort Nothing ctx = M.lookup name $ ctxSorts ctx
    return $ foldl findSort Nothing $ stContexts st


-- Modify current context
modifyCtx :: (Context -> Context) -> Solver ()
modifyCtx fun = do
    st <- ST.get
    let (ctx:ctxs) = stContexts st
        st' = st { stContexts = (fun ctx):ctxs }
    ST.put st'

-- Add function definition to current context
addFunDef :: String -> FuncInfo -> Solver ()
addFunDef n fi = modifyCtx $ \ctx ->
    ctx { ctxFunctions = M.insert n fi $ ctxFunctions ctx }

-- Add sort definition to current context
addSort :: String -> Z3.Sort -> Solver ()
addSort n s = modifyCtx $ \ctx ->
    ctx { ctxSorts = M.insert n s $ ctxSorts ctx }

trSatRes :: Z3.Result -> S.Satisfiability
trSatRes Z3.Sat = S.Satisfiable
trSatRes Z3.Unsat = S.Unsatisfiable
trSatRes Z3.Undef = S.Unknown

parseSMTType :: SMT.Type -> Solver Z3.Sort
parseSMTType (SMT.TApp (SMT.I (SMT.N "BitVec") [bits]) []) =
    -- TODO: cache
    Z3.mkBvSort $ fromIntegral bits
parseSMTType (SMT.TApp (SMT.I (SMT.N n) []) []) = do
    Just s <- getSort n
    return s
parseSMTType tc = error $ "Unsupported type constructor: " ++ show tc


smtNameToS :: SMT.Name -> String
smtNameToS (SMT.N n) = n

runCmd :: SMT.Command -> Solver ()
runCmd (SMT.CmdDeclareType (SMT.N n) 0) = do
    sym <- Z3.mkStringSymbol n
    sort <- Z3.mkUninterpretedSort sym
    addSort n sort
runCmd (SMT.CmdDefineType (SMT.N n) [] t) = do
    sort <- parseSMTType t
    addSort n sort
runCmd (SMT.CmdDeclareFun (SMT.N n) ps t) = do
    sym <- Z3.mkStringSymbol n
    sps <- mapM parseSMTType ps
    st <- parseSMTType t
    fd <- Z3.mkFuncDecl sym sps st
    addFunDef n $ Uninterpreted fd
runCmd (SMT.CmdDefineFun (SMT.N n) bs t e) = do
    addFunDef n $ Interpreted {
            ifiParams = map (smtNameToS . SMT.bindVar) bs,
            ifiExpr = e
        }
runCmd (SMT.CmdAssert e) = do
    ast <- parseSMTExpr e
    Z3.assertCnstr ast

builtins :: [(String,FuncInfo)]
builtins = [
    ("=",        BuiltinBin Z3.mkEq),
    ("not",      BuiltinUn  Z3.mkNot),
    ("and",      Builtin    Z3.mkAnd),
    ("or",       Builtin    Z3.mkOr),
    ("distinct", Builtin    Z3.mkDistinct)
    ]

addBuiltins :: Context -> Context
addBuiltins ctx =
    ctx { ctxFunctions = ctxFunctions ctx `M.union` M.fromList builtins }

runZ3Solver :: Solver a -> IO a
runZ3Solver (Solver act) = Z3.evalZ3 $ do
    liftIO $ putStrLn "Z3"
    exTrue <- Z3.mkTrue
    exFalse <- Z3.mkFalse
    let st = State {
                stContexts = [addBuiltins $ emptyCtx],
                stExTrue = exTrue,
                stExFalse = exFalse,
                stBitVecs = M.empty
            }
    (a,_) <- STS.runStateT act st
    return a

