{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, toDotWith, pipelinesDot)
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import Util.GraphHelpers (findNodeByL)

import Control.Applicative --(Applicative)
import Control.Monad.State (State, MonadState, gets, modify, execState)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad ((>=>), liftM, mapM_, forM_, forever)
import Control.Concurrent (forkOS,yield)

--import Control.Applicative

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64, Word)
import Data.Char (ord)
import Data.Function (on)

import Foreign.Ptr ( FunPtr, castFunPtr )

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as AST.G
import qualified LLVM.General.AST.Type as AST.T
import qualified LLVM.General.AST.CallingConvention as AST.CC
import qualified LLVM.General.AST.Attribute as AST.Attr
import qualified LLVM.General.AST.Instruction as AST.I
import qualified LLVM.General.AST.Visibility as AST.V
import qualified LLVM.General.AST.Linkage as AST.L
import qualified LLVM.General.AST.Constant as AST.C
import qualified LLVM.General.AST.AddrSpace as AST.AS
import qualified LLVM.General.AST.Operand as AST.OP
import qualified LLVM.General.AST.Name as AST.N
import qualified LLVM.General.AST.IntegerPredicate as AST.IP

import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Mod
import qualified LLVM.General.ExecutionEngine as LLVM.EE
import qualified LLVM.General.Analysis as LLVM.A
import qualified LLVM.General.PassManager as LLVM.PM
--import LLVM.General.Target
--import LLVM.General.CodeModel
--import LLVM.General.Transforms
--import LLVM.General.Analysis

import qualified Text.Show.Pretty as Pr
import Debug.Trace (trace, traceShow)
import System.Environment (getArgs, getProgName)
import System.IO  (writeFile)

-- heavily based on: http://www.stephendiehl.com/llvm/

{-
  AST.Module helpers
-}

mkConstString :: String -> (AST.T.Type, AST.C.Constant)
mkConstString s = (ty, const)
    where s' = s ++ ['\0']
          len = toInteger $ length s'
          const = mkConstString_ len s'
          ty = mkStrTy $ fromInteger len

-- make a static var -- i.e., global with internal linkage
mkStaticVar :: AST.Type -> String -> AST.C.Constant -> AST.Definition
mkStaticVar v_ty v_name v_val = AST.GlobalDefinition $ AST.globalVariableDefaults {
          AST.G.name = AST.Name v_name
        , AST.G.linkage = AST.L.Internal
        , AST.G.type' = v_ty
        , AST.G.isConstant = False
        , AST.G.initializer = Just v_val
    }

mkFnStaticConst :: AST.Type -> String -> AST.C.Constant -> AST.Definition
mkFnStaticConst v_ty v_name v_val = AST.GlobalDefinition $ AST.globalVariableDefaults {
          AST.G.name = AST.Name v_name
        , AST.G.linkage = AST.L.Internal
        , AST.G.type' = v_ty
        , AST.G.isConstant = True
        , AST.G.initializer = Just v_val
    }

mkExternalFn :: AST.Type -> String -> [(AST.Type, String)] -> AST.Definition
mkExternalFn ret_ty label args = AST.GlobalDefinition $ AST.functionDefaults {
          AST.G.name = AST.Name label
        , AST.G.parameters = (params, False)
        , AST.G.returnType = ret_ty
        , AST.G.basicBlocks = []
    }
    where
        params = [AST.Parameter p_ty (AST.Name p_name) [] | (p_ty, p_name) <- args]


mkFnStaticConstStr :: String -> String -> AST.Definition
mkFnStaticConstStr s_name s_val = mkFnStaticConst s_ty s_name s_val'
    where (s_ty, s_val') = mkConstString s_val

mkGlobalConst :: AST.Type -> String -> AST.C.Constant -> AST.Definition
mkGlobalConst v_ty v_name v_val = AST.GlobalDefinition $
    AST.globalVariableDefaults {
          AST.G.name = AST.Name v_name
        , AST.G.linkage = AST.L.External
        , AST.G.type' = v_ty
        , AST.G.isConstant = True
        , AST.G.initializer = Just v_val
    }

mkGlobalConstStr :: String -> String -> AST.Definition
mkGlobalConstStr s_name s_val = mkGlobalConst s_ty s_name s_val'
    where (s_ty, s_val') = mkConstString s_val

getLocalRef :: String -> AST.Operand
getLocalRef arg = AST.OP.LocalReference (AST.Name arg)

mkNullOp :: AST.Type -> AST.Operand
mkNullOp ty = AST.OP.ConstantOperand $ AST.C.Null ty

-- return a null pointer to the given type
mkNullPtrOp :: AST.Type -> AST.Operand
mkNullPtrOp ty = mkNullOp $ mkPtrTy ty


{-
     LLVM state
-}

data ArgState = ArgState {
      pgState    :: (AST.Type, String)
    , pgInput    :: (AST.Type, String)
    , pgPLI      :: PLI.PipelineImpl
    , pgPLG      :: PL.PLGraph
}

data CodeGenState = CodeGenState {
      cgAstModule :: AST.Module
    , cgArgs      :: ArgState
    , cgONCnts    :: [String]
}

-- llvm monad (all llvm state)
newtype LLVM a = LLVM { doLLVM :: State CodeGenState a }
    deriving (Monad, MonadState CodeGenState, Functor)
    --deriving (Functor, Applicative, Monad, MonadState AST.Module)

----
-----

cgStateTy :: LLVM AST.Type
cgStateTy = do
    st <- gets cgArgs
    return $ fst (pgState st)

cgInputTy :: LLVM AST.Type
cgInputTy = do
    st <- gets cgArgs
    return $ fst (pgInput st)

cgONCounters :: LLVM [String]
cgONCounters = gets cgONCnts

cgAddONCounter :: String -> LLVM ()
cgAddONCounter n = do
    modify $ \s -> s { cgONCnts = n : (cgONCnts s) }


pipelineTy :: AST.Type
pipelineTy = mkOpaqueTy "struct.pipeline_handle"

queueTy :: AST.Type
queueTy = mkOpaqueTy "struct.queue_handle"

--- XXX: Assumption about input struct
cgInput_make_gep :: AST.OP.Operand -> String -> AST.I.Instruction
cgInput_make_gep op field = mkGep_ op [idx0, idx1]
    where idx0 = operandInt32_0
          idx1 = operandConstInt32 $ input_idx field
          input_idx :: String -> Integer
          input_idx "buff" = 0
          input_idx "buff_len" = 1
          input_idx x = error $ "Uknown field: " ++ x


cgFnodeArgs :: LLVM [(AST.Type, String)]
cgFnodeArgs = do
    --args <- gets cgArgs
    ArgState { pgState = (st_ty, st_str), pgInput = (in_ty, in_str) } <- gets cgArgs
    return $ [(mkPtrTy st_ty, st_str), (mkPtrTy in_ty, in_str)]

-- pg operator functions accept :
--  - val (boolean)
--  + f-nodes arguments
cgOpArgs :: LLVM [(AST.Type, String)]
cgOpArgs = do
    fnode_args <- cgFnodeArgs
    return $ (i1Ty, "val"):fnode_args

cgPLI :: LLVM PLI.PipelineImpl
cgPLI = pgPLI <$> gets cgArgs

cgPLG :: LLVM PL.PLGraph
cgPLG = pgPLG <$> gets cgArgs


----
-----

-- the second argument is an LLVM monad that does not return anything
codeGen :: String -> AST.Type -> AST.Type -> PLI.PipelineImpl -> PL.PLGraph -> LLVM () -> CodeGenState
codeGen mod_name state_ty input_ty pli plg llvm = (execState $ doLLVM llvm) st0
    where st0 = CodeGenState {
          cgAstModule = emptyModule mod_name
        , cgArgs      = ArgState {
                            pgState = (state_ty, "state"),
                            pgInput = (input_ty, "input"),
                            pgPLI = pli,
                            pgPLG = plg }
        , cgONCnts    = []
    }
pg_fnode_args_names = ["state", "input"] -- XXX keep hard-coded for now


-- get the name of a definition
defName :: AST.Definition -> String
defName (AST.GlobalDefinition (AST.G.GlobalVariable {AST.G.name = (AST.Name x) })) = x
defName (AST.GlobalDefinition (AST.G.Function {AST.G.name = (AST.Name x) })) = x
defName x = error "NYI: Add the appropriate pattern!"

-- check if there is a name conflict between two definitions
defConflict :: AST.Definition -> AST.Definition -> Bool
defConfclit d1@(AST.GlobalDefinition _) d2@(AST.GlobalDefinition _) = (defName d1) == (defName d2)
defConflict _ _ = False -- by default no conflict

-- add a definition to the LLVM state
addDefn :: AST.Definition -> LLVM ()
addDefn d = do
    mod <- gets cgAstModule
    let defs = AST.moduleDefinitions mod
    --defs <- gets AST.moduleDefinitions
    modify $ \s -> s { cgAstModule = mod { AST.moduleDefinitions = defs ++ [d] } }

-- helper to add an external function to llvm state
addExternalFn :: AST.Type -> String -> [(AST.Type, String)] -> LLVM ()
addExternalFn ret_ty label args = addDefn $ mkExternalFn ret_ty label args

addExternalFnVargs :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> LLVM ()
addExternalFnVargs ret_ty label arg_tys = addDefn $
    AST.GlobalDefinition $ AST.functionDefaults {
          AST.G.name = label
        , AST.G.parameters = (params, True)
        , AST.G.returnType = ret_ty
        , AST.G.basicBlocks = []
    }
    where
        params = [AST.Parameter p_ty p_name [] | (p_ty, p_name) <- arg_tys]

-- helper to add a function
addFn :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock] -> LLVM ()
addFn ret_ty label arg_tys bbs = addDefn $
    AST.GlobalDefinition $ AST.functionDefaults {
          AST.G.name = label
        , AST.G.parameters = (params, False)
        , AST.G.returnType = ret_ty
        , AST.G.basicBlocks = bbs
    }
    where
        params = [AST.Parameter p_ty p_name [] | (p_ty, p_name) <- arg_tys]

addGlobalVar :: AST.Type -> String -> LLVM ()
addGlobalVar v_ty v_name = addDefn $
    AST.GlobalDefinition $ AST.globalVariableDefaults {
          AST.G.name = AST.Name v_name
        , AST.G.linkage = AST.L.External
        , AST.G.type' = v_ty
        , AST.G.isConstant = False
        , AST.G.initializer = Nothing
    }

-- add a static var -- i.e., global with internal linkage
addStaticVar :: AST.Type -> String -> AST.C.Constant -> LLVM ()
addStaticVar v_ty v_name v_val = addDefn $ mkStaticVar v_ty v_name v_val

-- random haskell note: a point free version of this function is:
-- addGlobalConst = ((.) . (.) . (.)) addDefn mkGlobalConst (see: http://stackoverflow.com/questions/5821089/haskell-function-composition-operator-of-type-cd-abc-abd)
addGlobalConst :: AST.Type -> String -> AST.C.Constant -> LLVM ()
addGlobalConst ty name val = addDefn $ mkGlobalConst ty name val

addGlobalConstStr :: String -> String -> LLVM ()
addGlobalConstStr s_name s_val = addDefn $ mkGlobalConstStr s_name s_val

addTy :: String -> AST.Type -> LLVM ()
addTy name ty = addDefn $ AST.TypeDefinition (AST.Name name) (Just ty)

{-
    Function Builder State
-}

-- basic block state
data BBstate = BBstate {
      bbInstructions :: [AST.I.Named  AST.I.Instruction]
    , bbTerminator   :: Maybe (AST.I.Named AST.I.Terminator)
    , bbIdx          :: Int
} deriving Show

bbInit :: Int -> BBstate
bbInit idx = BBstate { bbInstructions = [], bbTerminator = Nothing, bbIdx = idx}

-- note that instructions are appended to the front
bbAppend :: AST.I.Named AST.I.Instruction -> BBstate -> BBstate
bbAppend ins old = old {bbInstructions = ins:(bbInstructions old)}

bbTerminate :: AST.I.Named AST.I.Terminator -> BBstate -> BBstate
bbTerminate t old = old { bbTerminator = Just t }

bbGetBasicBlock :: (AST.Name, BBstate) -> AST.BasicBlock
bbGetBasicBlock (bb_name, bb) = AST.BasicBlock bb_name bb_ins bb_term
    where bb_term =  case (bbTerminator bb) of
                           Just x -> x
                           Nothing -> error $ "block " ++ (show bb_name) ++ " is unterminated"
          bb_ins = reverse $ bbInstructions bb

-- state for function builder
--   (follows the kaleidosope llvm codegen)
data FnState = FnState {
      fnName     :: AST.Name                  -- function name
    , retTy      :: AST.Type                  -- return type
    , argsL      :: [(AST.Type, AST.Name)]    -- argument list
    , currentBB  :: AST.Name                  -- current basic block
    , bbsMap     :: M.Map AST.Name BBstate    -- basic blocks for this function
    , insCount   :: Word                      -- count for unnamed instructions
    , tmpCount   :: Int                       -- count for temporary variables
    , blockCnt   :: Int                       -- number of blocks
    , addedDefs  :: [AST.Definition]          -- added defintions
    , entryBB    :: String                    -- entry bb
    , phiMap     :: M.Map String (AST.Type, [(AST.Operand, String)]) -- a map to build phi instructions
} deriving Show

fnStateInit :: AST.Name -> AST.Type -> [(AST.Type, AST.Name)] -> FnState
fnStateInit name ret_ty args = FnState {
          fnName    = name
        , retTy     = ret_ty
        , argsL     = args
        , currentBB = AST.Name entry
        , bbsMap = M.singleton (AST.Name entry) $ bbInit 0
        , insCount = 0
        , tmpCount = 0
        , blockCnt = 0
        , addedDefs = []
        , entryBB = entry
        , phiMap = M.empty
    } where entry = "entry"

fnStateSetBB :: AST.Name -> FnState -> FnState
fnStateSetBB bbname st = case M.lookup bbname bbs_map of
        Just _ -> st { currentBB = bbname }
        Nothing -> error $ "No such block: " ++ show bbname
    where bbs_map = bbsMap st

-- add a definition
--   NOTE: the conflicting check does not seem to be needed since conflicts seem
--   to be handled by the AST
fnStateAddDef :: AST.Definition  -> FnState -> FnState
fnStateAddDef def st = case L.find (defConflict def) defs of
    Just x -> error $ "Definition " ++ (defName def) ++ " Conflicts. Existing definitions:" ++ (show $ map defName defs)
    Nothing -> st { addedDefs = def:defs }
    where defs = addedDefs st

-- return a list of basic blocks from FnState
--   the basic blocks are ordered by the index
fnStateBBs :: FnState -> [AST.BasicBlock]
fnStateBBs s = map bbGetBasicBlock $ sort_fn $ M.toList (bbsMap s)
    where sort_fn :: [(AST.Name, BBstate)] -> [(AST.Name, BBstate)]
          sort_fn = L.sortBy (compare `on` (bbIdx . snd))

fnStateAddFn :: FnState -> LLVM ()
fnStateAddFn x = do
    addFn (retTy x) (fnName x) (argsL x) (fnStateBBs x)
    mapM_ addDefn (addedDefs x)
    return ()


newtype FnBuilder a = FnBuilder { doFnBuilder :: State FnState a }
    deriving (Monad, MonadState FnState)

runFnBuilder :: AST.Name -> AST.Type -> [(AST.Type,AST.Name)] -> FnBuilder () -> FnState
runFnBuilder name ret_ty args bld = (execState $ doFnBuilder bld) st0
    where st0 = fnStateInit name ret_ty args

fnAdd_ :: AST.Name -> AST.Type -> [(AST.Type,AST.Name)] -> FnBuilder () -> LLVM ()
fnAdd_ name ret_ty args = fnStateAddFn . (runFnBuilder name ret_ty args)

fnAdd :: String -> AST.Type -> [(AST.Type, String)] -> FnBuilder () -> LLVM ()
fnAdd name ret_ty args = fnAdd_ name' ret_ty args'
    where name' = AST.Name name
          args' = map (\(ty,s) -> (ty, AST.Name s)) args

-- get a new UnName
bldUnName :: FnBuilder AST.N.Name
bldUnName = do
    i <- gets insCount
    modify $ \s -> s { insCount = i + 1 }
    return $ AST.N.UnName (i + 1)

-- get a new variable name
bldTemp :: FnBuilder AST.Name
bldTemp = do
    i <- gets tmpCount
    (AST.Name fname) <- gets fnName
    modify $ \s -> s { tmpCount = i + 1}
    return $ AST.Name $ fname ++ ".tmp_" ++ (show i)

-- get basic block by name
bldGetBB :: AST.Name -> FnBuilder BBstate
bldGetBB bbname = do
    bbs <- gets bbsMap
    case M.lookup bbname bbs of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show bbname

-- get current basic block
bldCurrentBB :: FnBuilder BBstate
bldCurrentBB = gets currentBB >>= bldGetBB

-- updates the state of the current building block
bldUpdateBB :: (BBstate -> BBstate) -> FnBuilder ()
bldUpdateBB bb_update = do
    bbname <- gets currentBB
    bb_old <- bldGetBB bbname
    let bb_new = bb_update bb_old
    modify $ \s -> s { bbsMap = M.insert bbname bb_new (bbsMap s) }


-- add a named instruction
bldAddNamedInstruction_ :: AST.Name -> AST.Instruction -> FnBuilder AST.Operand
bldAddNamedInstruction_ name instr = do
    bldUpdateBB $ bbAppend (name AST.I.:= instr)
    return $ AST.OP.LocalReference name

bldAddNamedInstruction name instr = bldAddNamedInstruction_ (AST.Name name) instr

-- add a instrction that gets an automatic name (unname)
bldAddInstruction :: AST.Instruction -> FnBuilder AST.Operand
bldAddInstruction instr = do
    name <- bldUnName
    bldAddNamedInstruction_ name instr

-- this is for instructions that do not return results (e.g., calls to functions
-- returning void)
bldDoInstruction :: AST.Instruction -> FnBuilder ()
bldDoInstruction instr = bldUpdateBB $ bbAppend (AST.I.Do instr)

bldTerminate_ :: AST.I.Named AST.I.Terminator -> FnBuilder ()
-- this works, but I don't think I'll understand it in a few weeks:
-- bldTerminate = bldUpdateBB . bbTerminate
bldTerminate_ t = bldUpdateBB $ bbTerminate t

bldTerminate :: AST.I.Terminator -> FnBuilder ()
bldTerminate i = bldTerminate_ (AST.I.Do i)

-- set current bassic block
bldSetBB :: AST.Name -> FnBuilder ()
bldSetBB = modify . fnStateSetBB

-- add a new basic block
bldAddBB_ :: AST.Name -> FnBuilder ()
bldAddBB_  bbname = modify $ \st ->
    let bbs_map = bbsMap st
        cnt_new = 1 + (blockCnt st)
        bb_new = bbInit cnt_new
    in case M.lookup bbname bbs_map of
    Just _ -> error $ "basic block already exists: " ++ show bbname
    Nothing -> st {
          currentBB = bbname
        , bbsMap = M.insert bbname bb_new bbs_map
        , blockCnt = cnt_new
    }

bldAddBB n = bldAddBB_ (AST.Name n)


bldAddDef :: AST.Definition -> FnBuilder ()
bldAddDef = modify . fnStateAddDef

-- add a new phi to the state
bldNewPhi :: AST.Type -> String -> FnBuilder ()
bldNewPhi phi_ty phi_str = modify $ \st ->
        let phi_map = phiMap st
            phi_new = (phi_ty, [])
        in case M.lookup phi_str phi_map of
            Just _ -> error $ "phi already exists: " ++ phi_str
            Nothing -> st { phiMap = M.insert phi_str phi_new phi_map }

bldAddPhi :: String -> (AST.Operand, String) -> FnBuilder ()
bldAddPhi phi_str (op, label_str) = modify $ \st ->
    let phi_map = phiMap st
        adj_fn = \(t,xs) -> (t, (op, label_str):xs)
    in case M.lookup phi_str phi_map of
        Just (_, xs) -> st { phiMap = M.adjust adj_fn phi_str phi_map }
        Nothing -> error $ "phi does not exist: " ++ phi_str

bldPhi :: String -> FnBuilder AST.Operand
bldPhi phi_str = do
    phi_map <- gets phiMap
    case M.lookup phi_str phi_map of
        Just (ty, args) -> bldAddInstruction $ mkPhi ty args
        Nothing -> error $ "phi does not exist: " ++ phi_str

{-
    Instruction helpers/builders
-}

mkCall :: AST.CallableOperand -> [AST.Operand] -> AST.Instruction
mkCall fn args = AST.Call {
          AST.I.isTailCall = False
        , AST.I.callingConvention = AST.CC.C
        , AST.I.returnAttributes = []
        , AST.I.function = fn
        , AST.I.arguments = args_
        , AST.I.functionAttributes = []
        , AST.I.metadata = []
    }
    where args_ = [ (x, []) | x <- args ]

-- helper for calling functions
mkFnCall :: String -> [AST.Operand] -> AST.Instruction
mkFnCall fname ops = mkCall (getFnOp fname) ops

mkICmpEq :: AST.Operand -> AST.Operand -> AST.Instruction
mkICmpEq op0 op1 = AST.ICmp {
          AST.I.iPredicate = AST.IP.EQ
        , AST.I.operand0 = op0
        , AST.I.operand1 = op1
        , AST.I.metadata = []
    }

mkLd :: AST.Operand -> AST.Instruction
mkLd addr = AST.I.Load {
          AST.I.volatile = False
        , AST.I.address = addr
        , AST.I.maybeAtomicity = Nothing
        , AST.I.alignment = 0 -- ABI alignment for the target
        , AST.I.metadata = []
    }

mkSt :: AST.Operand -> AST.Operand -> AST.Instruction
mkSt addr val = AST.I.Store {
          AST.I.volatile = False
        , AST.I.address = addr
        , AST.I.value   = val
        , AST.I.maybeAtomicity = Nothing
        , AST.I.alignment = 0 -- ABI alignment for the target
        , AST.I.metadata = []
    }

mkPhi :: AST.Type -> [(AST.Operand, String)] -> AST.Instruction
mkPhi ty vals = AST.I.Phi {
          AST.I.type' = ty
        , AST.I.incomingValues = map (\(o,s) -> (o, AST.Name s)) vals
        , AST.I.metadata = []
    }

mkAdd :: AST.Operand -> AST.Operand -> AST.Instruction
mkAdd op0 op1 = AST.I.Add {
          AST.I.nsw = False
        , AST.I.nuw = False
        , AST.I.operand0 = op0
        , AST.I.operand1 = op1
        , AST.I.metadata = []
    }

mkConstInt :: Word32 -> Integer -> AST.C.Constant
mkConstInt bits val = AST.C.Int { AST.C.integerBits = bits, AST.C.integerValue = val }

mkConstInt32 = mkConstInt 32
mkConstInt8  = mkConstInt 8

mkConstIntOperand ::  Word32 -> Integer -> AST.Operand
mkConstIntOperand bits val = AST.OP.ConstantOperand $ mkConstInt bits val

operandConstInt8 :: Integer -> AST.Operand
operandConstInt8 = mkConstIntOperand 8

operandInt8_0 = operandConstInt8 8

operandConstInt32 :: Integer -> AST.Operand
operandConstInt32 = mkConstIntOperand 32

operandInt32_0 = operandConstInt32 0
operandInt32_1 = operandConstInt32 1

operandConstInt64 :: Integer -> AST.Operand
operandConstInt64 = mkConstIntOperand 64

operandInt64_0 = operandConstInt64 0
operandInt64_1 = operandConstInt64 1

operandTrue  = mkConstIntOperand 1 1
operandFalse = mkConstIntOperand 1 0

mkConstChar :: Char -> AST.C.Constant
mkConstChar c = mkConstInt8 $ toInteger $ ord c

mkIntTy :: Word32 -> AST.T.Type
mkIntTy bits = AST.T.IntegerType { AST.T.typeBits = bits }

i64Ty = mkIntTy 64
i32Ty = mkIntTy 32
i8Ty  = mkIntTy 8
i1Ty  = mkIntTy 1
voidTy = AST.T.VoidType
muxIdTy = i32Ty

mkArrayTy :: AST.T.Type -> Word64 -> AST.T.Type
mkArrayTy elem_ty len  = AST.T.ArrayType {
          AST.T.nArrayElements = len
        , AST.T.elementType = elem_ty
    }

mkPtrTy :: AST.T.Type -> AST.T.Type
mkPtrTy ref_ty = AST.T.PointerType {
          AST.T.pointerReferent = ref_ty
        , AST.T.pointerAddrSpace = AST.AS.AddrSpace 0
    }

mkOpaqueTy :: String -> AST.T.Type
mkOpaqueTy str = AST.T.NamedTypeReference $ AST.Name str

mkStrTy :: Word64 -> AST.T.Type
mkStrTy = mkArrayTy $ mkIntTy 8

mkConstString_ :: Integer -> [Char] -> AST.C.Constant
mkConstString_ size val = AST.C.Array {
          AST.C.memberType = AST.T.IntegerType 8
        , AST.C.memberValues = val_
    }
    where val_ = map mkConstChar val


mkTermRet :: AST.OP.Operand -> AST.I.Terminator
mkTermRet op = AST.I.Ret {
          AST.I.returnOperand = Just op
        , AST.I.metadata' = []
    }

mkGep_ :: AST.OP.Operand -> [AST.Operand] -> AST.I.Instruction
mkGep_ addr idxs = AST.I.GetElementPtr {
          AST.I.inBounds = True
        , AST.I.address = addr
        , AST.I.indices = idxs
        , AST.I.metadata = []
    }

-- note: s/operandConstInt32/operandConstInt64 causes a segfault :/
mkGep :: AST.OP.Operand -> [Integer] -> AST.I.Instruction
mkGep addr idxs = mkGep_ addr $ map operandConstInt32 idxs

mkGep64 :: AST.OP.Operand -> [Integer] -> AST.I.Instruction
mkGep64 addr idxs = mkGep_ addr $ map operandConstInt64 idxs

mkAlloca :: AST.T.Type -> Integer -> AST.I.Instruction
mkAlloca ty nelements = AST.I.Alloca {
          AST.I.allocatedType = ty
        , AST.I.numElements = Just $ operandConstInt64 nelements
        , AST.I.alignment = 0 -- "the target can choose to allign the allocation on any convenient boundary compatible with the type"
        , AST.I.metadata = []
    }

-- allocate a buffer in the stack using alloca
mkAllocaBuff :: Integer -> AST.I.Instruction
mkAllocaBuff len = mkAlloca (mkArrayTy i8Ty (fromInteger len)) 1

-- get a constant operand of a function to call it
--  (i.e., this is the first argument of mkCall)
getFnOp :: String -> AST.CallableOperand
getFnOp fn_name = Right $ AST.OP.ConstantOperand $ AST.C.GlobalReference (AST.Name fn_name)

getGlobalOp :: String -> AST.OP.Operand
getGlobalOp str_name = AST.OP.ConstantOperand $ AST.C.GlobalReference (AST.Name str_name)

--add a function that just prints its name
addDummyFn :: String -> LLVM ()
addDummyFn fn_name = do
    addGlobalConstStr str_name fn_name
    addFn voidTy (AST.Name fn_name) [] [bb]
    return ()
    where str_name = fn_name ++ ".name__"   -- string variable name
          name_op = getGlobalOp str_name -- function name operand
          bb_term = AST.I.Do termRetVoid    -- terminal instruction of basic block
          gep_name = AST.Name "gep"
          bb = AST.BasicBlock (AST.Name "bb0") bb_ins bb_term

          gep = gep_name AST.I.:= (mkGep name_op [0,0])
          call = AST.I.Do $ mkCall (getFnOp "puts") [AST.OP.LocalReference $ gep_name]

          bb_ins = [gep, call]

termRetVoid :: AST.I.Terminator
termRetVoid = AST.I.Ret { AST.I.returnOperand = Nothing, AST.I.metadata' = [] }

termRetOp :: AST.OP.Operand -> AST.I.Terminator
termRetOp op = AST.I.Ret { AST.I.returnOperand = Just op, AST.I.metadata' = [] }

termBr_ :: AST.Name -> AST.I.Terminator
termBr_ dest = AST.I.Br { AST.I.dest = dest, AST.I.metadata' = [] }

termBr x = termBr_ $ AST.Name x

termCondBr_ :: AST.OP.Operand -> AST.Name -> AST.Name -> AST.I.Terminator
termCondBr_ op true_dst false_dst = AST.I.CondBr {
          AST.I.condition = op
        , AST.I.trueDest = true_dst
        , AST.I.falseDest = false_dst
        , AST.I.metadata' = [] }

termCondBr :: AST.OP.Operand -> String -> String -> AST.I.Terminator
termCondBr op true_dst false_dst = termCondBr_ op (AST.Name true_dst) (AST.Name false_dst)

termSwitch_ :: AST.OP.Operand -> AST.Name -> [(AST.C.Constant,AST.Name)] -> AST.I.Terminator
termSwitch_ op def_dest dests = AST.I.Switch {
          AST.I.operand0' = op
        , AST.I.defaultDest = def_dest
        , AST.I.dests = dests
        , AST.I.metadata' = [] }

termSwitch :: AST.OP.Operand -> String -> [(AST.C.Constant, String)]  -> AST.I.Terminator
termSwitch op  def_dest dests = termSwitch_ op  def_dest' dests'
    where def_dest' = AST.Name def_dest
          dests' = map (\(c,s) -> (c, AST.Name s)) dests

testFnBB :: [AST.BasicBlock]
testFnBB = []

emptyModule :: String -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }


-- call a function
llvm_simple :: LLVM ()
llvm_simple = do
    addFn AST.T.VoidType (AST.Name "foo") [] []
    addGlobalVar i32Ty "i"
    addGlobalConst i32Ty "j" (mkConstInt32 42)
    addGlobalConstStr "hello" "Hello World!"

llvm_hello :: LLVM ()
llvm_hello = do
    addGlobalConstStr "hello" "Hello World!"
    addExternalFn i32Ty "puts" [((mkPtrTy i8Ty), "str")]
    addFn voidTy (AST.Name "callme") [] [bb0]
    addDummyFn "ilovepizza"
    return ()
    where bb0 = AST.BasicBlock bb_name bb_ins bb_term
          bb_name = AST.Name "bb0"
          --bb_i0   = mkCall hello_str

          hello_operand = getGlobalOp "hello"
          gep_ins = mkGep hello_operand [0,0]

          -- apparently you can have infix type constructors ...
          gep_named = (AST.Name "gep") AST.I.:= gep_ins
          gep_operand = AST.OP.LocalReference $ (AST.Name "gep")

          call_puts = mkCall (getFnOp "puts") [gep_operand]
          call_puts_named :: AST.I.Named AST.I.Instruction
          call_puts_named = AST.I.Do call_puts


          bb_ins  = [gep_named, call_puts_named]
          bb_term = AST.I.Do termRetVoid

{-
   Implementing Dragonet's protocol graph using LLVM
-}


-- type for global state that is passed between functions
--   HAS TO BE KEPT CONSISTENT w/ c-impl header file
pg_state_ty_ :: AST.Type
pg_state_ty_ = AST.T.StructureType {
      AST.T.isPacked = False
    , AST.T.elementTypes = [  i64Ty             -- pkt_counter
                            , (mkPtrTy i8Ty)    -- place_holder
                           ]
}

-- type for the input of this particular execution
--   HAS TO BE KEPT CONSISTENT w/ c-impl header file
pg_input_ty_ :: AST.Type
pg_input_ty_ = AST.T.StructureType {
      AST.T.isPacked = False
    , AST.T.elementTypes = [  (mkPtrTy i8Ty)     -- buffer (data)
                            , i64Ty              -- buffer size (len)
                            -- XXX for changes above this line, have a look at pg_input_idx XXX --
                            , i64Ty              -- space before
                            , i64Ty              -- space after
                            , mkArrayTy i64Ty 30 -- attrs
                           ]
}


-- return the name of the function corresponding to a node
pg_fname :: PG.Node -> String
pg_fname node = "pg__" ++ (nLabel node)

-- rethrn the name of the implementation function corresponding to a node
pg_fimplname :: PG.Node -> String
pg_fimplname n = "do_" ++ (pg_fname n)

-- pg functions for f-nodes accept:
--  - a state argument  (pointer to pg_state_ty_)
--  - buff     (packet buffer)
--  - buff_len
pg_fnode_args_ty_ :: [AST.Type]
pg_fnode_args_ty_  = [(mkPtrTy pg_state_ty_), (mkPtrTy pg_input_ty_)]

-- pg functions return an integer value:
--    0 -> cont: continue computation
--    1 -> done: reached end node
--
-- NB: An alternative implementation for detecting termination would be to
-- use some sort of exception mechanism. For example, upon entry to the
-- the graph the equivalent of a setjmp() could be called, and then have
-- the terminator node do a longjmp().
-- NB #2 : If we keep state in the operator nodes (as we do now), this might be
-- a bit tricky because we need to reset it.
pg_fn_ret_ty :: AST.Type
pg_fn_ret_ty = i32Ty

pgContOp = operandInt32_0
pgDoneOp = operandInt32_1
pgErrOp  = operandConstInt32 (-1)

pgContC = mkConstInt32 0
pgDoneC = mkConstInt32 1
pgErrC = mkConstInt32 (-1)

-- builds a pg function for the various different node types
llvm_pg_fn :: (PGAdjFull, DGI.Node, PG.Node, PGAdjFull) -> LLVM ()
llvm_pg_fn (edges_in, nid, node, edges_out) = case PG.nPersonality node of
    PG.FNode           -> llvm_pg_fnode nid node edges_out
    PG.ONode PG.OpAnd  -> llvm_pg_op  and_cnf nid node edges_out edges_in
    PG.ONode PG.OpOr   -> llvm_pg_op   or_cnf nid node edges_out edges_in
    PG.ONode PG.OpNAnd -> llvm_pg_op nand_cnf nid node edges_out edges_in
    PG.ONode PG.OpNOr  -> llvm_pg_op  nor_cnf nid node edges_out edges_in
    PG.CNode _         -> error "What is a CNode doing here?!"
    where or_cnf   = LlvmPgOpConf {shortCircuitIn = True,  shortCircuitOut = True}
          and_cnf  = LlvmPgOpConf {shortCircuitIn = False, shortCircuitOut = False}
          nand_cnf = LlvmPgOpConf {shortCircuitIn = True, shortCircuitOut  = False}
          nor_cnf  = LlvmPgOpConf {shortCircuitIn = False, shortCircuitOut = True}

-- create a dummy  implementation that always returns the value of the node's
-- label in the map, or 0 by default
llvm_pg_dummy_impl :: M.Map String Integer ->  (PGAdjFull, DGI.Node, PG.Node, PGAdjFull) -> LLVM ()
llvm_pg_dummy_impl def_map (edges_in, nid, node, edges_out) = case PG.nPersonality node of
    PG.FNode           -> llvm_dummy_impl def_val nid node edges_out
    PG.CNode _         -> error "What is a CNode doing here?!"
    _                  -> return ()
    where def_val = M.findWithDefault 0 nlbl def_map
          nlbl  = nLabel node

-- dummy implementation: always return def_val
llvm_dummy_impl :: Integer -> DGI.Node -> PG.Node -> PG.PGAdjFull -> LLVM ()
llvm_dummy_impl def_val nid node adj_out = do
    fnode_args <- cgFnodeArgs
    fnAdd (pg_fimplname node) i32Ty fnode_args $ do
        bldTerminate $ termRetOp $ operandConstInt32 def_val

bldConstStr :: String -> String -> FnBuilder AST.OP.Operand
bldConstStr str_name str_data = do
    (AST.Name fname) <- gets fnName
    let str_name' = fname ++ "." ++ str_name
    bldAddDef $ mkFnStaticConstStr str_name' str_data
    let str_op = getGlobalOp str_name'
    bldAddInstruction $ mkGep str_op [0,0]

-- Get pointer to the provided constant string
--   differs from bldConstStr by using temporary variable name
bldConstStr' :: String -> FnBuilder AST.Operand
bldConstStr' str = do
    (AST.Name tmp) <- bldTemp
    bldAddDef $ mkFnStaticConstStr tmp str
    bldAddInstruction $ flip mkGep [0,0] $ getGlobalOp tmp

-- TODO: use bldConstStr
bldPrintMsg :: String -> FnBuilder ()
bldPrintMsg msg = do
    -- first add the definition for the string
    (AST.Name fname) <- gets fnName
    let msgname = fname ++ ".msg"
    bldAddDef $ mkFnStaticConstStr msgname msg
    -- next, add code to print the message
    let msg_op = getGlobalOp msgname
    gep <- bldAddInstruction $ mkGep msg_op [0,0]
    bldAddInstruction $ mkCall (getFnOp "puts") [gep]
    return ()

-- TODO: use bldConstStr
bldPrintf :: String -> [AST.Operand] -> FnBuilder ()
bldPrintf str args = do
    -- add string
    (AST.Name tmp) <- bldTemp
    bldAddDef $ mkFnStaticConstStr tmp str
    -- call printf
    let tmp_op = getGlobalOp tmp
    gep <- bldAddInstruction $ mkGep tmp_op [0,0]
    bldAddInstruction $ mkCall (getFnOp "printf") ([gep] ++ args)
    return ()

bldDbgPrintf :: String -> [AST.Operand] -> FnBuilder ()
bldDbgPrintf str args = do
    --bldPrintf str args
    return ()

-- call a remote node
bldCallNode :: PG.Node -> String -> FnBuilder AST.Operand
bldCallNode node port = do
    let args = map getLocalRef pg_fnode_args_names
    let call_op = getFnOp $ pg_fname node
    bldAddInstruction $ mkCall call_op $ case (PG.nPersonality node, port) of
        (PG.FNode, _)         -> args
        (PG.ONode _, "true")  -> operandTrue:args
        (PG.ONode _, "false") -> operandFalse:args
        (PG.ONode _, _)       -> error $ "No idea how to call port " ++ port ++ " of operator node " ++ (show node)
        (PG.CNode _, _)       -> error $ "What is a CNode doing here?!"

-- call a set of remote nodes of a given port
-- associative list
--  bb_prefix : resulting basic blocks will be: bb_prefix ++ show idx
--  bb_cont   : jump there when all functions where called and returned CONT
--  bb_done   : jump there when any function returns DONE
--  bb_err    : jump there when an error happens (any function returns != DONE | CONT)
-- If any of the above keys is not present, the key will be used instead as a value
bldCallPort :: (PG.Port, [(DGI.Node, PG.Node)]) -> [(String,String)] -> FnBuilder ()
bldCallPort (port, outs) bb_assoc = do
    forM_ [0..(length outs)-1] $ \idx -> do
        let (nid, node) = outs !! idx
        -- add the basic block
        bldAddBB $ bb_name idx
        -- call the function of the connected node
        bldDbgPrintf ("Calling " ++ (pg_fname node) ++ "\n") []
        ret <- bldCallNode node port
        bldDbgPrintf ((pg_fname node) ++ " returned:%d\n") [ret]
        let bb_cont = if (idx + 1 == length outs)
            then get_bb "bb_cont"
            else bb_name $ idx + 1
        bldTerminate $ termSwitch ret bb_err [(pgContC, bb_cont), (pgDoneC, bb_done)]
    return () :: FnBuilder()
    where bb_done = get_bb "bb_done"
          get_bb = \k -> case (L.lookup k bb_assoc) of { Nothing -> k; Just x  -> x }
          bb_name = \idx -> (get_bb "bb_prefix") ++ (show idx)
          bb_err  = get_bb "bb_err"

-- Returns value of a named attribute if it exists
--   (attr "foo=bar" + name "foo" ->  Just "bar"
getPGNAttr :: PG.Node -> String -> Maybe String
getPGNAttr node n =
    drop (length n + 1) <$> (L.find (L.isPrefixOf (n ++ "=")) $ nAttributes node)

-- build a function for an F-node
--  distinguish between terminal and non-terminal nodes
llvm_pg_fnode :: DGI.Node -> PG.Node -> PG.PGAdjFull -> LLVM ()
llvm_pg_fnode nid node adj_out = do
    -- group outputs by port
    let outs' = PG.pgGroupAdjFull adj_out

    -- sort the out ports by the order they appear in the node
    let f = \x -> fromJust $ L.elemIndex x (PG.nPorts node)
    let outs = L.sortBy (compare `on` (f . fst)) outs'

    if length outs > 0 then llvm_pg_fnode_nonterminal nid node outs
    else llvm_pg_fnode_terminal nid node outs

-- build a function for a terminal F-node
llvm_pg_fnode_terminal :: DGI.Node -> PG.Node -> [(PG.Port, [(DGI.Node, PG.Node)])] -> LLVM ()
llvm_pg_fnode_terminal nid node outs = do
    -- If we're dealing with an enqueue node, generate its implementation
    external <- case getPGNAttr node "pipeline" of
        Just pln -> do { llvm_pg_fnode_enqueue nid node pln ; return False }
        _ -> return True
    fnode_args <- cgFnodeArgs
    fnAdd (pg_fname node) i32Ty fnode_args $ do
        -- declare the implementation function
        if external
            then bldAddDef $ mkExternalFn i32Ty (pg_fimplname node) fnode_args
            else return ()
        -- print message on entering the function
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering terminal node: " ++ fnstr) []
        -- set up a phi node for the return value
        bldNewPhi i32Ty "ret_phi"
        -- call implementation function
        impl_ret <- bldAddInstruction $ mkCall (getFnOp $ pg_fimplname node)
                                               (map getLocalRef pg_fnode_args_names)

        -- FIXME:
        -- For now we assume that all structurally terminal nodes terminate
        -- the computation. This, however, might not be correct. We need to
        -- make this decision based on the return value of the
        -- implementation function
        bldTerminate $ termBr "bb_done"

        forM_ [("bb_cont", pgContOp), ("bb_done", pgDoneOp), ("bb_err", pgErrOp)] $ \(bb,retval) -> do
            bldAddBB bb
            bldAddPhi "ret_phi" (retval, bb)
            bldTerminate $ termBr "bb_ret"

        bldAddBB "bb_ret"
        ret <- bldPhi "ret_phi"
        bldTerminate $ termRetOp ret

-- Handle non-terminal F-Node. Cases:
--   - Demux nodes
--   - regular F-Node
llvm_pg_fnode_nonterminal :: DGI.Node -> PG.Node -> [(PG.Port, [(DGI.Node, PG.Node)])] -> LLVM ()
llvm_pg_fnode_nonterminal nid node outs = do
    -- Generate impl function for demultiplexing node
    isDemux <- if nLabel node == "Demux"
        then do { llvm_pg_fnode_demux nid node ; return True }
        else return False
    -- Generate impl function for multiplexing node
    isMux <- case (getPGNAttr node "multiplex",getPGNAttr node "muxPL") of
        (Just aDN,Just aDP) -> do
            llvm_pg_fnode_mux nid node aDN aDP
            return True
        _ -> return False
    -- Generate Fnode function
    llvm_pg_fnode_nonterminal' nid node outs (not (isDemux || isMux))

-- Generate function implementing a demux F-node
--   - Poll the queue until we get an input
--   - Enable the next node, based on the mux identifier
llvm_pg_fnode_demux :: DGI.Node -> PG.Node -> LLVM ()
llvm_pg_fnode_demux nid node = do
    input_ty <- cgInputTy
    fnode_args <- cgFnodeArgs
    fnAdd (pg_fimplname node) i32Ty fnode_args $ do
        -- print message on entering the function
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering demux node: " ++ fnstr ++ "\n") []
        bldTerminate $ termBr "bb_poll"

        -- Basic Block for polling queue
        bldAddBB "bb_poll"
        plh <- bldAddInstruction $ mkLd $ getGlobalOp glblPipeline
        poll_ret <- bldAddInstruction $ mkFnCall "pl_poll" [plh]
        t <- bldAddInstruction $ mkICmpEq poll_ret (mkNullPtrOp input_ty)
        bldTerminate $ termCondBr t "bb_poll" "bb_ret"

        bldAddBB "bb_ret"
        mux_ret <- bldAddInstruction $ mkFnCall "input_muxid" [poll_ret]
        -- clear mux id
        bldAddInstruction $ mkFnCall "input_set_muxid" [poll_ret, operandInt32_0]
        -- exchange with current input, and free it
        let inp = getLocalRef "input"
        bldAddInstruction $ mkFnCall "input_xchg" [inp, poll_ret]
        bldAddInstruction $ mkFnCall "input_free" [poll_ret]
        bldTerminate $ termRetOp mux_ret

-- Generate function transfering packet to alternative pipeline
llvm_pg_fnode_enqueue :: DGI.Node -> PG.Node -> String -> LLVM ()
llvm_pg_fnode_enqueue nid node pipelineName = do
    input_ty <- cgInputTy
    fnode_args <- cgFnodeArgs
    pli <- cgPLI
    fnAdd (pg_fimplname node) i32Ty fnode_args $ do
        -- print message on entering the function
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering enqueue node: " ++ fnstr ++ "\n") []

        -- get queue handle
        let Just (PLI.POQueue queueName) = lookup pipelineName $ PLI.pliOutQs pli
        qh <- bldAddInstruction $ mkLd $ getGlobalOp $ glblOutqueue queueName
        -- enqueue packet
        let inp = getLocalRef "input"
        poll_ret <- bldAddInstruction $ mkFnCall "pl_enqueue" [qh, inp]
        bldTerminate $ termRetOp operandInt32_0

-- Generate function implementing a multiplexing F-node
--   This means tagging the packet with the right mux id
llvm_pg_fnode_mux :: DGI.Node -> PG.Node -> String -> String -> LLVM ()
llvm_pg_fnode_mux nid node dNodeL dPL = do
    -- First we need to figure out the multiplexing identifier, which we will
    -- make to be the port identifier so it can just be returned by the
    -- implementation fuction
    plg <- cgPLG
    let (Just dpg) = PL.plGraph <$> snd <$> findNodeByL ((==) dPL . PL.plLabel) plg -- dest pipeline
    --let (Just (dn,_))  = findNodeByL ((==) dNodeL . PG.nLabel) dpg -- Dest node
    let (Just (dxn,dxnL))  = findNodeByL ((==) "Demux" . PG.nLabel) dpg -- Demux node
    let dxPorts = flip zip [0..] $ PG.nPorts dxnL -- get map output port -> ID
    --let (Just (_,dxPort)) = find ((==) dn . fst) DGI.lsuc -- find outgoing edge from Demux node
    let (Just muxid) = lookup dNodeL dxPorts

    -- Generate the actual function
    input_ty <- cgInputTy
    fnode_args <- cgFnodeArgs
    fnAdd (pg_fimplname node) i32Ty fnode_args $ do
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering mux node: " ++ fnstr ++ "\n") []
        let inp = getLocalRef "input"
        bldAddInstruction $ mkFnCall "input_set_muxid" [inp, operandConstInt32 muxid]
        bldTerminate $ termRetOp operandInt32_0


-- build a function for a non-terminal F-node
-- NOTE:
--  The functionality of each f-node is implemented via an implementation
--  function that is called in the pg function of the f-node
--  The implementation function:
--   - takes the same arguments as the pg function
--   - returns an integer that corresponds to the port to be enabled
llvm_pg_fnode_nonterminal' :: DGI.Node -> PG.Node -> [(PG.Port, [(DGI.Node, PG.Node)])] -> Bool -> LLVM ()
llvm_pg_fnode_nonterminal' nid node outs' external = do
    -- XXX quick-n-dirty hack: if this is a boolean node, order ports so that
    -- false is first
    let attrs = PG.nAttributes node
    let getp = \x -> case L.lookup x outs' of {Just x -> x;  Nothing -> [] }
    let outs = case L.elem "Boolean" attrs of
                    --True  -> L.sortBy (compare `on` fst) outs'
                    True -> [  ("false", getp "false")
                             , ("true",  getp "true") ]
                    False -> outs'
    --
    fnode_args <- cgFnodeArgs
    fnAdd (pg_fname node) i32Ty fnode_args $ do
        -- declare the implementation function
        if external
            then bldAddDef $ mkExternalFn i32Ty (pg_fimplname node) fnode_args
            else return ()
        -- print message on entering the function
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering node: " ++ fnstr ++ "\n") []
        -- set up a phi node for the return value
        bldNewPhi i32Ty "ret_phi"
        -- call implementation function
        bldDbgPrintf ("Calling implementation function: " ++ (pg_fimplname node) ++ "\n") []
        impl_ret <- bldAddInstruction $ mkCall (getFnOp $ pg_fimplname node)
                                               (map getLocalRef pg_fnode_args_names)
        bldDbgPrintf ((pg_fimplname node) ++ " returned:%d\n") [impl_ret]
        -- bbs0: the first building block name for each port
        let bbs0 = map (\(port,_) -> ("bb_" ++ port ++ "0")) outs
        -- each port is assigned an integer starting from 0
        -- NB: We need to make sure that the order is maintained in the
        --     implementation function
        let dests = zip (map mkConstInt32 [0..]) bbs0
        -- build an LLVM switch statement that jumps to the first building
        -- block for each port based on the return value of the
        -- implementation function
        bldTerminate $ termSwitch impl_ret "bb_err" dests
        -- for each port, call edge_bb to build the edges's basic block
        forM_ outs $ \out@(port,nodes) -> do
            if length nodes > 0
            then do
                bldCallPort out [("bb_prefix", "bb_" ++ port)]
                return()
            else do
                -- port is not connected
                bldAddBB $ "bb_" ++ port ++ "0"
                bldTerminate $ termBr "bb_done"
                return ()

        forM_ [("bb_cont", pgContOp), ("bb_done", pgDoneOp), ("bb_err", pgErrOp)] $ \(bb,retval) -> do
            bldAddBB bb
            bldAddPhi "ret_phi" (retval, bb)
            bldTerminate $ termBr "bb_ret"

        bldAddBB "bb_ret"
        ret <- bldPhi "ret_phi"
        bldTerminate $ termRetOp ret

-- configure code generation for logical operator:
--  shortCircuitIn: what input value is short-circuited
--  shortCircuitOut: what output value is enabled when a short-circuit happens
data LlvmPgOpConf = LlvmPgOpConf {shortCircuitIn :: Bool, shortCircuitOut :: Bool} deriving Show

llvm_pg_op :: LlvmPgOpConf -> DGI.Node -> PG.Node -> PG.PGAdjFull -> PG.PGAdjFull -> LLVM ()
llvm_pg_op cnf nid node adj_out adj_in = do
    -- NOTE: We need to somehow reset counters for O-nodes where not every input
    -- port was enabled and no short-cirtuiting happens. The current approach is
    -- just to reset all node counters eagerly when done executing the graph for
    -- one packet.
    -- Another option would be to do this lazily, by keeping a version ID on
    -- each counter plus one global version.
    let nName = pg_fname node
        cntstr = nName ++ ".count"
    cgAddONCounter cntstr
    ops_args <- cgOpArgs
    fnAdd nName i32Ty ops_args $ do
        -- print message on entering the function
        (AST.Name fnstr) <- gets fnName
        bldDbgPrintf ("Entering node: " ++ fnstr ++ "\n") []
        -- set up a count variable: how many times this node was activated
        bldAddDef $ mkStaticVar i32Ty cntstr (mkConstInt32 0)
        -- set up a phi node for the return value
        bldNewPhi i32Ty "ret_phi"

        -- depedending on whether we are short-circuiting the true or the false
        -- input, generate the proper jump to the enabled bb (short-circuit) or
        -- to the check bb
        bldTerminate $ case shortCircuitIn cnf of
            True  -> termCondBr (getLocalRef "val") "bb_enabled" "bb_check"
            False -> termCondBr (getLocalRef "val") "bb_check" "bb_enabled"

        -- bb_check: check if count is completed
        bldAddBB "bb_check"
        cnt_ <- bldAddInstruction $ mkLd (getGlobalOp cntstr)
        cnt <- bldAddInstruction $ mkAdd cnt_ operandInt32_1
        bldAddInstruction $ mkSt (getGlobalOp cntstr) cnt
        -- we divide by two because we need to acount for both true/false ports
        let ins = operandConstInt32 $ toInteger $ quot (length adj_in) 2
        bldDbgPrintf (" -> " ++ fnstr ++ "count=%d (out of %d)\n") [cnt, ins]
        t <- bldAddInstruction $ mkICmpEq cnt ins
        bldTerminate $ termCondBr t "bb_enabled" "bb_ret"
        bldAddPhi "ret_phi" (pgContOp, "bb_check")

        -- bb_enabled: zero out count, and jump to bb_true0 or bb_false0
        bldAddBB "bb_enabled"
        entry <- gets entryBB
        -- short_circ is true if we came from the entry basic block, or false if
        -- we came from the bb_check
        short_circ <- bldAddInstruction $ mkPhi i1Ty [(operandTrue, entry), (operandFalse, "bb_check")]
        bldAddInstruction $ mkSt (getGlobalOp cntstr) operandInt32_0
        bldTerminate $ case shortCircuitOut cnf of
            True -> termCondBr short_circ "bb_true0" "bb_false0"
            False -> termCondBr short_circ "bb_false0" "bb_true0"

        -- code for enabling true/false ports
        forM_ ["true", "false"] $ \lbl -> do
            case L.lookup lbl (PG.pgGroupAdjFull adj_out) of
                Just lbl_outs -> bldCallPort (lbl, lbl_outs) [("bb_prefix", "bb_" ++ lbl)]
                Nothing       -> do
                    --error $ "Cannot find port " ++ lbl ++ " in operator node with id: " ++ (show nid) ++ " (" ++ (show $ nLabel node) ++ ")"
                    bldAddBB $ "bb_"++ lbl ++ "0"
                    bldPrintf (" -> NO " ++ lbl ++ " PORT DEFINED!") []
                    bldTerminate $ termBr "bb_err"

        forM_ [("bb_cont", pgContOp), ("bb_done", pgDoneOp), ("bb_err", pgErrOp)] $ \(bb,retval) -> do
            bldAddBB bb
            bldAddPhi "ret_phi" (retval, bb)
            bldTerminate $ termBr "bb_ret"

        bldAddBB "bb_ret"
        ret <- bldPhi "ret_phi"
        bldTerminate $ termRetOp ret


-- Main function for graph that just runs it in an infinite loop, starting with an empty input
llvm_pg_main_func :: PG.Node -> String -> LLVM ()
llvm_pg_main_func entry_node stackname = do
    state_ty <- cgStateTy
    input_ty <- cgInputTy
    pli <- cgPLI
    on_cnt <- cgONCounters
    fnAdd "pg_main" voidTy [] $ do
        snOp <- bldConstStr' stackname
        plnameOp <- bldConstStr' $ PL.plLabel $ PLI.pliPipeline pli

        -- Initialize pipeline
        plh <- bldAddInstruction $ mkFnCall "pl_init" [snOp, plnameOp]
        bldAddInstruction $ mkSt (getGlobalOp glblPipeline) plh -- global pipline handle
        state <- bldAddInstruction $ mkFnCall "pl_get_state" [plh]

        -- Create input queues
        forM_ (PLI.pliInQs pli) $ \(_,PLI.PIQueue qn) -> do
            qnOp <- bldConstStr' qn
            bldAddInstruction $ mkFnCall "pl_inqueue_create" [plh, qnOp]
        -- Bind output queues
        forM_ (PLI.pliOutQs pli) $ \(_,PLI.POQueue qn) -> do
            qnOp <- bldConstStr' qn
            qh <- bldAddInstruction $ mkFnCall "pl_outqueue_bind" [plh, qnOp]
            bldAddInstruction $ mkSt (getGlobalOp $ glblOutqueue qn) qh -- global pipline handle
        -- Wait for all pipelines to be ready
        bldAddInstruction $ mkFnCall "pl_wait_ready" [plh]

        input <- bldAddInstruction $ mkFnCall "input_alloc" [] -- allocate (and initialize) input

        bldTerminate $ termBr "body"                            -- jump to loop body

        bldAddBB "body" -- main loop (for now, loop forever)
        entry <- gets entryBB
        -- setup a loop counter
        cnt <- bldAddInstruction $ mkPhi i32Ty [(operandInt32_0, entry), (getLocalRef "cnt_next", "body")]
        bldDbgPrintf "count: %d\n" [cnt]
        cnt_next <- bldAddNamedInstruction "cnt_next" $ mkAdd cnt operandInt32_1

        bldAddInstruction $ mkCall (getFnOp $ pg_fname entry_node) [state, input] -- call entry
        bldAddInstruction $ mkFnCall "input_clean_attrs" [input] -- clean up input
        bldAddInstruction $ mkFnCall "input_clean_packet" [input] -- clean up input
        bldAddInstruction $ mkFnCall "pl_process_events" [plh] -- process events

        -- reset o-node counters
        forM_ on_cnt $ \cnt_name ->
            bldAddInstruction $ mkSt (getGlobalOp cnt_name) operandInt32_0

        -- start over again :-)
        bldTerminate $ termBr "body"




-- build an llvm module for the given protocol graph
llvm_pg :: PGraph -> Label -> LLVM ()
llvm_pg pgraph entry = do
    state_ty <- cgStateTy
    addTy "pg_state" state_ty
    -- add declarations for printf/puts
    addExternalFn i32Ty "puts" [((mkPtrTy i8Ty), "str")]
    addExternalFnVargs i32Ty (AST.Name "printf") [((mkPtrTy i8Ty), AST.Name "str")]
    {--
    addExternalFn voidTy ("llvm.memset.p0i8.i64")
                         [((mkPtrTy i8Ty), "str"),
                          (i8Ty, "val"),
                          (i32Ty, "len"),
                          (i32Ty, "align"),
                          (i1Ty,  "isvolatile")]
    --}
    --
    mapM_ llvm_pg_fn $ PG.pgFullNodes pgraph entry

-- build dummy implementation functions based on the given map
llvm_pg_dummy :: PGraph -> Label -> [(String,Integer)] -> LLVM ()
llvm_pg_dummy pgraph entry def_list = do
    mapM_ (llvm_pg_dummy_impl def_map) $ PG.pgFullNodes pgraph entry
    where def_map :: M.Map String Integer
          def_map = M.fromList def_list

{-
 -
 -}

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

type LlvmCtx   = LLVM.Ctx.Context
type LlvmMod   = LLVM.Mod.Module
type LlvmEeMod = LLVM.EE.ExecutableModule
type LlvmMcJIT = LLVM.EE.MCJIT

-- exection engine
withJitEE :: LlvmCtx -> (LlvmMcJIT -> IO a) -> IO a
withJitEE ctx = LLVM.EE.withMCJIT ctx optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

-- executable module
withExMod :: LlvmCtx -> LlvmMod -> (LlvmEeMod LlvmMcJIT -> IO a) -> IO a
withExMod ctx mod ioact = withJitEE ctx (\ee -> LLVM.EE.withModuleInEngine ee mod ioact)

foreign import ccall "dynamic" haskFn :: FunPtr (IO ()) -> (IO ())


modPrint :: LlvmMod  -> IO ()
modPrint mod = LLVM.Mod.moduleLLVMAssembly mod >>= putStrLn

modWriteFile :: LlvmMod -> String -> IO ()
modWriteFile mod fname = do
    --this segfaults
    --liftError $ LLVM.Mod.writeLLVMAssemblyToFile (LLVM.Mod.File fname) mod
    LLVM.Mod.moduleLLVMAssembly mod >>= (writeFile fname)

modExec :: LlvmCtx -> LlvmMod -> String -> IO ()
modExec ctx mod fn_name = withExMod ctx mod $ \emod -> do
    fn <- LLVM.EE.getFunction emod (AST.Name fn_name)
    case fn of
        Just f   -> run f
        Nothing  -> error ("Error: cannot get pointer to " ++ fn_name)
    where
        run :: FunPtr a -> IO ()
        run fn = haskFn (castFunPtr fn :: FunPtr (IO ()))

-- find a type by name in a module
findTy :: AST.Module -> String -> Maybe AST.Type
findTy ast ty_str = case L.find fn $ AST.moduleDefinitions ast of
                    Just (AST.TypeDefinition (AST.Name _) (Just ty)) -> Just ty
                    _ -> Nothing
    where fn :: AST.Definition -> Bool
          fn def = case def of
                    (AST.TypeDefinition (AST.Name s) _) -> s == ty_str
                    _ -> False

getTypes :: AST.Module -> [AST.Definition]
getTypes ast = filter fn $ AST.moduleDefinitions ast
    where fn :: AST.Definition -> Bool
          fn (AST.TypeDefinition _ _) = True
          fn _ = False

getTypeNames :: AST.Module -> [String]
getTypeNames ast = map (\x@(AST.TypeDefinition (AST.Name s) _) -> s) $ getTypes ast

-- Name of the global pipeline handle variable
glblPipeline :: String
glblPipeline = "pipeline_handle"

-- Name of the global out queue handle variables
glblOutqueue :: PL.PLabel -> String
glblOutqueue qn = "outqueue_" ++ qn -- TODO: do we need to sanitize this?


-- TODO:
--  - input from queues
--  - some sort of initialization infrastructure:
--    . initialize tap
--    . initialize each queue
--  - access state

codegen_all pgraph stackname = do
    let (entry_id, entry_node) = pgEntry pgraph
    let entry_label = PG.nLabel entry_node

    state_ty <- cgStateTy
    input_ty <- cgInputTy

    addDefn $ AST.TypeDefinition (AST.Name "struct.input") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.state") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.driver") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.tap_handler") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.pipeline_handle") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.queue_handle") Nothing
    -- hack:
    addDefn $ AST.TypeDefinition (AST.Name "struct.arp_pending") Nothing
    addDefn $ AST.TypeDefinition (AST.Name "struct.arp_cache") Nothing

    addExternalFn voidTy "pg_state_init" [((mkPtrTy state_ty), "state")]
    addExternalFn (mkPtrTy input_ty) "input_alloc" []
    addExternalFn voidTy "input_copy_packet" [((mkPtrTy input_ty), "input"), ((mkPtrTy i8Ty), "buff"), (i64Ty, "len")]
    addExternalFn voidTy "input_free" [((mkPtrTy input_ty), "input")]
    addExternalFn voidTy "input_clean_attrs" [((mkPtrTy input_ty), "input")]
    addExternalFn voidTy "input_clean_packet" [((mkPtrTy input_ty), "input")]
    addExternalFn muxIdTy "input_muxid" [((mkPtrTy input_ty), "input")]
    addExternalFn voidTy "input_set_muxid" [((mkPtrTy input_ty), "input"), (muxIdTy, "id")]
    addExternalFn voidTy "input_xchg" [((mkPtrTy input_ty), "a"), ((mkPtrTy input_ty), "b")]

    let plp_ty = mkPtrTy pipelineTy
    let qp_ty = mkPtrTy queueTy
    -- Pipeline init functions
    addExternalFn plp_ty "pl_init" [((mkPtrTy i8Ty), "stackname"), ((mkPtrTy i8Ty), "plname")]
    addExternalFn (mkPtrTy state_ty) "pl_get_state" [(plp_ty, "plh")]
    addExternalFn qp_ty "pl_inqueue_create" [(plp_ty, "plh"), ((mkPtrTy i8Ty), "name")]
    addExternalFn qp_ty "pl_outqueue_bind" [(plp_ty, "plh"), ((mkPtrTy i8Ty), "name")]
    addExternalFn voidTy "pl_wait_ready" [(plp_ty, "plh")]

    -- Queue interaction
    addExternalFn voidTy "pl_enqueue" [(qp_ty, "queue"), ((mkPtrTy input_ty), "input")]
    addExternalFn (mkPtrTy input_ty) "pl_poll" [(plp_ty, "plh")]
    addExternalFn voidTy "pl_process_events" [(plp_ty, "plh")]
    addStaticVar plp_ty glblPipeline (AST.C.Null plp_ty)
    --addGlobalVar (mkPtrTy pipelineTy) glblPipeline
    pli <- cgPLI
    forM_ (PLI.pliOutQs pli) $ \(_,(PLI.POQueue queue)) -> do
        addStaticVar qp_ty (glblOutqueue queue) (AST.C.Null qp_ty)

    llvm_pg pgraph entry_label   -- Build node functions
    llvm_pg_main_func entry_node stackname -- Build main runner function


passes :: LLVM.PM.PassSetSpec
passes = LLVM.PM.defaultCuratedPassSetSpec { LLVM.PM.optLevel = Just 3 }

-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = DGI.nmap fixN pg
    where
        fixN n
            | l == "Queue" = n { nLabel = "TapRxQueue" }
            | l == "TxQueue" = n { nLabel = "TapTxQueue" }
            | otherwise = n
            where l = nLabel n


-- Prepeares and runs the pipeline
runPipeline :: PL.PLGraph -> String -> String -> PLI.PipelineImpl -> IO ()
runPipeline plg stackname helpers pli = fmap (const ()) $ forkOS $ do
    writeFile ("pipeline-" ++ mname ++ ".dot") $ toDot pgraph
    LLVM.Ctx.withContext $ \ctx ->
        liftError $ LLVM.Mod.withModuleFromBitcode ctx llvm_helpers $ \mod2 -> do
            ast2 <- LLVM.Mod.moduleAST mod2
            -- load input type and state type from module
            let input_ty = case findTy ast2 "struct.input" of
                                Just ty -> ty
                                Nothing -> error "Could not find struct input"
            let state_ty = case findTy ast2 "struct.state" of
                                Just ty -> ty
                                Nothing -> error "Could not find struct state"
            let ast_mod = cgAstModule $ codeGen mname state_ty input_ty pli plg (codegen_all pgraph stackname)
            liftError $ LLVM.Mod.withModuleFromAST ctx ast_mod $ \mod -> do
                modWriteFile mod $ mname ++ "-cg.ll"
                liftError $ LLVM.Mod.linkModules False mod mod2
                --modPrint mod
                modWriteFile mod $ mname ++ "-linked-cg.ll"
                putStrLn $ "Verifying " ++ mname
                err <- runErrorT $ LLVM.A.verify mod
                case err of Right () -> putStrLn $ "module verified"
                            Left e   -> putStrLn $ "error verifying module:" ++ e
                --modExec ctx mod "pg_main"
                LLVM.PM.withPassManager passes $ \pm -> do
                    LLVM.PM.runPassManager pm mod
                    modWriteFile mod $ mname ++ "-optimized-cg.ll"
                    modExec ctx mod "pg_main"
                    return ()
    where
        pl = PLI.pliPipeline pli
        pgraph = PL.plGraph pl
        mname = PL.plLabel pl
        -- LLVM file with helper utilities
        llvm_helpers = LLVM.Mod.File $ "dist/build/" ++ helpers ++ ".bc"

plAssign :: PG.PGNode -> PL.PLabel
plAssign (_,n)
    | take 2 lbl == "Tx" || take 5 lbl == "TapTx" = "Tx"
    | otherwise = "Rx"
    where
        lbl = nLabel n

plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

main :: IO ()
main = do
    let fname_def = "unicorn-tests/hello.unicorn"       -- default unicorn file name

    xargs <- getArgs
    let fname = if (length xargs) == 0 then fname_def else xargs !! 0

    pname <- getProgName
    let helpers = case pname of
            "llvm-cgen" -> "llvm-helpers"
            "llvm-cgen-dpdk" -> "llvm-helpers-dpdk"
            _ -> error "Unknown executable name, don't know what helpers to use :-/"

    txt <- readFile fname
    graph <- UnicornAST.parseGraph txt
    let pgraph = pg4tap $ Unicorn.constructGraph graph
    writeFile "DELETEME.dot" $ toDot pgraph
    let plg = PL.generatePLG plAssign pgraph
    writeFile "pipelines.dot" $ pipelinesDot Nothing plg

    let stackname = "dragonet"
    PLI.runPipelines stackname plConnect (runPipeline plg stackname helpers) plg
    forever yield

