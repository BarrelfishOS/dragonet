{-# LANGUAGE ForeignFunctionInterface #-}
module Runner.Dynamic (
    createPipeline
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PI
import qualified Dragonet.Pipelines.Dynamic as PD
import qualified Data.Graph.Inductive as DGI
import Util.GraphHelpers (findNodeByL)

import qualified LLVM.General.AST as AST
import qualified LLVM.General.Context as LLVMCtx
import qualified LLVM.General.Module as LLVMMod
import qualified LLVM.General.ExecutionEngine as LLVMEE
import qualified LLVM.General.Analysis as LLVMA
import qualified LLVM.General.PassManager as LLVMPM

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Control.Concurrent (forkOS,yield)
import qualified Control.Concurrent.STM as STM
import Debug.Trace (trace)

newtype GraphHandle = GraphHandle (Ptr GraphHandle)
newtype NodeHandle = NodeHandle (Ptr NodeHandle)
newtype EdgeHandle = EdgeHandle (Ptr EdgeHandle)
newtype SpawnHandle = SpawnHandle (Ptr SpawnHandle)
type NodeFun = FunPtr (PI.StateHandle -> PI.InputHandle -> IO Word32)
type MuxId = Word32

-- Build node
buildDynNode :: DynState -> PG.Node -> IO NodeHandle
buildDynNode ds l
    | isFromQ = mkFromQueueNode gh lbl fqh
    | isDemux = mkDemuxNode gh lbl
    | isMux = mkMuxNode gh lbl muxId
    | isToQ = mkToQueueNode gh lbl qh
    | PG.FNode {} <- l = do
        fp <- nf l
        mkFNode gh lbl fp
    | PG.ONode { PG.nOperator = op } <- l = mkONode gh lbl op
    | otherwise = error "Unsupported node type"
    where
        gh = dsGraph ds
        nf = plhNodeFun $ dsHelpers ds
        lbl = PG.nLabel l
        isDemux
            | PG.FNode { PG.nLabel = "Demux" } <- l = True
            | otherwise = False

        mbMuxId = PGU.getPGNAttr l "muxid"
        isMux = isJust mbMuxId
        Just muxId = read <$> mbMuxId

        mbPL = PGU.getPGNAttr l "pipeline"
        isToQ = isJust mbPL
        Just pln = mbPL
        Just qh = pln `M.lookup` dsOutQs ds

        mbFPL = PGU.getPGNAttr l "frompipeline"
        isFromQ = isJust mbFPL
        Just fpln = mbFPL
        Just fqh = fpln `M.lookup` dsInQs ds



buildGraph :: DynState -> PG.PGraph -> IO DynState
buildGraph ds pg = do
    let gh = dsGraph ds
    -- Create nodes
    nlm <- forM (DGI.labNodes pg) $ \(n,l) -> do
        h <- buildDynNode ds l
        addPorts h $ length $ PG.nPorts l
        return (n,h)
    let nm = M.fromList nlm
    -- Add edges
    let aEdge (a,b,PG.Edge { PG.ePort = p }) = const () <$> addEdge src pi dst
            where
                Just al = DGI.lab pg a
                Just pi = lookup p $ flip zip [0..] $ fixBoolP $ PG.nPorts al
                Just src = M.lookup a nm
                Just dst = M.lookup b nm
        aEdge _ = return ()
    mapM aEdge $ DGI.labEdges pg
    -- Add spawns to nodes
    let st = (ds, S.fromList $ map (\(a,b,_) -> (a,b)) $ dsSpawns ds)
        dsSpawn d n = L.find (\(l,t,_) -> l == PG.nLabel n &&
                                          t == PG.nTag n) $ dsSpawns d
        procSpawn n (ds',s) (_,m) = do
            let Just nh = M.lookup n nm
                Just mh = M.lookup m nm
                Just mm = DGI.lab pg m
                (mL,mT) = (PG.nLabel mm, PG.nTag mm)
                ms = dsSpawn ds' mm
            (ret,sh) <- case ms of
                Just (_,_,sh) -> if (mL,mT) `S.member` s
                    then do
                        updateSpawn sh mh
                        return ((ds', (mL,mT) `S.delete` s), sh)
                    else return ((ds', s), sh)
                Nothing -> do
                    sh <- mkSpawn gh mh
                    let ds'' = ds' { dsSpawns = dsSpawns ds' ++ [(mL,mT,sh)] }
                    return ((ds'',s),sh)
            addSpawn nh sh
            return ret
    (ds',unused) <- foldM (\ret (n,_) ->
        foldM (procSpawn n) ret $ PGU.orderedSpawns pg n) st $ DGI.labNodes pg
    -- Remove old unused spawns
    let (old,remaining) =
            L.partition (\(a,b,_) -> (a,b) `S.member` unused) $ dsSpawns ds'
        ds'' = ds' { dsSpawns = remaining }
    forM old $ \(_,_,sh) -> rmSpawn gh sh
    -- Spawn init nodes
    let inits = [sh | n' <- S.toList $ PGU.entryNodes pg,
                       let Just l = DGI.lab pg n',
                       PG.nAttrElem (PG.NAttrCustom "init") l,
                       let Just (_,_,sh) = dsSpawn ds'' l]
    mapM_ (addInit gh) inits


    return ds''
    where
        fixBoolP l
            | "true" `elem` l && "false" `elem` l && length l == 2 =
                ["false", "true"]
            | otherwise = l



withHelpers :: String -> String -> ((String -> IO (FunPtr b)) -> IO a) -> IO a
withHelpers llvm_helpers dyn_helpers run = do
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            liftError $ LLVMMod.withModuleFromBitcode ctx file' $ \dyn_mod -> do
                liftError $ LLVMMod.linkModules False mod dyn_mod
                LLVMPM.withPassManager passes $ \pm -> do
                    LLVMPM.runPassManager pm mod
                    withJitEE ctx $ \ee -> do
                        LLVMEE.withModuleInEngine ee mod $ \emod -> do
                            run (pgf emod)

    where
        file = LLVMMod.File llvm_helpers
        file' = LLVMMod.File dyn_helpers
        pgf emod n = do
            mf <- LLVMEE.getFunction emod (AST.Name n)
            let f = case mf of
                    Just f -> f
                    Nothing -> error $ "getFunction " ++ n ++ " failed"
            return $ castFunPtr f


foreign import ccall "dynamic"
    splhFun :: FunPtr (PI.PipelineHandle -> IO ()) -> PI.PipelineHandle -> IO ()

-- Don't ask... ;-)
foreign import ccall "dynamic"
    c_pl_init :: FunPtr (CString -> CString -> IO PI.PipelineHandle)
        -> CString -> CString -> IO PI.PipelineHandle
foreign import ccall "dynamic"
    c_pl_queue_prepare ::
        FunPtr (PI.PipelineHandle -> CString -> IO PI.QueueHandle)
            -> PI.PipelineHandle -> CString -> IO PI.QueueHandle
foreign import ccall "dynamic"
    c_pl_wait_ready :: FunPtr (PI.PipelineHandle -> IO ())
        -> PI.PipelineHandle -> IO ()

pl_init_wrapper :: FunPtr (CString -> CString -> IO PI.PipelineHandle)
        -> String -> String -> IO PI.PipelineHandle
pl_init_wrapper fp sN pN = withCString sN $ \csN -> withCString pN $ \cpN ->
    c_pl_init fp csN cpN
pl_qprep_wrapper :: FunPtr (PI.PipelineHandle -> CString -> IO PI.QueueHandle)
        -> PI.PipelineHandle -> String -> IO PI.QueueHandle
pl_qprep_wrapper fp ph qN = withCString qN $ \cqN -> c_pl_queue_prepare fp ph cqN

type CmdQ = STM.TQueue DynCommand

data DynCommand =
    DCStart |
    DCInQAdd PL.PLabel PI.PInput |
    DCInQRemove PL.PLabel |
    DCOutQAdd PL.PLabel PI.POutput |
    DCOutQRemove PL.PLabel |
    DCSetGraph PG.PGraph |
    DCDestroy
    deriving Show

hStart :: CmdQ -> IO ()
hStart q = STM.atomically $ STM.writeTQueue q DCStart

hStop :: CmdQ -> IO () -> IO ()
hStop _ stop = stop

hInQAdd :: CmdQ -> PL.PLabel -> PI.PInput -> IO ()
hInQAdd q pl pi = STM.atomically $ STM.writeTQueue q $ DCInQAdd pl pi

hInQRemove :: CmdQ -> PL.PLabel -> IO ()
hInQRemove q pl = STM.atomically $ STM.writeTQueue q $ DCInQRemove pl

hOutQAdd :: CmdQ -> PL.PLabel -> PI.POutput -> IO ()
hOutQAdd q pl po = STM.atomically $ STM.writeTQueue q $ DCOutQAdd pl po

hOutQRemove :: CmdQ -> PL.PLabel -> IO ()
hOutQRemove q pl = STM.atomically $ STM.writeTQueue q $ DCOutQRemove pl

hSetGraph :: CmdQ -> PG.PGraph -> IO ()
hSetGraph q pg = STM.atomically $ STM.writeTQueue q $ DCSetGraph pg

hDestroy :: CmdQ -> IO ()
hDestroy q = STM.atomically $ STM.writeTQueue q DCDestroy


mkDPL :: PL.PLabel -> CmdQ -> IO () -> PD.DynPipeline
mkDPL l q stop =
    PD.DynPipeline {
        PD.dplLabel = l,
        PD.dplStart = hStart q,
        PD.dplStop = hStop q stop,
        PD.dplInQAdd = hInQAdd q,
        PD.dplInQRemove = hInQRemove q,
        PD.dplOutQAdd = hOutQAdd q,
        PD.dplOutQRemove = hOutQRemove q,
        PD.dplSetGraph = hSetGraph q,
        PD.dplDestroy = hDestroy q
    }

data PLHelpers = PLHelpers {
    plhInQCreate :: PI.PipelineHandle -> String -> IO PI.QueueHandle,
    plhOutQBind  :: PI.PipelineHandle -> String -> IO PI.QueueHandle,
    plhWaitReady :: PI.PipelineHandle -> IO (),
    plhNodeFun :: PG.Node -> IO NodeFun
}

data DynState = DynState {
    dsGraph   :: GraphHandle,
    dsPLH     :: PI.PipelineHandle,
    dsSpawns  :: [(String,String,SpawnHandle)],
    dsInQs    :: M.Map String PI.QueueHandle,
    dsOutQs   :: M.Map String PI.QueueHandle,
    dsHelpers :: PLHelpers
}

eventHandler :: DynState -> DynCommand -> IO (Maybe DynState)
eventHandler ds DCStart = do
    plhWaitReady (dsHelpers ds) $ dsPLH ds
    runGraph $ dsGraph ds
    return $ Just ds

eventHandler ds (DCInQAdd l pi) = do
    let hlp = dsHelpers ds
        plh = dsPLH ds
        PI.PIQueue n = pi
    qh <- plhInQCreate hlp plh n
    return $ Just $ ds { dsInQs = M.insert l qh $ dsInQs ds }

eventHandler ds (DCOutQAdd l po) = do
    let hlp = dsHelpers ds
        plh = dsPLH ds
        PI.POQueue n = po
    qh <- plhOutQBind hlp plh n
    return $ Just $ ds { dsOutQs = M.insert l qh $ dsOutQs ds }

eventHandler ds (DCInQRemove l) = do
    putStrLn "Warning: NDI DCInQRemove"
    return $ Just ds

eventHandler ds (DCOutQRemove l) = do
    putStrLn "Warning: NDI DCInQRemove"
    return $ Just ds

eventHandler ds (DCSetGraph pg) = do
    clearGraph $ dsGraph ds
    fmap Just $ buildGraph ds pg



createPipeline :: PL.PLGraph -> String -> String -> PL.PLabel
                    -> IO PD.DynPipeline
createPipeline plg stackname helpers mname = do
    cmdq <- STM.newTQueueIO
    mv <- STM.newEmptyTMVarIO
    -- Spawn off execution thread
    forkOS $ withHelpers llvm_helpers dyn_helpers $ \hf -> do
        -- Prepare helper functions
        (pl_init,pl_helpers) <- pl_get_funs hf
        -- Initialize pipeline
        plh <- pl_init stackname mname
        splh <- hf "set_pipeline_handle"
        splhFun splh plh
        -- Initialize empty dynamic graph
        gh <- mkGraph plh
        -- Return DynPipeline to main thread
        STM.atomically $ STM.putTMVar mv $ mkDPL mname cmdq $ stopGraph gh
        -- Start processing events from the queue
        let eventLoop st = do
                ev <- STM.atomically $ STM.readTQueue cmdq
                --putStrLn $ mname ++ ": Got event: " ++ show ev
                res <- eventHandler st ev
                --putStrLn $ mname ++ ": Done event"
                case res of
                    Nothing -> return ()
                    Just st' -> eventLoop st'
        putStrLn $ mname ++ ": Starting event loop"
        eventLoop $
            DynState {
                dsGraph = gh,
                dsPLH = plh,
                dsSpawns = [],
                dsInQs = M.empty,
                dsOutQs = M.empty,
                dsHelpers = pl_helpers }

    -- Wait for message from execution thread
    STM.atomically $ STM.takeTMVar mv
    where
        -- LLVM file with helper utilities
        llvm_helpers = "dist/build/" ++ helpers ++ ".bc"
        dyn_helpers = "dist/build/llvm-dyn-helpers.bc"
        pl_get_funs hf = do
            pl_init <- castFunPtr <$> hf "pl_init"
            pl_inq <- castFunPtr <$> hf "pl_inqueue_create"
            pl_outq <- castFunPtr <$> hf "pl_outqueue_bind"
            pl_wait <- castFunPtr <$> hf "pl_wait_ready"
            let ifName (PG.NImplFunction f) = "do_pg__" ++ f
                nodeFun l = castFunPtr <$> (hf $ ifName $ PG.nImplementation l)
            return (pl_init_wrapper pl_init,
                    PLHelpers {
                        plhInQCreate = pl_qprep_wrapper pl_inq,
                        plhOutQBind = pl_qprep_wrapper pl_outq,
                        plhWaitReady = c_pl_wait_ready pl_wait,
                        plhNodeFun = nodeFun })


-------------------------------------------------------------------------------
-- Convenient functions :)

mkGraph :: PI.PipelineHandle -> IO GraphHandle
mkGraph = c_mkgraph

addInit :: GraphHandle -> SpawnHandle -> IO ()
addInit = c_add_init

runGraph :: GraphHandle -> IO ()
runGraph = c_run_graph

stopGraph :: GraphHandle -> IO ()
stopGraph = c_stop_graph

clearGraph :: GraphHandle -> IO ()
clearGraph = c_clear_graph

mkFNode :: GraphHandle -> String -> NodeFun -> IO NodeHandle
mkFNode gh l f = withCString l $ \cl -> c_mkfnode gh cl f

mkONode :: GraphHandle -> String -> PG.NOperator -> IO NodeHandle
mkONode gh l o = withCString l $ \cl -> case o of
    PG.NOpAnd -> c_mkonode_and gh cl
    PG.NOpOr -> c_mkonode_or gh cl
    PG.NOpNAnd -> c_mkonode_nand gh cl
    PG.NOpNOr -> c_mkonode_nor gh cl

mkDemuxNode :: GraphHandle -> String -> IO NodeHandle
mkDemuxNode gh l = withCString l $ \cl -> c_mknode_demux gh cl

mkMuxNode :: GraphHandle -> String -> MuxId -> IO NodeHandle
mkMuxNode gh l i = withCString l $ \cl -> c_mknode_mux gh cl i

mkToQueueNode :: GraphHandle -> String -> PI.QueueHandle -> IO NodeHandle
mkToQueueNode gh l qh = withCString l $ \cl -> c_mknode_toqueue gh cl qh

mkFromQueueNode :: GraphHandle -> String -> PI.QueueHandle -> IO NodeHandle
mkFromQueueNode gh l qh = withCString l $ \cl -> c_mknode_fromqueue gh cl qh

mkSpawn :: GraphHandle -> NodeHandle -> IO SpawnHandle
mkSpawn = c_mkspawn

updateSpawn :: SpawnHandle -> NodeHandle -> IO ()
updateSpawn = c_updatespawn

rmSpawn :: GraphHandle -> SpawnHandle -> IO ()
rmSpawn = c_rmspawn

addPorts :: NodeHandle -> Int -> IO Int
addPorts nh n = fromIntegral <$> c_addports nh (fromIntegral n)

addEdge :: NodeHandle -> Int -> NodeHandle -> IO EdgeHandle
addEdge so p si = c_addedge so (fromIntegral p) si

addSpawn :: NodeHandle -> SpawnHandle -> IO ()
addSpawn = c_addspawn

-------------------------------------------------------------------------------
-- C Declarations

foreign import ccall "dyn_mkgraph"
    c_mkgraph :: PI.PipelineHandle -> IO GraphHandle
foreign import ccall "dyn_add_init"
    c_add_init :: GraphHandle -> SpawnHandle -> IO ()
foreign import ccall "dyn_rungraph"
    c_run_graph :: GraphHandle -> IO ()
foreign import ccall "dyn_stopgraph"
    c_stop_graph :: GraphHandle -> IO ()
foreign import ccall "dyn_cleargraph"
    c_clear_graph :: GraphHandle -> IO ()
foreign import ccall "dyn_mkfnode"
    c_mkfnode :: GraphHandle -> CString -> NodeFun -> IO NodeHandle
foreign import ccall "dyn_mkonode_and"
    c_mkonode_and :: GraphHandle -> CString -> IO NodeHandle
foreign import ccall "dyn_mkonode_or"
    c_mkonode_or :: GraphHandle -> CString -> IO NodeHandle
foreign import ccall "dyn_mkonode_nand"
    c_mkonode_nand :: GraphHandle -> CString -> IO NodeHandle
foreign import ccall "dyn_mkonode_nor"
    c_mkonode_nor :: GraphHandle -> CString -> IO NodeHandle
foreign import ccall "dyn_mknode_demux"
    c_mknode_demux :: GraphHandle -> CString -> IO NodeHandle
foreign import ccall "dyn_mknode_mux"
    c_mknode_mux :: GraphHandle -> CString -> MuxId -> IO NodeHandle
foreign import ccall "dyn_mknode_toqueue"
    c_mknode_toqueue ::
        GraphHandle -> CString -> PI.QueueHandle -> IO NodeHandle
foreign import ccall "dyn_mknode_fromqueue"
    c_mknode_fromqueue ::
        GraphHandle -> CString -> PI.QueueHandle -> IO NodeHandle
foreign import ccall "dyn_mkspawn"
    c_mkspawn :: GraphHandle -> NodeHandle -> IO SpawnHandle
foreign import ccall "dyn_updatespawn"
    c_updatespawn :: SpawnHandle -> NodeHandle -> IO ()
foreign import ccall "dyn_rmspawn"
    c_rmspawn :: GraphHandle -> SpawnHandle -> IO ()
foreign import ccall "dyn_addports"
    c_addports :: NodeHandle -> CSize -> IO CSize
foreign import ccall "dyn_addedge"
    c_addedge :: NodeHandle -> CSize -> NodeHandle -> IO EdgeHandle
foreign import ccall "dyn_addspawn"
    c_addspawn :: NodeHandle -> SpawnHandle -> IO ()

-------------------------------------------------------------------------------
-- Generic Helpers

-- Allocates array and passes it to f, the array is only available while in f
listAsArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
listAsArray l f = allocaBytesAligned sz al $ \a -> do
    forM (zip [0..] l) $ uncurry $ pokeElemOff a
    f a
    where
        n = length l
        al = alignment $ head l
        sz = n * (sizeOf $ head l)


type LlvmCtx   = LLVMCtx.Context
type LlvmMod   = LLVMMod.Module
type LlvmEeMod = LLVMEE.ExecutableModule
type LlvmMcJIT = LLVMEE.MCJIT

-- exection engine
withJitEE :: LlvmCtx -> (LlvmMcJIT -> IO a) -> IO a
withJitEE ctx = LLVMEE.withMCJIT ctx optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

-- executable module
withExMod :: LlvmCtx -> LlvmMod -> (LlvmEeMod LlvmMcJIT -> IO a) -> IO a
withExMod ctx mod ioact =
    withJitEE ctx (\ee -> LLVMEE.withModuleInEngine ee mod ioact)

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

passes :: LLVMPM.PassSetSpec
passes = LLVMPM.defaultCuratedPassSetSpec { LLVMPM.optLevel = Just 3 }

