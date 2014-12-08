{-# LANGUAGE ForeignFunctionInterface #-}
module Runner.Dynamic (
    GraphHandle,
    createPipelineClient,
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
import qualified Control.Monad.Trans.State.Strict as ST


newtype NodeHandle = NodeHandle Word64
newtype EdgeHandle = EdgeHandle Word64
newtype SpawnHandle = SpawnHandle Word64
newtype QueueHandle = QueueHandle Word64
type GraphHandle = PI.GraphHandle
type NodeFun = FunPtr (PI.StateHandle -> PI.InputHandle -> IO Word32)
type MuxId = Word32


type DynM a = ST.StateT DynState IO a

-- Build node
buildDynNode :: PG.Node -> DynM NodeHandle
buildDynNode n = do
    ds <- dmDS
    buildDynNode' ds n

buildDynNode' :: DynState -> PG.Node -> DynM NodeHandle
buildDynNode' ds l
    | isFromQ = mkFromQueueNode lbl fqh
    | isDemux = mkDemuxNode lbl
    | isMux = mkMuxNode lbl muxId
    | isToQ = mkToQueueNode lbl qh
    | isFS = mkFromSocketNode lbl fsId
    | isTS = mkToSocketNode lbl tsId
    | isUDPDemux = mkUDPDemuxNode lbl udpSIP udpSP udpDIP udpDP
    | isBalance = mkBalanceNode lbl
    | PG.FNode {} <- l = mkFNode lbl ifun isProductive
    | PG.ONode { PG.nOperator = op } <- l = mkONode lbl op
    | otherwise = error "Unsupported node type"
    where
        lbl = PG.nLabel l
        PG.NImplFunction ifun = PG.nImplementation l
        isProductive = not $ PG.nAttrElem (PG.NAttrCustom "unproductive") l
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

        mbFS = PGU.fromSocketId l
        isFS = isJust mbFS
        Just fsId = read <$> mbFS

        mbTS = PGU.toSocketId l
        isTS = isJust mbTS
        Just tsId = read <$> mbTS

        isUDPDemux = PG.nAttrElem (PG.NAttrCustom "udpdemux") l
        isBalance = PGU.isBalanceNode l
        udpSIP = maybe 0 read $ PGU.getPGNAttr l "srcip"
        udpDIP = maybe 0 read $ PGU.getPGNAttr l "dstip"
        udpSP = maybe 0 read $ PGU.getPGNAttr l "srcport"
        udpDP = maybe 0 read $ PGU.getPGNAttr l "dstport"





buildGraph :: PG.PGraph -> DynM ()
buildGraph pg = do
    -- Create nodes
    nlm <- forM (DGI.labNodes pg) $ \(n,l) -> do
        h <- buildDynNode l
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
    -- Add spawns to nodes, and track unnused spawns
    spawns <- dmDSGets dsSpawns
    let st = S.fromList $ map (\(a,b,_) -> (a,b)) spawns
        dsSpawn n ds = L.find (\(l,t,_) -> l == PG.nLabel n &&
                                           t == PG.nTag n) $ dsSpawns ds
        procSpawn n s (_,m) = do
            let Just nh = M.lookup n nm
                Just mh = M.lookup m nm
                Just mm = DGI.lab pg m
                (mL,mT) = (PG.nLabel mm, PG.nTag mm)
            ms <- dmDSGets (dsSpawn mm)
            (ret,sh) <- case ms of
                -- Spawn already exists
                Just (_,_,sh) -> if (mL,mT) `S.member` s
                    then do
                        updateSpawn sh mh
                        return ((mL,mT) `S.delete` s, sh)
                    else return (s, sh)
                -- Create new spawn
                Nothing -> do
                    sh <- mkSpawn mh
                    dmDSModify $ \ds ->
                        ds { dsSpawns = dsSpawns ds ++ [(mL,mT,sh)] }
                    return (s,sh)
            -- Add spawn to node
            addSpawn nh sh
            return ret
    unused <- foldM (\ret (n,_) ->
        foldM (procSpawn n) ret $ PGU.orderedSpawns pg n) st $ DGI.labNodes pg
    -- Remove old unused spawns
    spawns' <- dmDSGets dsSpawns
    let (old,remaining) =
            L.partition (\(a,b,_) -> (a,b) `S.member` unused) spawns'
    dmDSModify $ \ds -> ds { dsSpawns = remaining }
    forM old $ \(_,_,sh) -> rmSpawn sh
    -- Spawn init nodes
    spawns'' <- dmDSGets dsSpawns
    let initNodes = [l | n' <- S.toList $ PGU.entryNodes pg,
                       let Just l = DGI.lab pg n',
                       PG.nAttrElem (PG.NAttrCustom "init") l]
    forM_ initNodes $ \n -> do
        Just (_,_,sh) <- dmDSGets (dsSpawn n)
        addInit sh
    where
        fixBoolP l
            | "true" `elem` l && "false" `elem` l && length l == 2 =
                ["false", "true"]
            | otherwise = l




type TDynState = STM.TVar DynState

runDynMWithTDS :: TDynState -> DynM a -> IO a
runDynMWithTDS tds act = do
    ds <- STM.atomically $ STM.readTVar tds
    (a,ds') <- ST.runStateT act ds
    STM.atomically $ STM.writeTVar tds ds'
    return a

hStart :: TDynState -> IO ()
hStart tds = runDynMWithTDS tds $ do
    dmDebugPrint "hStart"
    startGraph

hStop :: TDynState -> IO ()
hStop tds = runDynMWithTDS tds $ do
    dmDebugPrint "hStop"
    stopGraph

hInQAdd :: TDynState -> PL.PLabel -> PI.PInput -> IO ()
hInQAdd tds pl (PI.PIQueue iq) = do
    runDynMWithTDS tds $ do
        dmDebugPrint "hInQAdd"
        addInQueue pl iq
    return ()

hInQRemove :: TDynState -> PL.PLabel -> IO ()
hInQRemove tds pl = runDynMWithTDS tds $ do
    dmDebugPrint "hInQRemove"
    ans <- dmDSGets $ M.lookup pl . dsOutQs
    case (ans) of
        Just qh -> rmInQueue pl qh
            -- XXX: not sure if we need to do anything here
        Nothing ->  return ()

hOutQAdd :: TDynState -> PL.PLabel -> PI.POutput -> IO ()
hOutQAdd tds pl (PI.POQueue oq) = do
    runDynMWithTDS tds $ do
        dmDebugPrint "hOutQAdd"
        addOutQueue pl oq
    return ()

hOutQRemove :: TDynState -> PL.PLabel -> IO ()
hOutQRemove tds pl = runDynMWithTDS tds $ do
    dmDebugPrint "hOutQRemove"
    ans <- dmDSGets $ M.lookup pl . dsInQs
    case (ans) of
        Just qh -> rmOutQueue pl qh
            -- XXX: not sure if we need to do anything here
        Nothing ->  return ()


{-
hOutQRemove :: TDynState -> PL.PLabel -> IO ()
hOutQRemove tds pl = runDynMWithTDS tds $ do
    dmDebugPrint "hOutQRemove"
    ans <- dmDSGets $ M.lookup pl . dsInQs
    case ans of
        Just qh -> dmDSGets $ M.lookup pl . dsInQs
        Nothing -> error "Remove not supported " ++ (show ans)
    let Just aa = ans
    rmOutQueue pl (Just ans)
-}

hSetGraph :: TDynState -> PG.PGraph -> IO ()
hSetGraph tds pg = runDynMWithTDS tds $ do
    dmDebugPrint "hSetGraph"
    clearGraph
    buildGraph pg

hDestroy :: TDynState -> IO ()
hDestroy q = do
    putStrLn "TODO: hDestroy"


mkDPL :: PL.PLabel -> TDynState -> PD.DynPipeline
mkDPL l tds =
    PD.DynPipeline {
        PD.dplLabel = l,
        PD.dplStart = hStart tds,
        PD.dplStop = hStop tds,
        PD.dplInQAdd = hInQAdd tds,
        PD.dplInQRemove = hInQRemove tds,
        PD.dplOutQAdd = hOutQAdd tds,
        PD.dplOutQRemove = hOutQRemove tds,
        PD.dplSetGraph = hSetGraph tds,
        PD.dplDestroy = hDestroy tds
    }


data DynState = DynState {
    dsLabel   :: String,
    dsGraph   :: GraphHandle,
    dsSpawns  :: [(String,String,SpawnHandle)],
    dsInQs    :: M.Map String QueueHandle,
    dsOutQs   :: M.Map String QueueHandle,
    -- Allocator helpers for node, edge, spawn, and queue handles
    dsNextNH  :: Word64,
    dsNextEH  :: Word64,
    dsNextSH  :: Word64,
    dsNextQH  :: Word64
}


-- Only usee graph handle to control this pipeline, but don't start anything
createPipelineClient :: GraphHandle -> PL.PLabel -> IO PD.DynPipeline
createPipelineClient gh pl = do
    tds <- STM.atomically $ STM.newTVar $
        DynState {
            dsLabel = pl,
            dsGraph = gh,
            dsSpawns = [],
            dsInQs = M.empty,
            dsOutQs = M.empty,
            dsNextNH = 0,
            dsNextEH = 0,
            dsNextSH = 0,
            dsNextQH = 0 }
    return $ mkDPL pl tds

{-|
 -      * Creates an OS thread for an execution thread
 -      * Waits for graph handle to instantiate on STM
 -      * Creates a client-side pipeline using the handle received
 -}
-- Start pipeline execution engine in this thread
createPipeline :: PL.PLGraph -> String -> String -> PL.PLabel
                    -> IO PD.DynPipeline
createPipeline plg stackname helpers mname = do
    cmdq <- STM.newTQueueIO
    mv <- STM.newEmptyTMVarIO
    -- Spawn off execution thread
    forkOS $ withHelpers llvm_helpers dyn_helpers $ \hf -> do
        -- Prepare helper functions
        (dl_init, dl_client, dl_run) <- pl_get_funs hf
        -- Initialize
        dl <- dl_init
        gh <- dl_client dl

        -- Return DynPipeline to main thread
        STM.atomically $ STM.putTMVar mv gh
        putStrLn $ mname ++ ": Starting event loop"
        dl_run dl

    -- Wait for graph handle from execution thread
    gh <- STM.atomically $ STM.takeTMVar mv
    -- Create a pipeline using the graph handle read from the STM
    createPipelineClient gh mname
    where
        -- LLVM file with helper utilities
        llvm_helpers = "dist/build/" ++ helpers ++ ".bc"
        dyn_helpers = "dist/build/llvm-dyn-helpers.bc"
        pl_get_funs hf = do
            hInit <- castFunPtr <$> hf "dyn_local_init"
            hGraph <- castFunPtr <$> hf "dyn_local_client"
            hRun <- castFunPtr <$> hf "dyn_local_run"
            let nodeFun l = do
                    x <- castFunPtr <$> (hf $ "do_pg__" ++ l)
                    -- force evaluation
                    return $! x
            return (localInitWrap hInit stackname mname nodeFun,
                    localGraphFH hGraph,
                    localRunFH hRun)


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


-------------------------------------------------------------------------------
-- Local Pipeline specific stuff

type LocalResFNode = CString -> Ptr () -> IO NodeFun
newtype LocalHandle = LocalHandle (Ptr LocalHandle)

foreign import ccall "dynamic"
    localInitFH ::
        FunPtr (CString -> CString -> FunPtr LocalResFNode -> IO LocalHandle)
            -> CString -> CString -> FunPtr LocalResFNode -> IO LocalHandle
foreign import ccall "dynamic"
    localGraphFH ::
        FunPtr (LocalHandle -> IO GraphHandle) -> LocalHandle -> IO GraphHandle
foreign import ccall "dynamic"
    localRunFH ::
        FunPtr (LocalHandle -> IO ()) -> LocalHandle -> IO ()

foreign import ccall "wrapper"
    wrapResFN :: LocalResFNode -> IO (FunPtr LocalResFNode)

localInitWrap ::
        FunPtr (CString -> CString -> FunPtr LocalResFNode -> IO LocalHandle)
            -> String -> String -> (String -> IO NodeFun) -> IO LocalHandle
localInitWrap cfun sn pl res = do
    let cfun' = localInitFH cfun
    fp <- wrapResFN (\n _ -> peekCString n >>= res)
    withCString sn $ \csn ->
        withCString pl $ \cpl ->
            cfun' csn cpl fp


-------------------------------------------------------------------------------
-- DynM helpers

dmDS :: DynM DynState
dmDS = ST.get

dmDSGets :: (DynState -> a) -> DynM a
dmDSGets = ST.gets

dmDSPut :: DynState -> DynM ()
dmDSPut = ST.put

dmDSModify :: (DynState -> DynState) -> DynM ()
dmDSModify = ST.modify

dmGH :: DynM GraphHandle
dmGH = ST.gets dsGraph

dmWithGH :: (GraphHandle -> DynM a) -> DynM a
dmWithGH f = do { gh <- dmGH ; f gh }

dmWithGHIO :: (GraphHandle -> IO a) -> DynM a
dmWithGHIO f = dmWithGH (liftIO . f)

dmNewNodeHandle :: DynM NodeHandle
dmNewNodeHandle = do
    ds@DynState { dsNextNH = nx } <- ST.get
    ST.put $ ds { dsNextNH = nx + 1 }
    return $ NodeHandle nx

dmNewEdgeHandle :: DynM EdgeHandle
dmNewEdgeHandle = do
    ds@DynState { dsNextEH = nx } <- ST.get
    ST.put $ ds { dsNextEH = nx + 1 }
    return $ EdgeHandle nx

dmNewSpawnHandle :: DynM SpawnHandle
dmNewSpawnHandle = do
    ds@DynState { dsNextSH = nx } <- ST.get
    ST.put $ ds { dsNextSH = nx + 1 }
    return $ SpawnHandle nx

dmNewQueueHandle :: DynM QueueHandle
dmNewQueueHandle = do
    ds@DynState { dsNextQH = nx } <- ST.get
    ST.put $ ds { dsNextQH = nx + 1 }
    return $ QueueHandle nx

dmDebugPrint :: String -> DynM ()
dmDebugPrint s = do
    dsl <- dmDSGets dsLabel
    --liftIO $ putStrLn $ dsl ++ ": " ++ s
    return ()

-------------------------------------------------------------------------------
-- Convenient interface for pipeline interaction

stopGraph :: DynM ()
stopGraph = dmWithGHIO c_stopgraph

startGraph :: DynM ()
startGraph = dmWithGHIO c_startgraph

clearGraph :: DynM ()
clearGraph = dmWithGHIO c_cleargraph


mkFNode :: String -> String -> Bool -> DynM NodeHandle
mkFNode l i prod = do
    nh <- dmNewNodeHandle
    let pr = if prod then 1 else 0
    dmWithGHIO $ \gh ->
        withCString l $ \cl ->
            withCString i $ \ci -> c_mkfnode gh nh cl ci pr
    return nh

mkONode :: String -> PG.NOperator -> DynM NodeHandle
mkONode l o = do
    let opF = case o of
                PG.NOpAnd -> c_mkonode_and
                PG.NOpOr -> c_mkonode_or
                PG.NOpNAnd -> c_mkonode_nand
                PG.NOpNOr -> c_mkonode_nor
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> opF gh nh cl
    return nh

mkDemuxNode :: String -> DynM NodeHandle
mkDemuxNode l = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_demux gh nh cl
    return nh

mkMuxNode :: String -> MuxId -> DynM NodeHandle
mkMuxNode l i = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_mux gh nh cl i
    return nh

mkToQueueNode :: String -> QueueHandle -> DynM NodeHandle
mkToQueueNode l qh = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_toqueue gh nh cl qh
    return nh

mkFromQueueNode :: String -> QueueHandle -> DynM NodeHandle
mkFromQueueNode l qh = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_fromqueue gh nh cl qh
    return nh

mkToSocketNode :: String -> Word64 -> DynM NodeHandle
mkToSocketNode l sid = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_tosocket gh nh cl sid
    return nh

mkFromSocketNode :: String -> Word64 -> DynM NodeHandle
mkFromSocketNode l sid = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_fromsocket gh nh cl sid
    return nh

mkUDPDemuxNode ::
    String -> Word32 -> Word16 -> Word32 -> Word16 -> DynM NodeHandle
mkUDPDemuxNode l sIP sP dIP dP = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_udpdemux gh nh cl sIP sP dIP dP
    return nh

mkBalanceNode :: String -> DynM NodeHandle
mkBalanceNode l = do
    nh <- dmNewNodeHandle
    dmWithGHIO $ \gh ->
        withCString l $ \cl -> c_mknode_balance gh nh cl
    return nh



mkSpawn :: NodeHandle -> DynM SpawnHandle
mkSpawn nh = do
    sh <- dmNewSpawnHandle
    dmWithGHIO $ \gh -> c_mkspawn gh sh nh
    return sh

updateSpawn :: SpawnHandle -> NodeHandle -> DynM ()
updateSpawn sh nh = dmWithGHIO $ \gh -> c_updatespawn gh sh nh

rmSpawn :: SpawnHandle -> DynM ()
rmSpawn sh = dmWithGHIO $ \gh -> c_rmspawn gh sh

addPorts :: NodeHandle -> Int -> DynM ()
addPorts nh n = dmWithGHIO $ \gh -> c_addports gh nh (fromIntegral n)

addEdge :: NodeHandle -> Int -> NodeHandle -> DynM EdgeHandle
addEdge so p si = do
    eh <- dmNewEdgeHandle
    dmWithGHIO $ \gh -> c_addedge gh eh so (fromIntegral p) si
    return eh

addSpawn :: NodeHandle -> SpawnHandle -> DynM ()
addSpawn nh sh = dmWithGHIO $ \gh -> c_addspawn gh nh sh

addInit :: SpawnHandle -> DynM ()
addInit sh = dmWithGHIO $ \gh -> c_add_init gh sh


addInQueue :: String -> String -> DynM QueueHandle
addInQueue pl ep = do
    qh <- dmNewQueueHandle
    dmDSModify $ \ds -> ds { dsInQs = M.insert pl qh $ dsInQs ds }
    dmWithGHIO $ \gh ->
        withCString ep $ \cep -> c_addinqueue gh qh cep
    return qh

rmInQueue :: String -> QueueHandle -> DynM ()
rmInQueue pl qh = do
    dmWithGHIO $ \gh -> c_rminqueue gh qh
    dmDSModify $ \ds -> ds { dsInQs = M.delete pl $ dsInQs ds }

addOutQueue :: String -> String -> DynM QueueHandle
addOutQueue pl ep = do
    qh <- dmNewQueueHandle
    dmDSModify $ \ds -> ds { dsOutQs = M.insert pl qh $ dsOutQs ds }
    dmWithGHIO $ \gh ->
        withCString ep $ \cep -> c_addoutqueue gh qh cep
    return qh

rmOutQueue :: String -> QueueHandle -> DynM ()
rmOutQueue pl qh = do
    dmWithGHIO $ \gh -> c_rmoutqueue gh qh
    dmDSModify $ \ds -> ds { dsOutQs = M.delete pl $ dsOutQs ds }



-------------------------------------------------------------------------------
-- C Declarations for pipeline interface channel

foreign import ccall "dynrc_startgraph"
    c_startgraph :: GraphHandle -> IO ()
foreign import ccall "dynrc_stopgraph"
    c_stopgraph  :: GraphHandle -> IO ()
foreign import ccall "dynrc_cleargraph"
    c_cleargraph  :: GraphHandle -> IO ()


foreign import ccall "dynrc_mkfnode"
    c_mkfnode ::
        GraphHandle -> NodeHandle -> CString -> CString -> CInt -> IO ()
foreign import ccall "dynrc_mkonode_and"
    c_mkonode_and :: GraphHandle -> NodeHandle -> CString -> IO ()
foreign import ccall "dynrc_mkonode_or"
    c_mkonode_or :: GraphHandle -> NodeHandle -> CString -> IO ()
foreign import ccall "dynrc_mkonode_nand"
    c_mkonode_nand :: GraphHandle -> NodeHandle -> CString -> IO ()
foreign import ccall "dynrc_mkonode_nor"
    c_mkonode_nor :: GraphHandle -> NodeHandle -> CString -> IO ()
foreign import ccall "dynrc_mknode_demux"
    c_mknode_demux :: GraphHandle -> NodeHandle -> CString -> IO ()
foreign import ccall "dynrc_mknode_mux"
    c_mknode_mux :: GraphHandle -> NodeHandle -> CString -> MuxId -> IO ()
foreign import ccall "dynrc_mknode_toqueue"
    c_mknode_toqueue ::
        GraphHandle -> NodeHandle -> CString -> QueueHandle -> IO ()
foreign import ccall "dynrc_mknode_fromqueue"
    c_mknode_fromqueue ::
        GraphHandle -> NodeHandle -> CString -> QueueHandle -> IO ()
foreign import ccall "dynrc_mknode_tosocket"
    c_mknode_tosocket ::
        GraphHandle -> NodeHandle -> CString -> Word64 -> IO ()
foreign import ccall "dynrc_mknode_fromsocket"
    c_mknode_fromsocket ::
        GraphHandle -> NodeHandle -> CString -> Word64 -> IO ()
foreign import ccall "dynrc_mknode_udpdemux"
    c_mknode_udpdemux :: GraphHandle -> NodeHandle -> CString
            -> Word32 -> Word16 -> Word32 -> Word16 -> IO ()
foreign import ccall "dynrc_mknode_balance"
    c_mknode_balance :: GraphHandle -> NodeHandle -> CString -> IO ()


foreign import ccall "dynrc_mkspawn"
    c_mkspawn :: GraphHandle -> SpawnHandle -> NodeHandle -> IO ()
foreign import ccall "dynrc_updatespawn"
    c_updatespawn :: GraphHandle -> SpawnHandle -> NodeHandle -> IO ()
foreign import ccall "dynrc_rmspawn"
    c_rmspawn :: GraphHandle -> SpawnHandle -> IO ()
foreign import ccall "dynrc_addports"
    c_addports :: GraphHandle -> NodeHandle -> CSize -> IO ()
foreign import ccall "dynrc_addedge"
    c_addedge ::
        GraphHandle -> EdgeHandle -> NodeHandle -> CSize -> NodeHandle -> IO ()
foreign import ccall "dynrc_addspawn"
    c_addspawn :: GraphHandle -> NodeHandle -> SpawnHandle -> IO ()
foreign import ccall "dynrc_add_init"
    c_add_init :: GraphHandle -> SpawnHandle -> IO ()

foreign import ccall "dynrc_addinqueue"
    c_addinqueue :: GraphHandle -> QueueHandle -> CString -> IO ()
foreign import ccall "dynrc_rminqueue"
    c_rminqueue :: GraphHandle -> QueueHandle -> IO ()
foreign import ccall "dynrc_addoutqueue"
    c_addoutqueue :: GraphHandle -> QueueHandle -> CString -> IO ()
foreign import ccall "dynrc_rmoutqueue"
    c_rmoutqueue :: GraphHandle -> QueueHandle -> IO ()

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

