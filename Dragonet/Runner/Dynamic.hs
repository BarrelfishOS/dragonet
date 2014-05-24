{-# LANGUAGE ForeignFunctionInterface #-}
module Runner.Dynamic (
    runPipeline
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PI
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

import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Control.Concurrent (forkOS,yield)
import Debug.Trace (trace)

newtype GraphHandle = GraphHandle (Ptr GraphHandle)
newtype NodeHandle = NodeHandle (Ptr NodeHandle)
newtype EdgeHandle = EdgeHandle (Ptr NodeHandle)
type NodeFun = FunPtr (PI.StateHandle -> PI.InputHandle -> IO Word32)
type MuxId = Word32

-- Build node
buildDynNode :: PI.PipelineImpl -> GraphHandle -> M.Map String PI.QueueHandle
        -> (String -> IO NodeFun) -> PG.Node -> IO NodeHandle
buildDynNode pli gh qm nf l
    | isDemux = mkDemuxNode gh lbl
    | isMux = mkMuxNode gh lbl muxId
    | isToQ = mkToQueueNode gh lbl qh
    | PG.nIsFNode l = do
        fp <- nf lbl
        mkFNode gh lbl fp
    | PG.nIsONode l = mkONode gh lbl op
    | otherwise = error "Unsupported node type"
    where
        lbl = PG.nLabel l
        isDemux = PG.nIsFNode l && PG.nLabel l == "Demux"

        mbMuxId = getPGNAttr l "muxid"
        isMux = isJust mbMuxId
        Just muxId = read <$> mbMuxId

        mbPL = getPGNAttr l "pipeline"
        isToQ = isJust mbPL
        Just pln = mbPL
        (Just qh) = M.lookup pln qm

        (PG.ONode op) = PG.nPersonality l


addMuxIds :: PL.PLGraph -> PG.PGraph -> PG.PGraph
addMuxIds plg pg = flip DGI.nmap pg $ \node ->
    case (getPGNAttr node "multiplex",getPGNAttr node "muxPL") of
        (Just aDN,Just aDP) ->
            node { PG.nAttributes = PG.nAttributes node ++
                    ["muxid=" ++ (show $ muxId plg aDN aDP)] }
        _ -> node

muxId :: PL.PLGraph -> String -> String -> MuxId
muxId plg dNodeL dPL = i
    where
        -- Dest Pipeline
        Just dpg =
            PL.plGraph <$> snd <$> findNodeByL ((==) dPL . PL.plLabel) plg
        -- Demux node
        Just (dxn,dxnL)  = findNodeByL ((==) "Demux" . PG.nLabel) dpg
        -- get map output port -> ID
        dxPorts = flip zip [0..] $ PG.nPorts dxnL
        Just i = lookup dNodeL dxPorts


-- Build graph for pipeline
buildDynGraph :: PL.PLGraph -> PI.PipelineImpl -> M.Map String PI.QueueHandle
        -> (String -> IO NodeFun) -> IO GraphHandle
buildDynGraph plg pli qm nf = do
    gh <- mkGraph
    -- Create nodes
    nlm <- forM (DGI.labNodes pg) $ \(n,l) -> do
        h <- buildDynNode pli gh qm nf l
        addPorts h $ length $ PG.nPorts l
        return (n,h)
    let nm = M.fromList nlm
    -- Set source node
    let entries = [n |(Just n) <- map ((`M.lookup` nm) . fst) $ PG.pgEntries pg]
    mapM_ (addSource gh) entries
    -- Add edges
    forM (DGI.labEdges pg) $ \(a,b,p) -> do
        let Just al = DGI.lab pg a
            Just pi = lookup p $ flip zip [0..] $ fixBoolP $ PG.nPorts al
            Just src = M.lookup a nm
            Just dst = M.lookup b nm
        addEdge src pi dst
    return gh
    where
        pg = addMuxIds plg $ PL.plGraph $ PI.pliPipeline pli
        fixBoolP l
            | "true" `elem` l && "false" `elem` l && length l == 2 =
                ["false", "true"]
            | otherwise = l


-- Initialize pipeline and queues
{-initPipeline :: String -> PI.iPipelineImpl
        -> IO (PI.PipelineHandle,M.Map String PI.QueueHandle)-}
initPipeline stackname pli pl_funs = do
    plh <- pl_init stackname $ PL.plLabel $ PI.pliPipeline pli
    -- Create input queues
    forM (PI.pliInQs pli) $ \(_,PI.PIQueue qn) -> do
        pl_inq plh qn

    -- Bind output queues
    qml <- forM (PI.pliOutQs pli) $ \(l,PI.POQueue qn) -> do
        qh <- pl_outq plh qn
        return (l,qh)

    pl_wait plh
    return (plh,M.fromList qml)
    where
        (pl_init,pl_inq,pl_outq,pl_wait) = pl_funs

withHelpers :: String -> ((String -> IO (FunPtr b)) -> IO a) -> IO a
withHelpers llvm_helpers run = do
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            LLVMPM.withPassManager passes $ \pm -> do
                LLVMPM.runPassManager pm mod
                withJitEE ctx $ \ee -> do
                    LLVMEE.withModuleInEngine ee mod $ \emod -> do
                        run (pgf emod)

    where
        file = LLVMMod.File llvm_helpers
        pgf emod n = do
            Just f <- LLVMEE.getFunction emod (AST.Name n)
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

runPipeline :: PL.PLGraph -> String -> String -> PI.PipelineImpl -> IO ()
runPipeline plg stackname helpers pli = fmap (const ()) $ forkOS $ do
    putStrLn $ "Initializing pipeline " ++ mname
    withHelpers llvm_helpers $ \hf -> do
        putStrLn "Helpers ready"
        pl_funs <- pl_get_funs hf
        (plh,qm) <- initPipeline stackname pli pl_funs
        putStrLn "Pipeline Queues initialized"
        splh <- hf "set_pipeline_handle"
        splhFun splh plh
        let nodeFun l = castFunPtr <$> (hf $ "do_pg__" ++ l)
        gh <- buildDynGraph plg pli qm (nodeFun :: String -> IO NodeFun)
        putStrLn "Running Graph"
        runGraph gh plh
        putStrLn "Graph initialized"
    putStrLn $ "Pipeline " ++ mname ++ " stopped running"
    where
        pl = PI.pliPipeline pli
        mname = PL.plLabel pl
        -- LLVM file with helper utilities
        llvm_helpers = "dist/build/" ++ helpers ++ ".bc"
        pl_get_funs hf = do
            pl_init <- castFunPtr <$> hf "pl_init"
            pl_inq <- castFunPtr <$> hf "pl_inqueue_create"
            pl_outq <- castFunPtr <$> hf "pl_outqueue_bind"
            pl_wait <- castFunPtr <$> hf "pl_wait_ready"
            return (pl_init_wrapper pl_init,
                    pl_qprep_wrapper pl_inq,
                    pl_qprep_wrapper pl_outq,
                    c_pl_wait_ready pl_wait)

-------------------------------------------------------------------------------
-- Convenient functions :)

mkGraph :: IO GraphHandle
mkGraph = c_mkgraph

addSource :: GraphHandle -> NodeHandle -> IO ()
addSource = c_add_source

runGraph :: GraphHandle -> PI.PipelineHandle -> IO ()
runGraph gh ph = c_run_graph gh ph

mkFNode :: GraphHandle -> String -> NodeFun -> IO NodeHandle
mkFNode gh l f = withCString l $ \cl -> c_mkfnode gh cl f

mkONode :: GraphHandle -> String -> PG.Operator -> IO NodeHandle
mkONode gh l o = withCString l $ \cl -> case o of
    PG.OpAnd -> c_mkonode_and gh cl
    PG.OpOr -> c_mkonode_or gh cl
    PG.OpNAnd -> c_mkonode_nand gh cl
    PG.OpNOr -> c_mkonode_nor gh cl

mkDemuxNode :: GraphHandle -> String -> IO NodeHandle
mkDemuxNode gh l = withCString l $ \cl -> c_mknode_demux gh cl

mkMuxNode :: GraphHandle -> String -> MuxId -> IO NodeHandle
mkMuxNode gh l i = withCString l $ \cl -> c_mknode_mux gh cl i

mkToQueueNode :: GraphHandle -> String -> PI.QueueHandle -> IO NodeHandle
mkToQueueNode gh l qh = withCString l $ \cl -> c_mknode_toqueue gh cl qh

addPorts :: NodeHandle -> Int -> IO Int
addPorts nh n = fromIntegral <$> c_addports nh (fromIntegral n)

addEdge :: NodeHandle -> Int -> NodeHandle -> IO EdgeHandle
addEdge so p si = c_addedge so (fromIntegral p) si

-------------------------------------------------------------------------------
-- C Declarations

foreign import ccall "dyn_mkgraph"
    c_mkgraph :: IO GraphHandle
foreign import ccall "dyn_add_source"
    c_add_source :: GraphHandle -> NodeHandle -> IO ()
foreign import ccall "dyn_rungraph"
    c_run_graph ::
        GraphHandle -> PI.PipelineHandle -> IO ()
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
foreign import ccall "dyn_addports"
    c_addports :: NodeHandle -> CSize -> IO CSize
foreign import ccall "dyn_addedge"
    c_addedge :: NodeHandle -> CSize -> NodeHandle -> IO EdgeHandle

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

getPGNAttr :: PG.Node -> String -> Maybe String
getPGNAttr node n =
    drop (length n + 1) <$>
        (L.find (L.isPrefixOf (n ++ "=")) $ PG.nAttributes node)



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

