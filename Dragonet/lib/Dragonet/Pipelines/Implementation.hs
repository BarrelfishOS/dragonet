{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Dragonet.Pipelines.Implementation(
    PInput(..),
    POutput(..),
    PipelineImpl(..),

    StackHandle(..),
    StateHandle(..),
    PipelineHandle(..),
    QueueHandle(..),
    InputHandle(..),
    UDPListenHandle(..),
    UDPFlowHandle(..),

    runPipelines,
    stopPipelines,
    stackState,

    init_shared_state,
    pipeline_init,
    pipeline_get_state,
    pipeline_inqueue_create,
    pipeline_outqueue_bind,
    pipeline_wait_ready,

    udpAddListen,
    udpRemoveListen,
    udpAddFlow,
    udpRemoveFlow,
) where

import Dragonet.Pipelines
import qualified Dragonet.Implementation as I
import qualified Data.Graph.Inductive as DGI
import qualified Data.ByteString.Internal as BI
import Data.Word
import Data.Maybe
import Control.Concurrent

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

data PInput = PIQueue String
    deriving (Show, Eq, Ord)

data POutput = POQueue String
    deriving (Show, Eq, Ord)


data PipelineImpl = PipelineImpl {
    pliPipeline :: Pipeline,
    pliInQs :: [(PLabel,PInput)],
    pliOutQs :: [(PLabel,POutput)]
} deriving (Show)

newtype StackHandle = StackHandle (Ptr StackHandle)
newtype StateHandle = StateHandle (Ptr StateHandle)
newtype PipelineHandle = PipelineHandle (Ptr PipelineHandle)
newtype QueueHandle = QueueHandle (Ptr QueueHandle)
    deriving (Storable)
newtype InputHandle = InputHandle (Ptr InputHandle)
newtype UDPListenHandle = UDPListenHandle (Ptr UDPListenHandle)
newtype UDPFlowHandle = UDPFlowHandle (Ptr UDPFlowHandle)



getPLIs :: (Pipeline -> Pipeline -> (POutput,PInput)) -> PLGraph
        -> [PipelineImpl]
getPLIs qconf g = map n2pi $ DGI.labNodes g
    where
        inConf p n' = (plLabel p',snd $ qconf p' p)
            where (Just p') = DGI.lab g n'
        outConf p n' = (plLabel p',fst $ qconf p p')
            where (Just p') = DGI.lab g n'
        n2pi (n,p) = PipelineImpl {
            pliPipeline = p,
            pliInQs = map (inConf p) $ DGI.pre g n,
            pliOutQs = map (outConf p) $ DGI.suc g n }

runPipelines ::
    -- Stack name
    String ->
    -- Should give queue implementation for queue between first and second
    -- pipeline
    (Pipeline -> Pipeline -> (POutput,PInput)) ->
    -- Initialize pipeline
    (PipelineImpl -> IO ()) ->
    PLGraph -> IO StackHandle
runPipelines sname qconf prun plg = do
    let plis = getPLIs qconf plg
    handle <- init_shared_state sname (length $ concatMap pliInQs plis)
    mapM_ prun plis
    return handle

stopPipelines :: StackHandle -> IO ()
stopPipelines h = c_stop_stack h

stackState :: StackHandle -> IO StateHandle
stackState = c_stack_state


udpAddListen :: StateHandle -> Word64 -> Word16 -> IO UDPListenHandle
udpAddListen = c_udp_state_add_listen

udpRemoveListen :: StateHandle -> UDPListenHandle -> IO ()
udpRemoveListen = c_udp_state_remove_listen

udpAddFlow :: StateHandle -> Word64 -> Word32 -> Word16 -> Word32 -> Word16
                    -> IO UDPFlowHandle
udpAddFlow = c_udp_state_add_flow

udpRemoveFlow :: StateHandle -> UDPFlowHandle -> IO ()
udpRemoveFlow = c_udp_state_remove_flow


-- C interface

foreign import ccall "init_shared_state"
    c_init_shared_state :: CString -> CSize -> IO StackHandle

foreign import ccall "stop_stack"
    c_stop_stack :: StackHandle -> IO ()

foreign import ccall "stack_state"
    c_stack_state :: StackHandle -> IO StateHandle

foreign import ccall "pl_init"
    c_pl_init :: CString -> CString -> IO PipelineHandle

foreign import ccall "pl_get_state"
    c_pl_get_state :: PipelineHandle -> IO StateHandle

foreign import ccall "pl_inqueue_create"
    c_pl_inqueue_create :: PipelineHandle -> CString -> IO QueueHandle

foreign import ccall "pl_outqueue_bind"
    c_pl_outqueue_bind :: PipelineHandle -> CString -> IO QueueHandle

foreign import ccall "pl_wait_ready"
    c_pl_wait_ready :: PipelineHandle -> IO ()

foreign import ccall "udp_state_add_listen"
    c_udp_state_add_listen ::
        StateHandle -> Word64 -> Word16 -> IO UDPListenHandle

foreign import ccall "udp_state_remove_listen"
    c_udp_state_remove_listen :: StateHandle -> UDPListenHandle -> IO ()

foreign import ccall "udp_state_add_flow"
    c_udp_state_add_flow ::
        StateHandle -> Word64 -> Word32 -> Word16 -> Word32 -> Word16
                    -> IO UDPFlowHandle

foreign import ccall "udp_state_remove_flow"
    c_udp_state_remove_flow :: StateHandle -> UDPFlowHandle -> IO ()

init_shared_state :: String -> Int -> IO StackHandle
init_shared_state n c = withCString n $ \cs ->
    c_init_shared_state cs (fromIntegral c)

pipeline_init :: String -> String -> IO PipelineHandle
pipeline_init stackN plN =
    withCString stackN $ \csn -> withCString plN $ \cpn ->
        c_pl_init csn cpn

pipeline_get_state :: PipelineHandle -> IO StateHandle
pipeline_get_state = c_pl_get_state

pipeline_inqueue_create :: PipelineHandle -> String -> IO QueueHandle
pipeline_inqueue_create plh n = withCString n $ \cn ->
    c_pl_inqueue_create plh cn

pipeline_outqueue_bind :: PipelineHandle -> String -> IO QueueHandle
pipeline_outqueue_bind plh n = withCString n $ \cn ->
    c_pl_outqueue_bind plh cn

pipeline_wait_ready :: PipelineHandle -> IO ()
pipeline_wait_ready = c_pl_wait_ready

