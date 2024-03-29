-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

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
    GraphHandle(..),

    runPipelines,
    runPipelines',
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
newtype GraphHandle = GraphHandle (Ptr GraphHandle)


{-|
 - Takes Pipeline graph, and way to generate connector nodes,
 -  and returns a separate implementation nodes for each pipeline
 -  (including proper connector nodes)
 -}
getPLIs ::
       (Pipeline -> Pipeline -> (POutput,PInput))
       -- ^ Function to generate connector node between two pipelines
    -> PLGraph
        -- ^ Pipeline graph to be executed
    -> [PipelineImpl]
        -- ^ Returns list of PipelineImpl (one for each pipeline)
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

{-|
 - Generates separate implementation graph for each pipeline with connector
 - nodes, and calls a initialization function for each of the pipeline.
 - Should give queue implementation for queue between first and second
 - pipeline
 -}
runPipelines' ::
    (Pipeline -> Pipeline -> (POutput,PInput))
     -- ^ Function to generate connector node between two pipelines
    -> (PipelineImpl -> IO a)
     -- ^ function to initialize a pipeline
    -> PLGraph
    -- ^ Pipeline graph to be executed
    -> IO [a]
    -- ^ Side-effects and returns answer (FIXME: which is ignored in `Pipelines/Dymamic.hs:run`!!)
runPipelines' qconf prun plg = do
    let plis = getPLIs qconf plg
    -- execute pipeline-initialization function for each pipeline
    results <- mapM prun plis
    return results


runPipelines ::
    -- Stack name
    String ->
    -- Should give queue implementation for queue between first and second
    -- pipeline
    (Pipeline -> Pipeline -> (POutput,PInput)) ->
    -- Initialize pipeline
    (PipelineImpl -> IO a) ->
    PLGraph -> IO (StackHandle,[a])
runPipelines sname qconf prun plg = do
    handle <- init_shared_state sname $ length $ concatMap pliInQs
                $ getPLIs qconf plg
    res <- runPipelines' qconf prun plg
    return (handle,res)

stopPipelines :: StackHandle -> IO ()
stopPipelines h = c_stop_stack h

stackState :: StackHandle -> IO StateHandle
stackState = c_stack_state


udpAddListen :: StateHandle -> Word64 -> Word64 -> Word16 -> IO UDPListenHandle
udpAddListen = c_udp_state_add_listen

udpRemoveListen :: StateHandle -> UDPListenHandle -> IO ()
udpRemoveListen = c_udp_state_remove_listen

udpAddFlow :: StateHandle -> Word64 -> Word64 -> Word32 -> Word16 -> Word32
                    -> Word16 -> IO UDPFlowHandle
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
        StateHandle -> Word64 -> Word64 -> Word16 -> IO UDPListenHandle

foreign import ccall "udp_state_remove_listen"
    c_udp_state_remove_listen :: StateHandle -> UDPListenHandle -> IO ()

foreign import ccall "udp_state_add_flow"
    c_udp_state_add_flow ::
        StateHandle -> Word64 -> Word64 -> Word32 -> Word16 -> Word32 -> Word16
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

