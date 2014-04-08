{-# LANGUAGE ForeignFunctionInterface, DeriveGeneric #-}

module Dragonet.Pipelines.Implementation(
    PInput(..),
    POutput(..),
    PipelineImpl(..),
    StackHandle,
    runPipelines,
    stopPipelines
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


-- C interface

foreign import ccall "init_shared_state"
    c_init_shared_state :: CString -> CSize -> IO StackHandle

foreign import ccall "stop_stack"
    c_stop_stack :: StackHandle -> IO ()

init_shared_state :: String -> Int -> IO StackHandle
init_shared_state n c = withCString n $ \cs ->
    c_init_shared_state cs (fromIntegral c)

