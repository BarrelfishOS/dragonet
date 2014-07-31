module Dragonet.Pipelines.Dynamic (
    DynPipeline(..),
    DynContext,
    initialContext,
    ctxState,
    run
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Control.Concurrent.STM as STM
import qualified Util.Misc as UM

import Control.Monad (forM_)
import Data.List (find)

data DynPipeline = DynPipeline {
    dplLabel      :: PL.PLabel,
    dplStart      :: IO (),
    dplStop       :: IO (),
    dplInQAdd     :: PL.PLabel -> PLI.PInput -> IO (),
    dplInQRemove  :: PL.PLabel -> IO (),
    dplOutQAdd    :: PL.PLabel -> PLI.POutput -> IO (),
    dplOutQRemove :: PL.PLabel -> IO (),
    dplSetGraph   :: PG.PGraph -> IO (),
    dplDestroy    :: IO ()
}

data DynState = DynState {
    dsHandle    :: PLI.StackHandle,
    dsPipelines :: [(DynPipeline,PLI.PipelineImpl)]
}

type DynContext = STM.TVar DynState

initialContext :: String -> IO DynContext
initialContext sname = do
    sh <- PLI.init_shared_state sname 0
    STM.newTVarIO $
        DynState {
            dsHandle = sh,
            dsPipelines = [] }

ctxState :: DynContext -> IO PLI.StackHandle
ctxState dctx = do
    ds <- STM.atomically $ STM.readTVar dctx
    return $ dsHandle ds

dsPipeline :: DynState -> PL.PLabel -> Maybe (DynPipeline, PLI.PipelineImpl)
dsPipeline ds pl = find (\(dpl,_) -> dplLabel dpl == pl) $ dsPipelines ds

dsAddDPL :: (DynPipeline,PLI.PipelineImpl) -> DynState -> DynState
dsAddDPL x@(_,pli) ds@DynState { dsPipelines = pls } =
    ds { dsPipelines = filter noMatch pls ++ [x] }
    where
        lbl (_,pli') = PL.plLabel $ PLI.pliPipeline pli'
        noMatch y = lbl y /= lbl x

diff :: Eq a => [a] -> [a] -> ([a],[a])
diff old new = (old `UM.minusL` new, new `UM.minusL` old)

run ::
    DynContext ->
    (PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)) ->
    (PL.PLabel -> IO DynPipeline) ->
    PL.PLGraph ->
    IO ()
run ctx pconn pcreate plg = do
        PLI.runPipelines' pconn pinit plg
        return ()
    where
    pinit pli = do
        let pl = PLI.pliPipeline pli
            lbl = PL.plLabel pl
        ds <- STM.readTVarIO ctx
        -- Get/create dpl
        (dpl,ins,outs) <- case dsPipeline ds lbl of
                Just x@(dpl,pli') -> do
                    dplStop dpl
                    return (dpl, PLI.pliInQs pli', PLI.pliOutQs pli')
                Nothing -> do
                    dpl <- pcreate lbl
                    return (dpl, [], [])
        -- Update queues between pipelines
        let (inDel,inAdd) = diff ins $ PLI.pliInQs pli
            (outDel,outAdd) = diff outs $ PLI.pliOutQs pli
        forM_ inDel $ \(l,_) -> dplInQRemove dpl l
        forM_ outDel $ \(l,_) -> dplOutQRemove dpl l
        forM_ outAdd $ \(l,q) -> dplOutQAdd dpl l q
        forM_ inAdd $ \(l,q) -> dplInQAdd dpl l q
        -- Update graph
        dplSetGraph dpl $ PL.plGraph pl
        -- Restart execution of pipeline
        dplStart dpl
        STM.atomically $ STM.modifyTVar ctx (dsAddDPL (dpl,pli))


