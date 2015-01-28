{-# LANGUAGE ExistentialQuantification #-}
module Stack (
    instantiateOpt,
    instantiateFlowsIO_,
    instantiateFlows,
    instantiateFlows_,

    instantiateIncrFlowsIO_,

    StackState(..),
    StackArgs(..),
    StackPrgArgs(..),

    AppDesc(..),
) where

import qualified Dragonet.Configuration            as C
import qualified Dragonet.Optimization             as O
import qualified Dragonet.Pipelines                as PL
import qualified Dragonet.Pipelines.Dynamic        as PLD
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications   as PLA
import qualified Dragonet.ProtocolGraph            as PG
import qualified Dragonet.ProtocolGraph.Utils      as PGU
import qualified Dragonet.Semantics                as Sem
import qualified Dragonet.Search                   as SE
import qualified Dragonet.NetState                 as NS
import qualified Dragonet.Flows                    as FL

import Dragonet.NetState (NetState, SocketId,AppId,EndpointId,EndpointDesc(..),epsDiff)
import Dragonet.Flows (Flow(..), epToFlow, flowStr)

import qualified Graphs.Cfg as GCFG

import qualified Data.Graph.Inductive as DGI
import Util.GraphHelpers (findNodeByL)

import Control.Applicative ((<$>))

import qualified Graphs.LPG as LPG
import qualified Graphs.ImplTransforms as IT

import Runner.Dynamic (createPipeline, createPipelineClient)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits (testBit)

import Util.XTimeIt (doTimeIt,dontTimeIt)
import Text.Show.Pretty (ppShow)

putStrLnDbg x = putStrLn x
putStrLnDbgN x = return ()
--putStrLnDbgN x = putStrLn x


-- StackArgs: static arguments for instantiating the stack
--  Don't use StackArgs to construct it, use stackArgsDefault
--  If you want to call it directly, read the comments at stackArgsDefault
type PrgApplyCfg = (PLI.StateHandle -> C.Configuration -> IO ())
type PlaConfigure = (StackState -> String -> PG.PGNode -> String)
type GraphH = (PG.PGraph, Sem.Helpers)
data StackArgs = StackArgs {
    -- NIC-specific fields
      stPrg       :: StackPrgArgs -- | PRG args
    -- other fields
    , stLpgH      :: GraphH -- | (unconfigured) LPG (+sem helpers)
    , stCfgPLA    :: PlaConfigure -- | Group the LPG nodes into pipelines
    , stName      :: String -- | stack name
    -- helper fiels (can be build from fields above)
    , stMergedH   :: Sem.Helpers -- | merged helpers
}

data StackPrgArgs = StackPrgArgs {
      stPrgH      :: GraphH -- | (unconfigured) PRG (+sem helpers)
    , stLlvmH     :: String -- | LLVM helpers filename
    , stCfgImpl   :: PrgApplyCfg -- | implement given PRG conf
}

-- helper for constructing StackArgs
stackArgsDefault :: IO StackArgs
stackArgsDefault = do
    lpgH <- LPG.graphH -- default LPG
    return $ StackArgs {
    -- H/W specific fields: must be set by the caller
      stPrg       = undefined
    -- fields with default values: may be set by the caller
    , stLpgH      = lpgH
    , stCfgPLA    = plAssignByTag
    , stName      = "dragonet"
    -- fields set by initInstArgs: must not be set by the caller
    , stMergedH   = undefined
  }
  where plAssignByTag :: StackState -> String -> PG.PGNode -> String
        plAssignByTag _ _ (_,n) = PG.nTag n

-- initialize stack arguments
--   sets fields that are not set by the user
initStackArgs :: StackArgs -> StackArgs
initStackArgs args =  args {stMergedH =  mergedHelpers}
    where (_,prgHelpers) = stPrgH $ stPrg args
          (_,lpgHelpers) = stLpgH args
          mergedHelpers = prgHelpers `Sem.mergeHelpers` lpgHelpers

-- application descriptor
data AppDesc = AppDesc {
    adLabel :: String,
    adGraphHandle :: PLI.GraphHandle
}

-- StackState: dynamic stack state (changes as flows come and go)
-- TODO: Add StackArgs here
data StackState = StackState {
      ssNetState       :: NetState
    , ssApplications   :: M.Map AppId AppDesc
    , ssAppChans       :: M.Map PLA.ChanHandle AppId
    -- callback to update the graph
    , ssUpdateGraphs   :: STM.TVar StackState -> IO ()
    -- each stack state gets a new version number
    , ssVersion        :: Int
    -- state about (explicitly) registered flows
    , ssFlowsSt        :: FL.FlowsSt
    -- pipeline handlers
    , ssCtx            :: PLD.DynContext
    , ssStackHandle    :: PLI.StackHandle
    , ssSharedState    :: PLI.StateHandle
}

ssEndpoints ss = NS.nsEndpoints $ ssNetState ss

ssRunNetState_ :: StackState -> NS.NetStateM a -> (a, NetState)
ssRunNetState_ ss m = NS.runState m ns
    where ns = ssNetState ss

ssRunNetState :: StackState -> NS.NetStateM a -> (a, StackState)
ssRunNetState ss m = (x,ss')
    where (x,ns') = ssRunNetState_ ss m
          ss' = ss { ssNetState = ns'}

-- return a configured LPG based on endpoint information
ssConfigureLpg :: StackState -> StackArgs -> PG.PGraph
ssConfigureLpg ss args = C.applyConfig lpgCfg lpgU
    where allEps   = M.elems $ ssEndpoints ss
          lpgCfg   = LPG.lpgConfig allEps
          (lpgU,_) = stLpgH args

-- wrapper for O.makegraph
ssMakeGraph :: StackState
            -> StackArgs
            -> C.Configuration
            -> String
            -> [PG.PGraph -> PG.PGraph]
            -> IO PL.PLGraph
ssMakeGraph ss args prgConf lbl xforms = do
    let mergeH     = stMergedH args
        (prgU,_)   = stPrgH $ stPrg args
        lpgC       = ssConfigureLpg ss args
        -- graph transformations
        debug :: O.DbgFunction ()
        debug = O.dbgDotfiles $ "out/graphs-fff/" ++ (show $ ssVersion ss)
        --dbg = O.dbgDummy lbl
        dbg    = debug lbl
        cfgPLA = stCfgPLA args
        pla    = (plAssign cfgPLA ss)
    O.makeGraph mergeH prgU lpgC xforms (pla lbl) dbg prgConf

-- wrapper for O.optimize
ssOptimize :: (Ord a, Show a)
           => StackState -> StackArgs
           -> (StackState -> [EndpointDesc] -> O.CostFunction a)
           -> (StackState -> [(String,C.Configuration)])
           -> IO (PL.PLGraph, (String, C.Configuration), PG.PGraph)
ssOptimize ss args costFun cfgOracle = do
    let mergeH    = stMergedH args
        (prgU,_)  = stPrgH $ stPrg args
        lpgC      = ssConfigureLpg ss args
        -- graph transformations
        allEps     = M.elems $ ssEndpoints ss
        implXForms = [IT.coupleTxSockets, IT.mergeSockets]
        --dbg :: O.DbgFunction ()
        dbg = O.dbgDotfiles $ "out/graphs-ooo/" ++ (show $ ssVersion ss)
        --dbg = O.dbgDummy
        cfgPLA = stCfgPLA args
        pla  = (plAssign cfgPLA ss)
        costFun' = costFun ss allEps
        pCfgs = cfgOracle ss
    O.optimize mergeH prgU lpgC implXForms pla dbg costFun' pCfgs

-- Force socket nodes in their respective pipeline, for the rest use the
-- function f
plAssign f ss cfg m@(_,n)
    | Just said <- PGU.getPGNAttr n "appid" = "App" ++ said
    | otherwise = f ss cfg m

-- Creates input and output nodes to connect two pipelines
plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

addMuxIdsPG :: PL.PLGraph -> PG.PGraph -> PG.PGraph
addMuxIdsPG plg pg = flip DGI.nmap pg $ \node ->
    case (PGU.getPGNAttr node "multiplex",PGU.getPGNAttr node "muxPL") of
        (Just aDN,Just aDP) ->
            node { PG.nAttributes = PG.nAttributes node ++
                    [PG.NAttrCustom $ "muxid=" ++ (show $ muxId plg aDN aDP)] }
        _ -> node

addMuxIds :: PL.PLGraph -> PL.PLGraph
addMuxIds plg = DGI.nmap fixPL plg
    where fixPL pl = pl { PL.plGraph = addMuxIdsPG plg $ PL.plGraph pl }

muxId :: PL.PLGraph -> String -> String -> Int
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


--------------------------------------------------------------------------------
-- Handlers for app interface events
{-|
 - This function handles the events received from the application.
 - The supported event-types are appConnected, AppRegister, UDPBind,
 -      UDPFlow, and Span
 -}

eventHandle :: STM.TVar StackState -> PLA.ChanHandle -> PLA.Event -> IO ()
eventHandle sstv ch event = do
    putStrLnDbgN $ "EventHandle => channel: "
             ++ (show ch) ++ " Event: " ++ (show event)
    eventHandler sstv ch event

eventHandler :: STM.TVar StackState -> PLA.ChanHandle -> PLA.Event -> IO ()
{-|
 -  Handing event AppConnected:
 -      * Assign current appID as application-identifier
 -      * Increase appID
 -      * Add channel into list of application channels
 -      * Insert applicationID in list of applications
 -}
eventHandler sstv ch (PLA.EvAppConnected gh) = do
    STM.atomically $ do
        ss <- STM.readTVar sstv
        let (aid, ns') = ssRunNetState_ ss NS.allocAppId
            app = AppDesc {adLabel = "", adGraphHandle = gh}
        STM.writeTVar sstv $ ss {
                    ssNetState = ns',
                    ssAppChans = M.insert ch aid $ ssAppChans ss,
                    ssApplications = M.insert aid app $ ssApplications ss
                }
        return ()

{-|
 -  Handing event AppRegister:
 -      * Lookup appID and channel handle
 -      * Insert applicationID in list of applications (FIXME: Again, why?
 -          its already done in AppConnected event)
 -      * Send welcome message back to the application with an applicationID
 -}
eventHandler sstv ch (PLA.EvAppRegister lbl flags) = do
    aid <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            Just app = M.lookup aid $ ssApplications ss
        STM.writeTVar sstv $ ss {
                ssApplications = M.insert aid app $ ssApplications ss
            }
        return aid
    putStrLnDbgN $ "AppRegister: " ++ lbl ++ "[" ++ show ch ++ "] -> " ++ show aid
                            ++ ", flags: " ++ show flags
    PLA.sendMessage ch $ PLA.MsgWelcome aid

{-|
 -  Handing event SocketUDPBind:
 -      * Update network state
 -      * Send back a message to application with socketInfo and sid
 -}
eventHandler sstv ch
             (PLA.EvSocketUDPBind le@(lIp,lPort) re@(rIp,rPort) flags) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            ((sid,eid), ss') = ssRunNetState ss (NS.udpBind aid (le,re))
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLnDbgN $ "SocketUDPBind f=" ++ show re ++ "/" ++ show le ++ "- "
               ++ show sid ++ ", flags: " ++ show flags
    if testBit flags PLA.appFlagsMore
        then return ()
        else ssUpdateGraphs ss sstv
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid

{-|
 -  Handing event SocketUDPFlow:
 -      Registers a flow
 -}
eventHandler sstv ch
             (PLA.EvSocketUDPFlow sid le@(lIp,lPort) re@(rIp,rPort) flags) = do
    -- update flows state
    ss <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let fl = epToFlow $ NS.mkUdpEndpoint le re
            ss' = ss {ssFlowsSt = FL.fsAddFlow (ssFlowsSt ss) fl}
        STM.writeTVar sstv $ ss'
        return ss'
    putStrLnDbgN $ "SocketUDPFlow f=" ++ show re ++ "/" ++ show le
                ++ " for " ++ show sid ++ ", flags: " ++  show flags
    -- Sending message to app before we actually reflect this on stack
    PLA.sendMessage ch $ PLA.MsgStatus True
    if testBit flags PLA.appFlagsMore
        then return ()
        else ssUpdateGraphs ss sstv
    -- Sending message to app after  we reflect this on stack
    --PLA.sendMessage ch $ PLA.MsgStatus True

{-|
 -  Handling event SocketSpan:
 -      It creates a new socket with all the configuration from specified
 -      socket, including endpoint, application-id.
 -      It differs in socket-id (new socket-id is used)
 -      It adds this new socket into the list of sockets associated with
 -              the endpoint from old socket
 -
 -      * It sends an application message with socket-info and new socket-id
 -      * It calls the updateGraph to reflect new changes
 -}
eventHandler sstv ch (PLA.EvSocketSpan oldsid flags) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            (sid,ss') = ssRunNetState ss (NS.socketSpan aid oldsid)
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLnDbgN $ "SocketSpan existing=" ++ show oldsid ++ " for sid:" ++ show sid
                    ++ ", flags: " ++  show flags
    if testBit flags PLA.appFlagsMore
        then return ()
        else ssUpdateGraphs ss sstv
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid

eventHandler sstv ch (PLA.EvNop flags) = do
    ss <- STM.atomically $ STM.readTVar sstv
    putStrLnDbgN $ "#### NOOP called from " ++ show ch ++ " with flags " ++ show flags
    if testBit flags PLA.appFlagsMore
        then return ()
        else ssUpdateGraphs ss sstv
    PLA.sendMessage ch $ PLA.MsgStatus True
{-|
 -  Handling event "all other types":
 -      This is pretty much an error case.  Currently we are only printing
 -      the event Name and egnoring it.
 -  TODO: Should I report error to an application here?
 -}
eventHandler sstv ch ev = do
    putStrLnDbgN $ "eventHandler: UNHANDLED EVENT: " ++ show ch ++ " " ++ show ev


--------------------------------------------------------------------------------
-- Main

initStackSt = StackState {
    -- need to be set by the caller
    ssUpdateGraphs = undefined,
    ssCtx = undefined,
    ssStackHandle = undefined,
    ssSharedState = undefined,
    -- initial values
    ssNetState       = NS.initNetSt,
    ssApplications   = M.empty,
    ssAppChans       = M.empty,
    ssVersion        = 0,
    ssFlowsSt        = FL.flowsStInit
}

-- initialize stack state
ssInitialize
    -- | Function to update the graph
    :: (STM.TVar StackState -> IO ())
    -> StackArgs
    -- | Updated stack-state and IO actions
    -> IO (STM.TVar StackState)
ssInitialize updateGraph args = do
    ctx <- PLD.initialContext (stName args)
    stackHandle <- PLD.ctxState ctx
    sharedState <- PLI.stackState stackHandle
    STM.atomically $ STM.newTVar $
             initStackSt {
                  ssUpdateGraphs = updateGraph
                , ssCtx          = ctx
                , ssStackHandle  = stackHandle
                , ssSharedState  = sharedState
             }

-- bump the version number
ssNewVer :: STM.TVar StackState -> IO (StackState)
ssNewVer sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let ss' = ss { ssVersion = ssVersion ss + 1 }
    STM.writeTVar sstv ss'
    return ss'

-- bumps version number and resets the flow state. Returns new stack state and
-- old flow state
ssUpdateGraph sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let flowsSt = ssFlowsSt ss
        flowsSt' = FL.fsReset flowsSt
        ss' = ss {
                ssVersion = ssVersion ss + 1,
                ssFlowsSt = flowsSt'
        }
    STM.writeTVar sstv ss'
    return (ss', flowsSt)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type UpdateGraphFn = STM.TVar StackState -> IO ()
instantiateStack :: StackArgs -> UpdateGraphFn -> IO ()
instantiateStack args updateGraph = do
    putStrLn "Let's fire her up!"
    -- initialize stack state and call updateGraph for the first time
    sstv <- ssInitialize updateGraph args
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread (stName args) (eventHandle sstv)

--  Function: depending on userspace/driverspace pipeline
--       create pipeline-threads
--  QUESTION: Do these pipelines are getting created everytime
--      updateGraph is being called?  They should happen only
--      in begining, and whenever new application arrives.
ssCreatePL
    :: StackArgs
    -> StackState
    -> PL.PLGraph
    -> PL.PLabel
    -> IO PLD.DynPipeline
ssCreatePL args ss plg pl@('A':'p':'p':aids) = do
    -- Creating application pipeline.  Untrusted zone
    --      in application address-space
    putStrLnDbgN $ "Creating App pipeline: " ++ pl
    createPipelineClient agh pl
    where
      aid = read aids
      Just app = M.lookup aid $ ssApplications ss
      agh = adGraphHandle app
ssCreatePL args ss plg pl = do
    -- Creating stack-pipeline. Trusted zone
    --      in dragonet address-space
    putStrLnDbgN $ "Creating local pipeline: ##### " ++ pl
    -- Creates a thread to handle the pipeline
    --      NOTE: following function will actually create a thread
    createPipeline plg (stName args) (stLlvmH $ stPrg args) pl

-- updateGraph function that uses O.optimize
updateGraphOpt
    :: (Show a, Ord a)
    => (StackState -> [EndpointDesc] -> O.CostFunction a) -- cost function
    -> (StackState -> [(String,C.Configuration)]) -- oracle
    -> StackArgs
    -> STM.TVar StackState
    -> IO ()
updateGraphOpt costFn cfgOracle args sstv = do
    ss <- ssNewVer sstv
    putStrLnDbgN $ "updateGraphOpt: v" ++ (show $ ssVersion ss)
    let lpgC = ssConfigureLpg ss args

    -- STEP: Generate an Optimize combined graph,
    --    based on the above PRG, LPG and transformations
    (plg,(_,pCfg),prgpc) <- ssOptimize ss args costFn cfgOracle

     -- STEP: apply PRG configuration
     --    This is the step where actually filters will be inserted in the NIC
    (stCfgImpl $ stPrg args) (ssSharedState ss) pCfg

    -- STEP: Run pipelines
    PLD.run (ssCtx ss) plConnect (ssCreatePL args ss plg) $ addMuxIds plg

    --putStrLn "updateGraph exit"
    -- FIXME: Create file here.
    --putStrLn "################### calling appendFile with app ready notice"
    appendFile "allAppslist.appready" "Application is ready!\n"


updateGraphFlows
    :: ([Flow] -> IO C.Configuration)
    -> StackArgs
    -> STM.TVar StackState
    -> IO ()
updateGraphFlows getConf args sstv = do
   putStrLnDbgN "updateGraphFlows"
   (ss,flowStOld) <- ssUpdateGraph sstv
   putStrLn $ "updateGraphFlows: v" ++ (show $ ssVersion ss)
   -- get list of all endpoints from stack-state
   let
       lbl = "updateGraphFlows"
--       allEps   = M.elems $ ssEndpoints ss
--       prevEps = M.elems prevEpsM
--       newEps = epsDiff allEps prevEps
--       rmEps = epsDiff prevEps allEps
       xforms = [IT.balanceAcrossRxQs, IT.coupleTxSockets]

       -- non-incremental version
       -- we just use the new flowSt to calculate all flows
       flows' = S.toList $ FL.fsCurrent $ ssFlowsSt ss
       -- Reversal of flows was needed in old code.  Let me see if I still need it -PS
       --flows = reverse $ flows'
       flows =  flows'

   putStrLnDbgN $ "Flows:\n" ++ (ppShow $ map flowStr flows)
   --putStrLn $ "Flow count: " ++ (show $ length flows)
   -- These messages are very useful for debugging: PS
--   putStrLn $ "=====> LAST EPS: " ++ (ppShow prevEps)
--   putStrLn $ "=====> REMOVED: " ++ (ppShow rmEps)
--   putStrLn $ "=====> ADDED: " ++ (ppShow newEps)
--   putStrLn $ "=====> CURRENT: " ++ (ppShow allEps)

   prgConf <- getConf flows
   --putStrLn $ "generated conf:\n" ++ (ppShow $ prgConf)

   -- STEP: Create a new combined, but pipelined graph
   --          with both LPG and PRG with configuration applied
   plg <- ssMakeGraph ss args prgConf lbl xforms

   -- STEP: apply PRG configuration
   -- NOTE: This is the step where actually filters will be inserted
   --      in the NIC
   (stCfgImpl $ stPrg args) (ssSharedState ss) prgConf

   -- STEP: Run each pipeline
   --   * Create pipeline in separate thread if it don't exist
   --   * If pipeline exist then
   --       + stop it
   --       + Update the in/out connecting queues based on changes
   -- NOTE: ctx is still an initial context, is that efficient?
   PLD.run (ssCtx ss) plConnect (ssCreatePL args ss plg) $ addMuxIds plg

   putStrLnDbgN "updateGraph exit"
   -- FIXME: Create file here.
   putStrLnDbgN "################### calling appendFile with app ready notice"
   appendFile("allAppslist.appready") $ "Application is ready!\n"


-- OLD/Deprecated interface


instantiateFlowsIO_
    :: ([Flow] -> IO (C.Configuration))
    -> (StackState -> String -> PG.PGNode -> String)
    -> StackPrgArgs
    -> IO ()
instantiateFlowsIO_ getConfIO cfgPLA prgArgs = do
    args0 <- stackArgsDefault
    let args = initStackArgs $ args0 { stPrg    = prgArgs
                                     , stCfgPLA = cfgPLA }
        updFn = updateGraphFlows getConfIO args
    instantiateStack args updFn

instantiateSearchIO :: (SE.OracleSt o a)
                    => SE.SearchParams o a
                    -> StackArgs
                    -> IO ()
instantiateSearchIO params args = do
    -- initialize search state
    searchSt <- SE.initSearchIO params
    let getConfIO :: [Flow] -> IO (C.Configuration)
        getConfIO = SE.runSearchIO searchSt
        updFn = updateGraphFlows getConfIO args
    instantiateStack args updFn



instantiateFlows_
        -- | Function to get a configuration by searching using Oracle
        :: ([Flow] -> C.Configuration)
        -- | function to Group the LPG nodes into pipelines
        -> (StackState -> String -> PG.PGNode -> String)
        -> StackPrgArgs
        -> IO ()
instantiateFlows_ getConf cfgPLA prgArgs = do
    args0 <- stackArgsDefault
    let args = initStackArgs $ args0 { stPrg    = prgArgs
                                     , stCfgPLA = cfgPLA }
        updFn = updateGraphFlows getConf' args
        getConf' flows = return $ getConf flows
    instantiateStack args updFn

-- instantiateFlows function that uses the new interface
-- TODO: remove this and have the calleres use the new interface
instantiateFlows
        -- | Function to get a configuration by searching using Oracle
        :: ([Flow] -> C.Configuration)
        -- | Unconfigured PRG + helpers
        -> (PG.PGraph,Sem.Helpers)
        -- | Name of llvm-helpers, needed for implementing the graph
        -> String
        -- | function to implement given PRG conf
        -> (PLI.StateHandle -> C.Configuration -> IO ())
        -- | function to Group the LPG nodes into pipelines
        -> (StackState -> String -> PG.PGNode -> String)
        -> IO ()
instantiateFlows getConf prgH llvmH cfgImpl cfgPLA = do
    let prgArgs = StackPrgArgs { stPrgH = prgH
                               , stLlvmH = llvmH
                               , stCfgImpl = cfgImpl }
    instantiateFlows_ getConf cfgPLA prgArgs


-- instantiateFlows function that uses the new interface:
-- TODO: remove this and have the callers use the new interface
-- instantiate a stack using Dragonet.Optimization
instantiateOpt :: (Ord a, Show a) =>
        (PG.PGraph,Sem.Helpers)                     -- | Unconf PRG + helpers
        -> String                                      -- | Name of llvm-helpers
        -> (StackState -> [EndpointDesc] -> O.CostFunction a) -- | Cost Function
        -> (StackState -> [(String,C.Configuration)]) -- | Oracle
        -> (PLI.StateHandle -> C.Configuration -> IO ()) -- | Implement PRG conf
        -> (StackState -> String -> PG.PGNode -> String) -- | Assign nodes to PL
        -> IO ()
instantiateOpt prgH llvmH costFun cfgOracle cfgImpl cfgPLA = do
    args0 <- stackArgsDefault
    let prgArgs = StackPrgArgs { stPrgH = prgH
                               , stLlvmH = llvmH
                               , stCfgImpl = cfgImpl }
    let args = initStackArgs $ args0 { stPrg     = prgArgs
                                     , stCfgPLA  = cfgPLA }
    let updFn = updateGraphOpt costFun cfgOracle args
    instantiateStack args updFn


---
--- Incremental version
---

-- incremental state (includes search state to pass back and forth)
data StackIncrSt s = StackIncrSt {
      siSearchSt      :: s
    , siUpdatingGraph :: Bool
}

siInitialize :: s -> IO (STM.TVar (StackIncrSt s))
siInitialize st0 = do
    STM.atomically $ STM.newTVar $ StackIncrSt {
          siSearchSt = st0
        , siUpdatingGraph = False
    }

-- So the graph may be updated in the event handlers (e.g., registering a flow).
-- This is probably why the STM is used, but I'm not sure if it's actually
-- needed.
--
-- In any case, the new state needs to include the new search state which is
-- computed after the search. I'm wary of running the search inside the
-- transaction, so instead I split the whole thing in to two parts and do a
-- sanity check that nothing happened in between. If something tries to update
-- the graph when the graph is updating we bail out.
--
--  Note: event handlers can run as the search is ongoing and they get to update
--  the new state of flows, but cannot update the graph. For a more complete
--  implentation we can have another flag that something triggered a graph
--  update but doing control flow for this thing might get tricky.

doUpdateGraphPrepare si ss =
    let flowsSt  = ssFlowsSt ss
        flowsSt' = FL.fsReset flowsSt
        ss' = ss { ssVersion = ssVersion ss + 1, ssFlowsSt = flowsSt'}
        si' = si {siUpdatingGraph = True}
    in (si', ss', flowsSt)

siUpdateGraphPrepare sitv sstv = STM.atomically $ do
    si <- STM.readTVar sitv
    ss <- STM.readTVar sstv
    let mret = case siUpdatingGraph si of
                 True  -> Nothing -- the graph is already updating!
                 False -> Just $ doUpdateGraphPrepare si ss
    case mret of
        Nothing -> return Nothing
        Just (si', ss', flowsSt) ->
            do STM.writeTVar sitv si'
               STM.writeTVar sstv ss'
               return $ Just (si', ss', flowsSt)

siUpdateGraphDone sitv searchSt = STM.atomically $ do
    si <- STM.readTVar sitv
    let si' = case siUpdatingGraph si of
                    True -> si { siUpdatingGraph = False
                                , siSearchSt = searchSt }
                    False -> error "siUpdateGraphDone: this is not supposed to happen!"
    STM.writeTVar sitv si'
    return si'

-- This is an incremental version of upateGraphFlows in that it allows passing
-- state to and from the search function
updateGraphFlowsIncr
    :: (s -> FL.FlowsSt -> IO (C.Configuration, s))
    -> StackArgs
    -> STM.TVar (StackIncrSt s)
    -> STM.TVar StackState
    -> IO ()
updateGraphFlowsIncr doSearch args sitv sstv = do
   prep <- siUpdateGraphPrepare sitv sstv
   case prep of
     Nothing -> do
         putStrLn "updateGraphFlowsIncr called when update in progress, baling out"
         return ()
     Just (si', ss', flowsSt) -> do
        let searchSt = siSearchSt si'
            xforms = [IT.balanceAcrossRxQs, IT.coupleTxSockets]
            lbl = "updateGraphFlowsIncr"
        (prgConf, searchSt') <- doSearch searchSt flowsSt
        si'' <- siUpdateGraphDone sitv searchSt'
        -- implement new configuration:
        --- create a new graph
        plg <- ssMakeGraph ss' args prgConf lbl xforms
        --- apply PRG configuration
        (stCfgImpl $ stPrg args) (ssSharedState ss') prgConf
        --- run it
        PLD.run (ssCtx ss') plConnect (ssCreatePL args ss' plg) $ addMuxIds plg
        --
        putStrLnDbgN "updateGraphFlowsIncr exit"
        -- Create file here.
        putStrLnDbgN "################### calling appendFile with app ready notice"
        appendFile("allAppslist.appready") $ "Application is ready!\n"
        return ()

instantiateIncrFlowsIO_
    :: (s -> FL.FlowsSt -> IO (C.Configuration, s)) -- search function
    -> s -- initial search state
    -> (StackState -> String -> PG.PGNode -> String)
    -> StackPrgArgs
    -> IO ()
instantiateIncrFlowsIO_ incSearchIO si0 cfgPLA prgArgs = do
    args0 <- stackArgsDefault
    sitv <- siInitialize si0
    let args = initStackArgs $ args0 { stPrg    = prgArgs
                                     , stCfgPLA = cfgPLA }
        updFn = updateGraphFlowsIncr incSearchIO args sitv
    instantiateStack args updFn
