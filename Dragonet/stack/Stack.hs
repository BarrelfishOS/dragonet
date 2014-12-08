module Stack (
    instantiateOpt,
    instantiateFlows, instantiateFlows_,

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
import qualified Dragonet.Search                   as Srch
import qualified Dragonet.NetState                 as NS

import Dragonet.NetState (NetState, SocketId,AppId,EndpointId,EndpointDesc(..),epsDiff)
import Dragonet.Flows (Flow(..), epToFlow, flowStr)

import qualified Data.Graph.Inductive as DGI
import Util.GraphHelpers (findNodeByL)

import Control.Applicative ((<$>))

import qualified Graphs.LPG as LPG
import qualified Graphs.ImplTransforms as IT

import Runner.Dynamic (createPipeline, createPipelineClient)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M

import Util.XTimeIt (doTimeIt,dontTimeIt)
import Text.Show.Pretty (ppShow)

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
    -- quick hack to maintain the previous endpoints so that we can find the
    -- difference between the current and the old state. It is probably better
    -- to actually track the changes from the event handlers though.
    , ssPrevEndpoints  :: M.Map EndpointId EndpointDesc
    -- callback to update the graph
    , ssUpdateGraphs   :: STM.TVar StackState -> IO ()
    -- each stack state gets a new version number
    , ssVersion        :: Int
    -- explicit registered flows
    , ssFlows          :: [Flow]
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
        --dbg = O.dbgDummy
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

{-|
 -  Handing event AppConnected:
 -      * Assign current appID as application-identifier
 -      * Increase appID
 -      * Add channel into list of application channels
 -      * Insert applicationID in list of applications
 -}
eventHandler sstv ch (PLA.EvAppConnected gh) = do
    putStrLn $ "AppConnected: " ++ show ch
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
eventHandler sstv ch (PLA.EvAppRegister lbl) = do
    aid <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            Just app = M.lookup aid $ ssApplications ss
        STM.writeTVar sstv $ ss {
                ssApplications = M.insert aid app $ ssApplications ss
            }
        return aid
    putStrLn $ "AppRegister: " ++ lbl ++ "[" ++ show ch ++ "] -> " ++ show aid
    PLA.sendMessage ch $ PLA.MsgWelcome aid

{-|
 -  Handing event SocketUDPBind:
 -      * Lookup appID
 -      * Use nextSocketID for this socket
 -      * Use nextEndpointID for this endpoint
 -      * Increment both nextSocketID, and nextEndpointID for future use.
 -      * Create an endpoint-descriptor with listening IPAddr and port number
 -              in endpoint descriptor
 -      * Create a socket-descriptor with this applicationID, endpoint
 -      * Insert endpoint-descriptor, socket-descriptor into proper lists
 -      * Send back a message to application with socketInfo and sid
 -      * Call updateGraph which will
 -              + Find best configuration to run
 -              + Insert filters in NIC if necessary
 -              + Inform running pipelines about the change
 -}
eventHandler sstv ch (PLA.EvSocketUDPBind le@(lIp,lPort) re@(rIp,rPort)) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            ((sid,eid), ss') = ssRunNetState ss (NS.udpBind aid (le,re))
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPBind f=" ++ show (rIp,rPort) ++ "/"
                ++ show (lIp,lPort) ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv
{-|
 -  Handing event SocketUDPFlow:
 -      Registers a flow
 -}
eventHandler sstv ch (PLA.EvSocketUDPFlow sid le@(lIp,lPort) re@(rIp,rPort)) = do
    -- update ssFlows
    ss <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let oldFlows = ssFlows ss
            fl = epToFlow $ NS.mkUdpEndpoint le re
            ss' = ss { ssFlows = fl:oldFlows }
        STM.writeTVar sstv $ ss'
        return ss'
    putStrLn $ "SocketUDPFlow f=" ++ show (rIp,rPort) ++ "/"
                ++ show (lIp,lPort)
                ++ " for " ++ show sid
    PLA.sendMessage ch $ PLA.MsgStatus True
    ssUpdateGraphs ss sstv

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
eventHandler sstv ch (PLA.EvSocketSpan oldsid) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            (sid,ss') = ssRunNetState ss (NS.socketSpan aid oldsid)
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketSpan existing=" ++ show oldsid ++ "for sid:" ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv
{-|
 -  Handling event "all other types":
 -      This is pretty much an error case.  Currently we are only printing
 -      the event Name and egnoring it.
 -  TODO: Should I report error to an application here?
 -}
eventHandler sstv ch ev = do
    putStrLn $ "eventHandler " ++ show ch ++ " " ++ show ev


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
    ssPrevEndpoints  = M.empty,
    ssVersion        = 0,
    ssFlows          = []
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

-- Update:
--  bump the version number
--  update the prevEndpoints
--  return : new stack state, endpoints added, endpoints removed
ssExecUpd :: STM.TVar StackState -> IO (StackState, M.Map EndpointId EndpointDesc)
ssExecUpd sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let prevEps = ssPrevEndpoints ss
        newEps  = ssEndpoints ss
        ss' = ss { ssVersion = ssVersion ss + 1, ssPrevEndpoints = newEps }
    STM.writeTVar sstv ss'
    return (ss', prevEps)

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
    PLA.interfaceThread (stName args) (eventHandler sstv)

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
    putStrLn $ "Creating App pipeline: " ++ pl
    createPipelineClient agh pl
    where
      aid = read aids
      Just app = M.lookup aid $ ssApplications ss
      agh = adGraphHandle app
ssCreatePL args ss plg pl = do
    -- Creating stack-pipeline. Trusted zone
    --      in dragonet address-space
    putStrLn $ "Creating local pipeline: ##### " ++ pl
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
    putStrLn "updateGraphOpt"
    ss <- ssNewVer sstv
    let lpgC = ssConfigureLpg ss args

    -- STEP: Generate an Optimize combined graph,
    --    based on the above PRG, LPG and transformations
    (plg,(_,pCfg),prgpc) <- ssOptimize ss args costFn cfgOracle

     -- STEP: apply PRG configuration
     --    This is the step where actually filters will be inserted in the NIC
    (stCfgImpl $ stPrg args) (ssSharedState ss) pCfg

    -- STEP: Run pipelines
    PLD.run (ssCtx ss) plConnect (ssCreatePL args ss plg) $ addMuxIds plg

    putStrLn "updateGraph exit"
    -- FIXME: Create file here.
    putStrLn "################### calling appendFile with app ready notice"
    appendFile("allAppslist.appready") $ "Application is ready!\n"


-- updateGraph function that uses Dragonet.Search functions
updateGraphFlows
    :: ([Flow] -> IO C.Configuration)
    -> StackArgs
    -> STM.TVar StackState
    -> IO ()
updateGraphFlows getConf args sstv = do
   putStrLn "updateGraphFlows"
   (ss, prevEpsM) <- ssExecUpd sstv
   -- get list of all endpoints from stack-state
   let
       lbl = "updateGraphFlows"
       --prevEps = M.elems prevEpsM
       --newEps = epsDiff allEps prevEps
       --rmEps = epsDiff prevEps allEps
       --putStrLn $ "=====> REMOVED: " ++ (ppShow rmEps)
       --putStrLn $ "=====> ADDED: " ++ (ppShow newEps)

       -- OLD implenetation: where flows are taken from endpoints
       --xforms = [IT.coupleTxSockets, IT.mergeSockets]
       --flows = map epToFlow $ M.elems $ ssEndpoints ss
       -- NEW implementations: flows are registered externally
       --  NB: because there are no socket endpoints now, we use
       --  balanceAcrossRxQs to distribute spanned sockets across pipelines
       xforms = [IT.balanceAcrossRxQs, IT.coupleTxSockets]
       flows = ssFlows ss


   putStrLn $ "Flows:\n" ++ (ppShow $ map flowStr flows)
   prgConf <- getConf flows

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

   putStrLn "updateGraph exit"
   -- FIXME: Create file here.
   putStrLn "################### calling appendFile with app ready notice"
   appendFile("allAppslist.appready") $ "Application is ready!\n"

-- OLD/Deprecated interface

instantiateSearchIO :: (Srch.OracleSt o a)
                    => Srch.SearchParams o a -> StackArgs -> IO ()
instantiateSearchIO params args = do
    -- initialize search state
    searchSt <- Srch.initSearchIO params
    let getConfIO :: [Flow] -> IO (C.Configuration)
        getConfIO = Srch.runSearchIO searchSt
        updFn = updateGraphFlows getConfIO args
    instantiateStack args updFn


instantiateFlows_ ::
        -- | Function to get a configuration by searching using Oracle
           ([Flow] -> C.Configuration)
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
instantiateFlows ::
        -- | Function to get a configuration by searching using Oracle
           ([Flow] -> C.Configuration)
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
