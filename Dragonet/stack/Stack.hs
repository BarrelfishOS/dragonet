module Stack (
    instantiateOpt,
    instantiateFlows,
    startStack,

    StackState(..),
    EndpointDesc(..),
    AppDesc(..),
    OracleArgs(..)
) where

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Dynamic as PLD
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as PLA
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem

import Dragonet.Endpoint (SocketId,AppId,IPv4Addr,UDPPort,EndpointDesc(..),)
import Dragonet.Flows  (Flow(..))

import Graphs.Cfg (e10kCfgEmpty)

import qualified Data.Maybe as MB

import qualified Data.Graph.Inductive as DGI
import Control.Applicative ((<$>))
import Util.GraphHelpers (findNodeByL)
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap
import qualified Graphs.LPG as LPG
import qualified Graphs.ImplTransforms as IT

import Runner.Dynamic (createPipeline, createPipelineClient)

import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import qualified Data.Map as M
import qualified Data.Set as S

import Util.XTimeIt (doTimeIt,dontTimeIt)
import Text.Show.Pretty (ppShow)

type EndpointId = Int

data AppDesc = AppDesc {
    adLabel :: String,
    adGraphHandle :: PLI.GraphHandle
}

-- For endpoints, we use local/remote
-- Should we use the same for flows (instead of Rx/Tx)?
-- We typically use flows on the Rx side, so the mapping is:
--  local:  dst
--  remote: src
epToFlow EndpointUDPv4 {epLocalIp = lIp,
                          epLocalPort = lPort,
                          epRemoteIp = rIp,
                          epRemotePort = rPort }
   = FlowUDPv4 {
          flSrcIp = rIp,
          flDstIp = lIp,
          flDstPort  = lPort,
          flSrcPort  = rPort }

data StackState = StackState {
    ssNextAppId :: AppId,  -- This ID will be given to the next app that tries to connect
    ssNextSocketId :: SocketId,
    ssNextEndpointId :: EndpointId,
    ssApplications :: M.Map AppId AppDesc,
    ssAppChans :: M.Map PLA.ChanHandle AppId,
    ssEndpoints :: M.Map EndpointId EndpointDesc,
    -- A mapping from socketId to EndpointId:
    --  used in spanning
    ssSockets :: M.Map SocketId EndpointId,
    -- quick hack to maintain the previous endpoints so that we can find the
    -- difference between the current and the old state. It is probably better
    -- to actually track the changes from the event handlers though.
    ssPrevEndpoints :: M.Map EndpointId EndpointDesc,
    ssUpdateGraphs :: STM.TVar StackState -> IO (),
    ssVersion :: Int
}


-- Oracle related
data OracleArgs =  OracleArgs {
        oracleOldConf           :: (String,C.Configuration)
        , oraclePrg             :: PG.PGraph
        , oracleNewConns        :: [EndpointDesc]
        , oracleSS              :: StackState
    } deriving ()

initOracleArgs :: PG.PGraph -> [EndpointDesc] -> StackState -> OracleArgs
initOracleArgs prg endps ss = OracleArgs  {
    oracleOldConf = getEmptyConf,
    oraclePrg  = prg,
    oracleNewConns  = endps,
    oracleSS = ss
}

-- Force socket nodes in their respective pipeline, for the rest use the
-- function f
plAssign f ss cfg m@(_,n)
    | Just said <- PGU.getPGNAttr n "appid" = "App" ++ said
    | otherwise = f ss cfg m


{-|
 - Creates input and output nodes to connect two pipelines
 -}
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
 - The supported event-types are appConnected, AppRegister, UDPListen,
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
        let aid = ssNextAppId ss
            app = AppDesc {
                    adLabel = "",
                    adGraphHandle = gh
                }
        STM.writeTVar sstv $ ss {
                    ssNextAppId = aid + 1,
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
 -  Handing event SocketUDPListen:
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
eventHandler sstv ch (PLA.EvSocketUDPListen (ip,port)) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            sid = ssNextSocketId ss
            eid = ssNextEndpointId ss
            ep = EndpointUDPv4 {
                    epSockets = [(sid,aid)],
                    epLocalIp = if ip == 0 then Nothing else Just ip,
                    epLocalPort = if port == 0 then Nothing else Just port,
                    epRemoteIp = Nothing,
                    epRemotePort = Nothing
                }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssNextEndpointId = eid + 1,
                ssSockets = M.insert sid eid $ ssSockets ss,
                ssEndpoints = M.insert eid ep $ ssEndpoints ss
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPListen p=" ++ show (ip,port) ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv
{-|
 -  Handing event SocketUDPFlow:
 -      This is prettymuch same as above execution (eventHandler UDPListen),
 -      except that it does not force having Nothing for remote IP and port
 -      TODO: This function can be written as specialized case of above.
 -}
eventHandler sstv ch (PLA.EvSocketUDPFlow (lIp,lPort) (rIp,rPort)) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            sid = ssNextSocketId ss
            eid = ssNextEndpointId ss
            ep = EndpointUDPv4 {
                    epSockets = [(sid,aid)],
                    epLocalIp =if lIp == 0 then Nothing else Just lIp,
                    epRemoteIp = if rIp == 0 then Nothing else Just rIp,
                    epLocalPort = if lPort == 0 then Nothing else Just lPort,
                    epRemotePort = if rPort == 0 then Nothing else Just rPort
                }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssNextEndpointId = eid + 1,
                ssSockets = M.insert sid eid $ ssSockets ss,
                ssEndpoints = M.insert eid ep $ ssEndpoints ss
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPFlow f=" ++ show (rIp,rPort) ++ "/"
                ++ show (lIp,lPort) ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
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
            -- get endpointID from old socket
            Just eid = M.lookup oldsid $ ssSockets ss
            sid = ssNextSocketId ss
            Just ep = M.lookup eid $ ssEndpoints ss
            -- add a new socket with appID and old endpointID to the list
            ep' = ep { epSockets = (sid,aid):(epSockets ep)}
            -- Update the stackState with:
            ss' = ss {
                ssNextSocketId = sid + 1, -- incremented SID
                ssEndpoints = M.insert eid ep' $ ssEndpoints ss -- updated endpoints
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketSpan existing=" ++ show oldsid ++ " -> " ++ show sid
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


-- generates the necessary LPG configuration based on the stack state
-- (specifically on the state's endpoints and sockets)
lpgConfigSS :: StackState -> C.Configuration
lpgConfigSS ss = LPG.lpgConfig allEps
    where
        allEps = M.elems $ ssEndpoints ss


-- instantiate a stack using Dragonet.Optimization
instantiateOpt :: (Ord a, Show a) =>
           (PG.PGraph,Sem.Helpers)                     -- | Unconf PRG + helpers
        -> String                                      -- | Name of llvm-helpers
        -> (StackState -> [EndpointDesc] -> O.CostFunction a) -- | Cost Function
        -> (OracleArgs -> [(String,C.Configuration)]) -- | Oracle
        -> (PLI.StateHandle -> C.Configuration -> IO ()) -- | Implement PRG conf
        -> (StackState -> String -> PG.PGNode -> String) -- | Assign nodes to PL
        -> IO ()
instantiateOpt (prgU,prgHelp) llvmH costFun cfgOracle cfgImpl cfgPLA = do
    -- Prepare graphs and so on
    (lpgU,lpgHelp) <- LPG.graphH
    let helpers = prgHelp `Sem.mergeHelpers` lpgHelp
        stackname = "dragonet"
    ctx <- PLD.initialContext stackname
    stackhandle <- PLD.ctxState ctx
    sharedState <- PLI.stackState stackhandle

    {-| Function 'updateGraph' adapts the graph to current stack state.  It
     -   * generates all the endpoints from current stack-state,
     -   * generates an LPG based on the stack-state
     -   * Finds out best PRG configuration for current state by using Oracle
     -   * Decides on transformations to be applied on the graph before
     -          implementing it
     -   * STEP: Generate an Optimize combined graph,
     -            based on the above PRG, LPG and transformations
     -   * STEP: Applies the configuration changes
     -   * STEP: Runs the pipelines
     -
     -}
    let updateGraphT x = doTimeIt "updateGraph"  $ updateGraph x
        updateGraph sstv = do
            putStrLn "updateGraph entry"
            ss <- ssNewVer sstv

            -- get list of all endpoints from stack-state
            let allEps = M.elems $ ssEndpoints ss
                lpgCfg = LPG.lpgConfig allEps

                -- Configure LPG with all these endpoints
                lpgC = C.applyConfig lpgCfg lpgU
                -- creating an LPG dotfile for debugging
                dbg = O.dbgDotfiles $ "out/graphs-tap/" ++ (show $ ssVersion ss)
                --dbg = O.dbgDummy

                -- Generate PRG configurations using ORACLE based on
                --          current LPG and stackState
                pCfgs = cfgOracle $ initOracleArgs lpgC allEps ss

                -- Transformations to be applied to graph before implementing it
                implTransforms = [IT.coupleTxSockets, IT.mergeSockets]

            -- LPG config is essentially all flows in network stack,
            --      so, showing current state here for debugging
            putStrLn $ "LPG config: " ++ show lpgCfg

            -- STEP: Generate an Optimize combined graph,
            --      based on the above PRG, LPG and transformations
            (plg,(_,pCfg), prgpc) <- O.optimize
                    helpers                    -- ^ Semantics helpers combined
                    prgU                       -- ^ Unconfigured PRG
                    lpgC                       -- ^ Configured LPG
                    implTransforms             -- ^ Implementation transforms
                    (plAssign cfgPLA ss)       -- ^ Assign nodes to pipelines
                    dbg                        -- ^ Debugging function
                    (costFun ss allEps)        -- ^ Cost function
                    pCfgs                      -- ^ Configurations to evaluate

            putStrLn $ "Optimization done!"

            -- STEP: apply PRG confuguration
            --      This is the step where actually filters will be inserted
            --      in the NIC
            cfgImpl sharedState pCfg

            -- Function to Create pipelines
            let createPL pl@('A':'p':'p':aids) = do
                    -- Creating application pipeline.  Untrusted zone
                    --      in application address-space
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    -- Creating stack-pipeline. Trusted zone
                    --      in dragonet address-space
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    createPipeline plg stackname llvmH pl

            -- STEP: Run pipelines
            PLD.run ctx plConnect createPL $ addMuxIds plg

            putStrLn "updateGraph exit"
            -- FIXME: Create file here.
            putStrLn "################### calling appendFile with app ready notice"
            appendFile("allAppslist.appready") $ "Application is ready!\n"

    putStrLn "Let's fire her up!"
    sstv <- ssInit updateGraph
    -- Stack is initialized, now lets update the state for first time manually
    updateGraph sstv

    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)


--

initStackSt = StackState {
    ssNextAppId = 1,
    ssNextSocketId = 1,
    ssNextEndpointId = 1,
    ssApplications = M.empty,
    ssAppChans = M.empty,
    ssSockets = M.empty,
    ssEndpoints = M.empty,
    ssPrevEndpoints = M.empty,
    ssUpdateGraphs = undefined,
    ssVersion = 0
}

-- TODO: use this in instantiate
-- bump the version number
ssNewVer :: STM.TVar StackState -> IO (StackState)
ssNewVer sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let ss' = ss { ssVersion = ssVersion ss + 1 }
    STM.writeTVar sstv ss'
    return ss'

{-|
 - Initialize the stackState, with specific function (eg: updateGraph)
 - for updating the graphs
 -}
ssInit ::
    -- | Function to update the graph
        (STM.TVar StackState -> IO ())
    -- | Updated stack-state and IO actions
    ->  IO (STM.TVar StackState)
ssInit updateGraph =
    STM.atomically $ STM.newTVar $ initStackSt {ssUpdateGraphs = updateGraph}

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

-- config LPG based on stack state information
ssConfigLPG :: StackState -> PG.PGraph -> PG.PGraph
ssConfigLPG ss lpgU = C.applyConfig (lpgConfigSS ss) lpgU

-- a simpler version of instantiate:
--  - assumes a configured PRG and does not call optimize
--  - PRG does not change across execution
startStack :: (PG.PGraph, Sem.Helpers) -- | unconfigured LPG
           -> (PG.PGraph, Sem.Helpers) -- | configured PRG
           -> (PG.PGraph -> PG.PGraph -> PG.PGraph) -- | embedding function
           -> String -- | name of llvm-helpers
           -> (StackState -> String -> PG.PGNode -> String) -- | PL assignment
           -> IO ()
startStack (lpgU, lpgH) (prgC, prgH) embed_fn llvmH cfgPLA = do
    let mergeH = prgH `Sem.mergeHelpers` lpgH
        stackname = "dragonet"
    ctx <- PLD.initialContext stackname
    stackH <- PLD.ctxState ctx
    sharedSt <- PLI.stackState stackH

    let updateGraph sstv = do
            putStrLn "updateGraph entry"
            ss <- ssNewVer sstv
            let lpgC = ssConfigLPG ss lpgU
                dbg ::O.DbgFunction ()
                dbg = O.dbgDotfiles $ "out/graphs-dummy/" ++ (show $ ssVersion ss)
                debug = dbg "dummy"
                implTransforms = [IT.mergeSockets]
                lbl = "dummy"
                pla = (plAssign cfgPLA ss)

            --debug "prg" $ O.DbgPGraph prgC
            plg <- O.makeGraph' mergeH prgC lpgC embed_fn implTransforms (pla lbl) debug
            error "DONE!"

            let createPL pl@('A':'p':'p':aids) = do
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    createPipeline plg stackname llvmH pl
            PLD.run ctx plConnect createPL $ addMuxIds plg
            putStrLn "updateGraph exit"

    putStrLn "Let's fire her up!!"
    sstv <- ssInit updateGraph
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)



getEmptyConf = ("EmptyCOnf",
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList [])
                    ]
                )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


epsDiff :: [EndpointDesc] -> [EndpointDesc] -> [EndpointDesc]
epsDiff es1 es2 = S.toList $ S.difference (S.fromList es1) (S.fromList es2)

{-|
 - The function 'instantiateFlows' is initialization function which
 -   * Sets up all the event handling mechanisms in place:
 -      + Done by 'updateGraph' function defined in the function-body.
 -   * Creates initial minimal stack which will handle the default graph,
 -      by calling 'updateGraph' function
 -   * Then goes on **infinite event dispatch** loop handling
 -          request events from applications
 -}
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
instantiateFlows getConf (prgU,prgHelp) llvmH cfgImpl cfgPLA = do
    -- Prepare graphs and so on
    (lpgU, lpgHelp) <- LPG.graphH
    -- Merge LPG and PRG helpers
    let mergeH = prgHelp `Sem.mergeHelpers` lpgHelp
        stackname = "dragonet"

    ctx <- PLD.initialContext stackname
    stackhandle <- PLD.ctxState ctx
    sharedState <- PLI.stackState stackhandle

    {-|
     - Function 'updateGraph' adapts the graph to current stack state.  It
     -   * generates all the endpoints from current stack-state,
     -   * generates an LPG based on the stack-state
     -   * STEP: Finds out best PRG configuration for current state
     -          by using given Oracle
     -   * Decides on transformations to be applied on the graph before
     -          implementing it
     -   * STEP: Generate a combined pipelined-graph,
     -            based on the above PRG, LPG and transformations
     -   * STEP: Applies the configuration changes
     -   * STEP: Runs the pipelines
     -
     -  This function will be called in following situations
     -      * To manually kickoff start of the stack (only once, in this function)
     -      * App event UDPListen
     -      * App event UDPFlow
     -      * App event SpanSocket
     -}
    let updateGraph sstv = do
            putStrLn "instantiateFlows::updateGraph"

            -- TODO: Understand what is this?
            (ss, prevEpsM) <- ssExecUpd sstv

            -- get list of all endpoints from stack-state
            let allEps = M.elems $ ssEndpoints ss
                --prevEps = M.elems prevEpsM
                --newEps = epsDiff allEps prevEps
                --rmEps = epsDiff prevEps allEps

                -- LPG config is essentially all flows in network stack
                lpgCfg = LPG.lpgConfig allEps
                -- Configure LPG
                lpgC = C.applyConfig lpgCfg lpgU

                -- creating an LPG dotfile for debugging
                lbl = "instSearch"
                debug :: O.DbgFunction ()
                --dbg = O.dbgDummy
                debug = O.dbgDotfiles $ "out/graphs-xxx/" ++ (show $ ssVersion ss)
                dbg = debug lbl

                -- Transformations to be applied to graph before implementing it
                implTransforms = [IT.coupleTxSockets, IT.mergeSockets]
                pla = (plAssign cfgPLA ss)
                --putStrLn $ "=====> REMOVED: " ++ (ppShow rmEps)
                --putStrLn $ "=====> ADDED: " ++ (ppShow newEps)
                --putStrLn $ "LPG config: " ++ show lpgCfg

             -- Generate PRG configurations using ORACLE based on current LPG
             --         and stackState
            let prgConf = getConf $ map epToFlow allEps

            -- STEP: Create a new combined, but pipelined graph
            --          with both LPG and PRG with configuration applied
            plg <- O.makeGraph mergeH prgU lpgC implTransforms (pla lbl) dbg prgConf

            -- STEP: apply PRG confuguration
            -- NOTE: This is the step where actually filters will be inserted
            --      in the NIC
            cfgImpl sharedState prgConf

            --  Function: depending on userspace/driverspace pipeline
            --       create pipeline-threads
            --  QUESTION: Do these pipelines are getting created everytime
            --      updateGraph is being called?  They should happen only
            --      in begining, and whenever new application arrives.
            let createPL pl@('A':'p':'p':aids) = do
                    -- If it starts with word "App", then assuming application
                    -- Creating application pipeline.  Untrusted zone
                    --      in application address-space
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    -- Creating stack-pipeline. Trusted zone
                    --      in dragonet address-space
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    -- Creates a thread to handle the pipeline
                    --      NOTE: following function will actually create a thread
                    createPipeline plg stackname llvmH pl

            -- STEP: Run each pipeline
            --   * Create pipeline in separate thread if it don't exist
            --   * If pipeline exist then
            --       + stop it
            --       + Update the in/out connecting queues based on changes
            -- NOTE: ctx is still an initial context, is that efficient?
            PLD.run ctx plConnect createPL $ addMuxIds plg

            putStrLn "updateGraph exit"
            -- FIXME: Create file here.
            putStrLn "################### calling appendFile with app ready notice"
            appendFile("allAppslist.appready") $ "Application is ready!\n"

        -- | Wrapper for updateGraph which will time the execution of function
        updateGraphT x = doTimeIt "instantiateFlows::updateGraph"  $ updateGraph x

    putStrLn "Let's fire her up!"
    -- Creating a stack-state with updateGraph as a function to update graph
    sstv <- ssInit updateGraph
    -- Stack is initialized, now lets update the state for first time manually
    updateGraph sstv

    -- Starting an **infinite event loop** handling events
    --      using event-handlers defined in sstv.
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)

