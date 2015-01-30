module Graphs.ImplTransforms (
    mergeSockets,
    coupleTxSockets,
    balanceAcrossRxQs,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Graph as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Map as M
import qualified Data.List as DL
import qualified Data.Set as S
import Data.Word (Word64)
import Data.Functor ((<$>))
import Data.Maybe

import Debug.Trace (trace)
traceN a b = b


--balanceAcrossRxQs :: PG.PGraph -> PG.PGraph
--balanceAcrossRxQs =  balanceAcrossRxQsDummy
balanceAcrossRxQs = balanceAcrossRxQsReal

balanceAcrossRxQsDummy :: PG.PGraph -> PG.PGraph
balanceAcrossRxQsDummy g = g

-- At this point (i.e., when transformation functions are called), only the
-- valid RxQueues remain (i.e., the ones that will receive packets). Each of
-- this queue might contain balance nodes that steers packets from the same
-- endpoint to different sockets. Note that these balance nodes (and their
-- sockets) are duplicated across all H/W queues.
--
-- This transformation assigns a single socket of the endpoint balance nodes to
-- each pipeline. Hence, it statically balances endpoints across different
-- pipelines using a socket per pipeline.
--
-- For this to make more sense in the general case, we might want to do that
-- *only* for sockets with the same AppId.
balanceAcrossRxQsReal :: PG.PGraph -> PG.PGraph
balanceAcrossRxQsReal g = foldl balEpAcrossRxQs g (M.toList balEpsMap)
    where
          -- List of all balance nodes with their endpont-ID's
          balEps :: [(Integer, DGI.Node)]
          --balEps :: [(Integer, (DGI.Node, PG.NLabel))]
          --balEps = [ (eid, (nid, (PG.nLabel nlbl))) |
          balEps = [ (eid, nid) |
                     (nid,nlbl) <- DGI.labNodes g,
                     let eid_ = PGU.balanceNodeEndpointId nlbl,
                     isJust eid_,
                     let Just eid = eid_]
          -- map from endpoint-id to balance nodes
          --    given endpoint can endup in multiple partitions, having
          --    multiple balance nodes (one in each reachable partition).
          --    Here, we are collecting all balance nodes associated with
          --        same endpoint

          -- balEpsMap :: M.Map Integer [(DGI.Node, PG.NLabel)]
          balEpsMap :: M.Map Integer [DGI.Node]
          balEpsMap = foldl foldFn M.empty balEps
          foldFn m (eid,nid) = M.alter alterF eid m
          --foldFn m (eid, (nid, nlbl)) = M.alter alterF eid m
            where alterF Nothing        = Just $ [nid]
                  alterF (Just oldnids) = Just $ nid:oldnids


{-|
 - Balance the given endpoint into all the sockets spanned with this endpoint
 -  An endpoint can have multiple balance nodes (one in each reachable partition).
 -  So, we try and distribute the reachable partitions evenly between related
 -   sockets.
 -}
balEpAcrossRxQs ::
    -- | Initial graph to be balanced
       PG.PGraph
    -- | (endpoint-id, list of balance nodes associated with the endpoint)
    -> (Integer, [DGI.Node])
    -- -> (Integer, [(DGI.Node, PG.NLabel)])
    -- | Graph after applying the balancing
    -> PG.PGraph
--balEpAcrossRxQs g (eid,nids@((nid0,lab0):_)) = traceN msg $ enfoceEP2SocketsMapping g groupedNids
balEpAcrossRxQs g (eid,nidss@((nid0):_)) =
    --traceN msg $ enfoceEP2SocketsMapping g ([] ++ groupedNids)
    traceN msg $ enfoceEP2SocketsMapping g ([defaultQMap] ++ groupedNids)
    where
    ports' = PG.nPorts $ fromJust $ DGI.lab g nid0

    {- Reversing to get deterministic ordering so that q0 will always go to socket-0, etc
     -  I don't know if this ordering is assured or not, and most probably this will break
     -  if embedding or node insertion code changes the way these nodes are inserted
     -  Cleaner fix will be to use the associated queue/partition attribute
     -  for sorting and making sure that sockets are deterministically used
     -}

    nidss' = reverse nidss

    portForQ0 = head $ reverse ports'
    ports = reverse $ tail $ reverse ports'

    partitionForQ0 = head nidss'
    nids = drop 1 nidss'

    balancedPorts = balancedChunks (length nids) ports
    groupedNids :: [(DGI.Node, [PG.NPort])]

    -- Doing explicit mapping for queue-0 to socket-0
    defaultQMap :: (DGI.Node, [PG.NPort])
    --defaultQMap = ((head nidss'), [(head ports')])
    defaultQMap = (partitionForQ0, [portForQ0])

    balancedPorts' = DL.sort balancedPorts
    groupedNids = zip nids $ concat $ repeat balancedPorts'
    msg
            | length nids == length ports = "############################### one pipeline/queue per socket"
            | length nids > length ports = "############################### Multiple pipelines/queues per socket"
            | otherwise = -- TODO: Add special balance nodes to allow
                          -- single pipeline/queue to loadbalance to between multiple sockets
                        ("############################### Warning: More sockets than pipelines/queues, ignoring few extra sockets ports=[["
                        ++ show ports ++ ",  nids=[[" ++ show nids ++ "]]")

myIsNotEmpty [] = False
myIsNotEmpty _ = True

-- | Balance the given list l into balanced n sublists
balancedChunks :: Int -> [a] -> [[a]]
balancedChunks n l = emptyRemoved
    where
    validGroupIDs = [0..(n - 1)]
    indexedList = zip l [0..]
    -- Group/hash the list into sublist based on the position/index of the element
    grouped = map (
            \rem -> map fst $ (filter (\(_, idx) -> (mod idx n) == rem) indexedList)
        ) validGroupIDs
    -- There will be empty elements when (length l < n) so, lets remove them
    emptyRemoved = filter myIsNotEmpty grouped


-- | Given the mapping between pipeline and sockets, implement it by removing
--      the generic balance node, and connecting the pipeline directly to the
--      socket
enfoceEP2SocketsMapping ::
    -- | Initial graph on which mapping will be applied
       PG.PGraph
    -- | Mapping of pipeline/partition/queue to application sockets/output ports
    -> [(DGI.Node, [PG.NPort])]
    -- | Graph after applying the mapping on intial graph
    -> PG.PGraph
-- end: No mappings, returning initial graph as it is
enfoceEP2SocketsMapping g [] = g
-- end: No socket mapped to the endpoint: This is treated as error as all
--      the incoming packets here will end up getting dropped
--enfoceEP2SocketsMapping g (eps, []):xs = error "the endpoint does not have any socket connected"
enfoceEP2SocketsMapping g (x:xs) = traceN msg $ gAns
    where
    eps = fst x
    ports' = snd x
    ports = DL.sort ports'
    (p, msg)
        | ports == [] = error "the endpoint does not have any socket connected"
        | (length ports) == 1 = ((head ports), "")
        | otherwise = ((head ports),
            ("WARNING: pipeline mapped to multiple sockets is not supported, "
             ++ "ignorming all but first socket: "
             ++ " used {{{\n" ++ (show $ head ports) ++ "\n}}}"
             ++ " ignored {{{\n" ++ (show $ tail ports) ++ "\n}}}"
            )
           )
    g' = mapEPtoSinglePort g eps p
    gAns = enfoceEP2SocketsMapping g' xs

{-
-- case: pipeline/EP mapped to single socket.  Just drop the balance node and
--  connect the pipeline directly to the mapped single socket
enfoceEP2SocketsMapping g (eps, p:[]):xs = enfoceEP2SocketsMapping g' xs
    where
    g' = mapEPtoSinglePort g eps p
-- case: pipeline/EP mapped to list of sockets.
--      ideally, we should replace the generic balance node with specific
--      balancing node which will balance only between given sockets
--  Currently, We are using only first socket and ignoring all other sockets
enfoceEP2SocketsMapping g (eps, p:ps):xs = traceN msg gAns
    where
    g' = mapEPtoSinglePort g eps p
    gAns = enfoceEP2SocketsMapping g' xs
    msg = "WARNING: pipeline mapped to multiple sockets is not supported, "
             ++ "ignorming all but first socket: "
             ++ " used {{{\n" ++ (show p) ++ "\n}}}"
             ++ " ignored {{{\n" ++ (show ps) ++ "\n}}}"

-}

-- Maps single pipeline/EndPoint to single socket/port
--  It removes the balance node and connects directly to the given socket
mapEPtoSinglePort g epNid port =  g'
   where (prevNid,prevE) = case DGI.lpre g epNid of
             [x] -> x
             []  -> error "balEpAcrossRxQs: 0 predecessor"
             _   -> error "balEpAcrossRxQs: >1 predecessors"
         prevL = fromJust $ DGI.lab g prevNid
         (nextNid,nextE) = case PGU.lsucPortNE g epNid port of
            [x] -> x
            []  -> error $ "balEpAcrossRxQs: port " ++ (show port) ++ " 0 sucs"
            _   -> error $ "balEpAcrossRxQs: port " ++ (show port) ++ " >1 sucs"
         g' = traceN msg
              $ DGI.insEdge (prevNid, nextNid, prevE)
              $ DGI.delNodes delNs g

         delNs = [ nid | nid <- DFS.dfs [epNid] g, nid /= nextNid ]
         msg = ""


-- Remove fromSocket nodes without a matching toSocket node
--
-- In the Embedding phase, all LPG nodes are duplicated.
-- Hence, all socket nodes are replicated in each pipeline.
-- During cleanup, hardware configuration eliminates toSocket nodes based
-- on queue filters.
--
-- For each queue, each toSocket has a corresponding fromSocket node (they have
-- a label {to,from}socket=id with the same id). This function eliminates
-- fromSocket nodes without a matching toSocket node.
coupleTxSockets :: PG.PGraph -> PG.PGraph
coupleTxSockets pg = DGI.delNodes badFsn pg
    where
        -- set of (queueTag, socketId) constructed for all toSocket nodes
        tsn = S.fromList $ [(PG.nTag l,sid) |
                            (_,l) <- DGI.labNodes pg,
                            let msid = PGU.toSocketId l,
                            isJust msid,
                            let Just sid = msid]

        badFsn = [n | (n,l) <- DGI.labNodes pg,
                      let msid = PGU.fromSocketId l,
                      isJust msid,
                      let Just sid = msid,
                      (PG.nTag l,sid) `S.notMember` tsn]

        label n = (PG.nTag l,PG.nLabel l)
            where l = fromJust $ DGI.lab pg n
        l = map label


-- Merge socket nodes so that there is only one instance
mergeSockets :: PG.PGraph -> PG.PGraph
mergeSockets pg = pg'
    where (pg',_,_) = foldl mergeSockNode (pg,M.empty,M.empty) $ DGI.labNodes pg


type MSNCtx = (PG.PGraph,
               -- toSocket nodes map: sid -> Node
               M.Map Word64 DGI.Node,
               -- fromSocket nodes map: sid ->
               --   Left id  -> first occurence (id of first node)
               --   Right id -> second occrence (id of balancer node)
               M.Map Word64 (Either DGI.Node DGI.Node))

mergeSockNode :: MSNCtx -> (DGI.Node,PG.Node) -> MSNCtx
mergeSockNode (pg,mt,mf) (n,l)
    -- toSocket nodes (Rx)
    | Just sid <- read <$> PGU.toSocketId l =
        case M.lookup sid mt of
            -- First occurrence of this ToSocket node:
            --  insert it to the toSocket node map
            Nothing -> (pg, M.insert sid n mt, mf)
            -- Subsequent ToSocket nodes:
            --  have all its predeccessors point to the first occurence
            --  and remove the node
            --  AKK: don't we (in theory) need an OR node here?
            Just sn ->
                let newEs = [(m,sn,el) | (m,el) <- DGI.lpre pg n]
                    pg' = DGI.insEdges newEs $ DGI.delNode n pg
                in (pg', mt, mf)
    -- fromSocket nodes (Tx)
    | Just sid <- read <$> PGU.fromSocketId l,
      Just said <- PGU.getPGNAttr l "appid" =
        case M.lookup sid mf of
            -- First occurrence of this FromSocket node
            Nothing -> (pg, mt, M.insert sid (Left n) mf)
            -- Second occurrence of this FromSocket node: add balance node
            Just (Left sn) ->
                let [bn] = DGI.newNodes 1 pg in
                (DGI.insEdge (sn,bn,PG.Edge "out") $
                    DGI.delNode n $
                    fixFSEdges bn n $ fixFSEdges bn sn $
                    DGI.insNode (bn,balanceNode sid said) pg,
                 mt,
                 M.insert sid (Right bn) mf)
            -- Subsequent FromSocket nodes
            Just (Right bn) -> (DGI.delNode n $ fixFSEdges bn n pg, mt, mf)

    | otherwise = (pg,mt,mf)
    where
        balanceNode sid said =
            PG.nAttrAdd (PG.NAttrCustom $ "appid=" ++ said) $
            PGU.balanceNode ("TxSocket" ++ show sid) []
        -- Move all edges originating at "out" port of fsn to a new port on bn
        fixFSEdges bn fsn g =
            -- Remove existing edges
            GH.delLEdges [(fsn,dn,p) | (dn,p@(PG.Edge "out")) <- sucs] $
                -- Add outgoing edges to balance node on new port
                DGI.insEdges [(bn,dn,PG.Edge port) |
                              (dn,p@(PG.Edge "out")) <- sucs] $
                -- Add new port to balance node
                GH.updateN (\l -> l { PG.nPorts = PG.nPorts l ++ [port] }) bn g
            where
                sucs = DGI.lsuc g fsn
                Just bL = DGI.lab g bn
                port = show $ length $ PG.nPorts bL

