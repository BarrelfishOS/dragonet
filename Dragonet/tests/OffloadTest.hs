{-# LANGUAGE QuasiQuotes #-}

--import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NPort, Edge(..), NOperator(..))
import Dragonet.ProtocolGraph.Utils (getFNodeByName, getFNodeByName')
import Dragonet.DotGenerator (toDot)
import qualified Dragonet.Unicorn as U
import qualified Util.GraphHelpers as GH

import Control.Monad.State (State, MonadState, gets, modify, runState)
import Control.Applicative ((<$>))
import Data.Maybe
import Data.List (intercalate)
import Text.Show.Pretty (ppShow)

import Debug.Trace (trace)
import Text.RawString.QQ (r)

------------------------------------------------
-- helpers (to be eventually moved to other files)

-- node type tests
isONode :: PGNode -> Bool
isONode (_, ONode {}) = False
isONode _ = True

isFNode :: PGNode -> Bool
isFNode (_, FNode {}) = True
isFNode _ = False

isCNode :: PGNode -> Bool
isCNode (_, CNode {}) = True
isCNode _ = False

-- helper to easily define equality for PredicateExpr
data PTerm = PTerm PGNode NPort
instance Eq PTerm where
    (PTerm (_, n1) p1) == (PTerm (_, n2) p2) = (nLabel n1 == nLabel n2) && (p1 == p2)

data PredicateExpr = PredicateTerm PTerm |
                     PredicateOr  [PredicateExpr] |
                     PredicateAnd [PredicateExpr] |
                     PredicateNot PredicateExpr |
                     PredicateTrue | PredicateFalse |
                     PredicateUndef
    deriving (Eq)

instance Show PredicateExpr where
    show (PredicateTerm (PTerm (_, node) nport)) = "(" ++ (nLabel node) ++ "," ++ nport ++ ")"
    show (PredicateOr l)  = "( " ++ (intercalate " OR " $ map show l) ++ " ) "
    show (PredicateAnd l) = "( " ++ (intercalate " AND " $ map show l) ++ " ) "
    show (PredicateNot e) = "(NOT " ++ (show e) ++ ")"
    show PredicateTrue    = "True"
    show PredicateFalse   = "False"
    show PredicateUndef   = "UNDEFINED"

-- create an AND predicate, and do folding when possible
predicateAND :: [PredicateExpr] -> PredicateExpr
predicateAND [] = error "Empty predicate list"
predicateAND x  = let tr = if True then \a b -> b else trace
                      x'   = tr ("AND args: " ++ (show x)) x
                      ret  = doAND PredicateUndef x'
                      ret' = tr ("RESULT: " ++ (show ret)) ret
                   in ret'
    where doAND :: PredicateExpr -> [PredicateExpr] -> PredicateExpr
          doAND result           []                  = result
          doAND _                (PredicateFalse:xs) = PredicateFalse
          doAND PredicateUndef   (PredicateTrue:xs)  = doAND PredicateTrue xs
          doAND result           (PredicateTrue:xs)  = doAND result xs
          doAND PredicateUndef   (x:xs)              = doAND (PredicateAnd [x])   xs
          doAND PredicateTrue    (x:xs)              = doAND x  xs
          doAND (PredicateAnd r) (x:xs)              = doAND (PredicateAnd (x:r)) xs
          doAND r                l                   = error $ "This should not happen (AND)" ++ "result: --" ++ (show r) ++ "-- rest: --" ++ (show l) ++ "--"

-- create an OR predicate, and do folding when possible
predicateOR :: [PredicateExpr] -> PredicateExpr
predicateOR [] = error "Empty predicate list"
predicateOR x  = let tr = if True then \a b -> b else trace
                     x'   = tr ("OR args: " ++ (show x)) x
                     ret  = doOR PredicateUndef x'
                     ret' = tr ("RESULT: " ++ (show ret)) ret
                  in ret'
    where doOR :: PredicateExpr -> [PredicateExpr] -> PredicateExpr
          doOR result           []                  = result
          doOR _               (PredicateTrue:xs)   = PredicateTrue
          doOR PredicateUndef  (PredicateFalse:xs)  = doOR PredicateFalse xs
          doOR result          (PredicateFalse:xs)  = doOR result xs
          doOR PredicateUndef  (x:xs)               = doOR (PredicateOr [x]) xs
          doOR PredicateFalse  (x:xs)               = doOR x xs
          doOR (PredicateOr r) (x:xs)               = doOR (PredicateOr (x:r)) xs
          doOR r               l                    = error $ "This should not happen (OR)" ++ "result: --" ++ (show r) ++ "-- rest: --" ++ (show l) ++ "--"

-- potentially replace them with the versions above that do folding
predAnd = predicateAND
predOr  = predicateOR
--predAnd = PredicateAnd
--predOr  = PredicateOr

-- simple equality for now
predEquiv :: PredicateExpr -> PredicateExpr -> Bool
predEquiv a b =  a == b

-- state for computing predicate expression:
data PredCompSt = PredCompSt {
      predGraph  :: PGraph
    , predSrc    :: PGNode
    , predDst    :: PGNode
    , predInPort :: Maybe NPort
} deriving (Show)

-- initialize state
initPredCompSt_ = PredCompSt {
      predGraph  = undefined
    , predSrc    = undefined
    , predDst    = undefined
    , predInPort = Nothing
}

initPredCompSt :: PGraph -> String -> String -> PredCompSt
initPredCompSt gr src dst = initPredCompSt_ {
      predGraph = gr
    , predSrc   = getFNodeByName gr src
    , predDst   = getFNodeByName gr dst
}

pgEdgePort :: PGEdge -> NPort
pgEdgePort pedge = case pedge of
    (_, _, ESpawn _) -> error "cannot deal with spawn edges just yet"
    (_, _, Edge p)   -> p


-- Compute a predicate expression for flows (packets, whatever) that start from
--  predSrc and reach predDst, in predGraph
computePred :: PredCompSt -> PredicateExpr
-- compute predicate of src->dst, where dst is an FNode:
--  predicate of pre(dst) AND predicate of src->pre(dst)
computePred st@(PredCompSt { predDst = (_, FNode {}) } )
    -- we reached src: end recursion
    | same_node          = PredicateTrue
    -- we reched another (i.e., not src) entry node
    | npredecessors == 0 = error "Not sure what should happen now..." --PredicateFalse
    | npredecessors > 1  = error "F-nodes have at most one predecessor"
    -- recurse
    | otherwise          = expr
    where (src, dst, gr) = (predSrc st, predDst st, predGraph st)
          -- equality by DGI.Node (we assume nodes are in the same graph)
          same_node      = fst src == fst dst
          -- predecessors of destintation (going backwards)
          predecessors   = GH.labLPre gr dst
          npredecessors  = length predecessors
          (pnode, pedge) = predecessors !! 0
          port           = pgEdgePort pedge
          newst          = st { predDst = pnode, predInPort = Just port}
          expr'          = computePred newst
          term           = PredicateTerm (PTerm pnode port)
          expr           = case pnode of
                             (_, ONode {})            -> expr'
                             (_, FNode {nPorts = np}) -> if length np == 1
                                                             then expr'
                                                             else predAnd [expr', term]
-- compute predicate of src->dst, where dst is an ONode:
--  OP (e.g., AND) [ predicate of src -> pre ] for each pre in pre(dst)
computePred st@(PredCompSt { predDst = (_, ONode { nOperator = op })} ) = expr
    where inport = fromJust $ predInPort st
          -- predecessors of destintation (going backwards)
          predecessors = GH.labLPre (predGraph st) (predDst st)
          -- get predicates for all predecessors
          exprargs :: [PredicateExpr]
          exprargs = map getPred predecessors
          -- here we assume that O-nodes are not connecte in reverse
          -- (true->false, false->true)
          getPred :: (PGNode, PGEdge) -> PredicateExpr
          getPred (n,_) = computePred $ st {predDst = n, predInPort = Just "true"}
          -- predicate operand
          expr = case (inport, op) of
                  ("true", NOpAnd)  -> predAnd exprargs
                  ("true", NOpOr)   -> predOr exprargs
                  ("true", _)       -> error "NYI"
                  ("false", NOpAnd) -> predOr  $ map PredicateNot exprargs
                  ("false", NOpOr)  -> predAnd $ map PredicateNot exprargs
                  ("false", _)      -> error "NYI"
                  (_, _)            -> error $ "Expecting true/false, not:" ++ inport

-- silly hellper
pathPredicate :: PGraph -> String -> String -> PredicateExpr
pathPredicate g s d = computePred $ initPredCompSt g s d

------------------------------------------------
-- Embedding

-- embed state
data EmbedSt = EmbedSt {
      lpg           :: PGraph
    , prg           :: PGraph
    , lpgEntry      :: String   -- LPG node to connect PRG to
    , lpgSink       :: String
    , prgEntry      :: String
    , prgSink       :: String
    , embCandidates :: [PGNode] -- nodes that are candidates for embedding
} deriving (Show)

-- embed state initalizer with some defaults
initEmbedSt = EmbedSt {
      lpg           = undefined
    , prg           = undefined
    , lpgEntry      = "TxEntry"
    , lpgSink       = "TxOut"
    , prgEntry      = "TxEntry"
    , prgSink       = "TxOut"
    , embCandidates = []
}

-- state monad for manipulating EmbedSt
newtype EmbedExec a = EmbedExec { doEmbed :: State EmbedSt a }
    deriving (Monad, MonadState EmbedSt, Functor)
--
-- NOTE: let's start with embedding the TX side, and see later if/what applies
-- to the RX side
runEmbeddingTx :: EmbedSt -> (Maybe PGraph, EmbedSt)
runEmbeddingTx st0 = (runState $ doEmbed embedTx) st0

embedTx :: EmbedExec (Maybe PGraph)
embedTx = do
    candidatesInit
    embedNodes
    -- what happens at this point if there are PRG nodes that modify packets
    -- that also exist in the LPG. We probably need to abort.
    return Nothing

-- initialize candidate list
candidatesInit :: EmbedExec ()
candidatesInit = do
    lpg <- gets lpg
    sink <- getFNodeByName lpg <$> gets lpgSink
    modify $ \s -> s { embCandidates = (lpgGetCandidates lpg sink) }
    return ()

-- Get the (single) predecssor.
-- If there are more than one predecessors throw an error. Note that F-nodes
-- have, by definition, a single predecessor.
getSinglePre :: PGraph -> PGNode -> PGNode
getSinglePre g n = if (length ps) == 1 then (ps !! 0) else error "expecting single predecessor"
    where ps = GH.labPre g n

-- Get the embedding candidate list given the sink node.
-- The candidate nodes should be F-nodes and have no other F-nodes in the path
-- to the sink node. If they do, we need to embed this node first. If we cannot,
-- then an ordering dependency will be broken if we embed the successor.
lpgGetCandidates :: PGraph -> PGNode -> [PGNode]
lpgGetCandidates graph sink = filter isFNode $ GH.rdfsStop isONode [start_node] graph
    where start_node = getSinglePre graph sink


-- try to embed node
--  A node can be embedded if:
--    . No other F-node depends on it (this is ensured by choosing the candidates)
--    . We can embed LPG node X if all packets (and only those) that reach both
--      LPG:X and lpgSink reach PRG:X.
--  when done, update candidates as needed
tryEmbedNode :: PGNode -> EmbedExec ()
tryEmbedNode lpg_node = do
    (lpg, prg) <- gets $ \s -> (lpg s, prg s)
    let node_lbl = nLabel $ snd lpg_node
        prg_node = getFNodeByName' prg node_lbl
    case prg_node of
        Nothing -> return ()
        Just prg_node' -> tryEmbedMatchedNode lpg_node prg_node'
    return ()

tryEmbedMatchedNode :: PGNode -> PGNode -> EmbedExec ()
tryEmbedMatchedNode lpg_node prg_node = do
    (lpg, prg) <- gets $ \s -> (lpg s, prg s)
    prg_entry  <- getFNodeByName prg <$> gets prgEntry
    lpg_entry  <- getFNodeByName lpg <$> gets lpgEntry

    let pred_prg = computePred $ initPredCompSt_ { predGraph = prg, predSrc = prg_entry, predDst = prg_node }
        pred_lpg = computePred $ initPredCompSt_ { predGraph = lpg, predSrc = lpg_entry, predDst = lpg_node }

    case predEquiv pred_prg pred_lpg of
        False -> return () -- cannot embed
        True  -> doEmbedNode

doEmbedNode :: PGnode -> EmbedExec ()
doEmbedNode = do
    -- remove from LPG
    -- add candidates
    return ()

embedNodes :: EmbedExec ()
embedNodes = do
    candidates <- gets embCandidates
    case candidates of
        []     -> return ()
        (x:xs) -> do
            modify $ \s -> s { embCandidates = xs } -- update candidate list
            tryEmbedNode x                          -- try to embed x
            embedNodes                              -- recurse


-- print a path predicate
prPathPredicate :: PGraph -> String -> String -> IO (PredicateExpr)
prPathPredicate g s d = do
    let x =pathPredicate g s d
    putStrLn $ s ++ "->" ++ d ++ " has a path predicate: " ++ (show x)
    return x

main = do
    lpg <- U.fileToGraph "unicorn-tests/offload-tx-lpg.unicorn"
    prg <- U.fileToGraph "unicorn-tests/offload-tx-prg.unicorn"
    --let (emb, st) = runEmbeddingTx initEmbedSt { lpg = lpg, prg = prg }
    --putStrLn (ppShow $ map (\(_,x) -> nLabel x) (embCandidates st))
    prPathPredicate lpg "TxEntry" "TxUdpChecksum"
    prPathPredicate prg "TxEntry" "TxUdpChecksum"
    --prPathPredicate lpg "TxEthernetHdrFill" "TxOut"
    --prPathPredicate lpg "TxUdpChecksum" "TxOut"
    return ()


-- Notes on i82599's offload functions
--
-- Disclaimer: this is based on reading the datasheet without investigating all
-- details.
--

-- Tx side:
--
-- i82599 operates with transmit contexts. A TX context can be configured with
-- specific offload functions on the NIC. For example, to offload IPv4 header
-- checksum and UDP checksum you need to setup a context by specifying:
--  . TUCMD.IPV4: this is for IPv4 packets
--  . MACLEN:     offset from the start of the DMAed region to where the IPv4
--                checksum should start
--  . IPLEN:      length of the IPv4 header
--  . TUCMD.L4T:  L4 protocol (00b for UDP, 01b for TCP)
--
-- Once a TX context is setup, you can send packets via this context by
-- speficying it in the descriptors. There are 2 TX contexts per queue
-- available.
--
-- This means that packets that do not adhere to the setup parameters need to
-- sent via another context.
--
-- From a reading of the ixgbe linux driver source code, I believe that linux
-- willl set the TX context for every packet.
--
-- We can only offload nodes that:
--  . have no dependencies
--  . all of their dependencies can be offloaded
--
-- Assumming that we have setup two contexts in the i82599:
--  CTX0: no offload functions, everything in software
--  CTX1: IPv4 header/UDP checksum offload
-- We need to use CTX1 for packets that match the context configuration and CTX0
-- for everything else.

-- a test graph
g1 :: PGraph
g1 = U.strToGraph [r|
graph g1 {
    node SRC {
        port p1[A B C]
        port p2[D]
    }

    node A {
        port true[O1]
        port false[]
    }


    node B {
        port true[O1]
        port false[]
    }

    or O1 {
        port true[O2]
        port false[]
    }

    node C {
        port true[O2]
        port false[]
    }

    or O2 {
        port true[X]
        port false[]
    }

    node X { port o[DST]
             port e[]
    }

    node DST {}
    node D {}
}
|]

