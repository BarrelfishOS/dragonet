#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import DragonetDSL
import qualified Operations as OP
import qualified DotGenerator as DG


import Debug.Trace as T
import Control.Monad.State
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as BS

type Packet = BS.ByteString

data AttrValue = AttrS String | AttrI Int
    deriving Show
data Context = Context {
    ctxPacket :: Packet,
    ctxAttrs  :: M.Map String AttrValue
}
    deriving Show
data BoolPort = Ptrue | Pfalse
    deriving Show

data ImplNode = ImplNode {
    inNode :: OP.Node,
    inImpl :: Maybe (State Context String),
    inPorts :: [(String, [ImplNode])]
}

instance Eq ImplNode where
    (==) a b = (inNode a) == (inNode b)

instance Show ImplNode where
    show i = "ImplNode." ++ (nLabelStr $ inNode i)



packetLen :: State Context Int
packetLen = do
    ctx <- get
    return $ BS.length $ ctxPacket ctx

setAttr :: String -> AttrValue -> State Context ()
setAttr n v = do
    (Context p a) <- get
    put $ Context p $ M.insert n v a

getAttr :: String -> State Context AttrValue
getAttr n = do
    ctx <- get
    return $ attr ctx
    where
        attr c = fromJust $ M.lookup n $ ctxAttrs c


convert16BE w1 w2 =
    (shiftL (fromIntegral w1 :: Word16) 8) .|.
        (fromIntegral w2 :: Word16)

convert32BE w1 w2 w3 w4 =
    (shiftL (fromIntegral w1 :: Word32) 24) .|.
        (shiftL (fromIntegral w2 :: Word32) 16) .|.
        (shiftL (fromIntegral w3 :: Word32) 8) .|.
        (fromIntegral w4 :: Word32)



readPsafe :: Int -> Int -> State Context [Word8]
readPsafe offset len = do
    ctx <- get
    return $ BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8safe :: Int -> State Context (Maybe Word8)
readP8safe offset = do
    ctx <- get
    ws <- readPsafe offset 1
    case ws of
        w:[] -> return (Just w)
        _ -> return Nothing

readP16BEsafe :: Int -> State Context (Maybe Word16)
readP16BEsafe offset = do
    ctx <- get
    ws <- readPsafe offset 2
    case ws of
        w1:w2:[] -> return (Just $ convert16BE w1 w2)
        _ -> return Nothing

readP32BEsafe :: Int -> State Context (Maybe Word32)
readP32BEsafe offset = do
    ctx <- get
    ws <- readPsafe offset 4
    case ws of
        w1:w2:w3:w4:[] -> return (Just $ convert32BE w1 w2 w3 w4)
        _ -> return Nothing




readP :: Int -> Int -> State Context [Word8]
readP offset len = do
    ctx <- get
    if (length $ l ctx) /= len then
        error "Invalid read"
    else
        return (l ctx)
    where
        l ctx = BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8 :: Int -> State Context Word8
readP8 offset = do
    ctx <- get
    w:[] <- readP offset 1
    return w

readP16BE :: Int -> State Context Word16
readP16BE offset = do
    ctx <- get
    w1:w2:[] <- readP offset 2
    return (convert16BE w1 w2)

readP32BE :: Int -> State Context Word32
readP32BE offset = do
    ctx <- get
    w1:w2:w3:w4:[] <- readP offset 4
    return (convert32BE w1 w2 w3 w4)









pbool b = if b then "true" else "false"

toPort = return



sourceImpl = toPort "out"

l2EtherClassifiedImpl = do
    return "true"

l2EtherValidLengthImpl = do
    len <- packetLen
    --toPort $ pbool (len >= 56)
    toPort $ pbool (len >= 14) -- Actually an ethernet frame needs to be
                               -- at least 56 bytes, in our packets the padding
                               -- is removed

l2EtherValidTypeImpl = do
    etype <- readP16BE 12
    toPort $ pbool (etype >= 0x0800)

l2EtherValidMulticastImpl = do
    dmac <- readP 0 6
    toPort $ pbool
        ((dmac /= ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) &&
        (((head dmac) .&. 1) == 1)))
   

l2EtherValidBroadcastImpl = do
    dmac <- readP 0 6
    toPort $ pbool (dmac == ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff]))

l2EtherValidUnicastImpl = do
    dmac <- readP8 0
    toPort $ pbool $ ((dmac .&. 1) == 0)

l2EtherValidSrcImpl = do
    toPort "true"

l2EtherClassifyL3Impl = do
    etype <- readP16BE 12
    setAttr "L3Offset" (AttrI 14)
    toPort $ case etype of
        0x0800 -> "ipv4"
        0x86DD -> "ipv6"
        0x0806 -> "arp"
        _ -> "drop"


l3IPv4ValidHeaderLengthImpl = do
    l2EtherClassifyL3Impl
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    ihl <- if ((len - off) < 20) then
        toPort 20 -- does not matter as long as it's long enough
    else
        readP8 off
    hlen <- toPort ((fromIntegral ihl :: Int) .&. 0xf)
    toPort $ pbool $ (hlen >= 20 && (len - off) >= 4*hlen)

l3IPv4ValidVersionImpl = do
    l2EtherClassifyL3Impl
    (AttrI off) <- getAttr "L3Offset"
    ver <- readP8 off
    toPort $ pbool ((shiftR ver 4) == 4)
    

l3IPv6ValidHeaderLengthImpl = do
    toPort "true"

-- Sinks
packetDropImpl = toPort "Packet dropped!"
l3ARPOutImpl = toPort "Got ARP packet!"
l3IPOutImpl = toPort "Valid IP packet!"


-- get LPG
[dragonetImpl_f|lpgImpl.dragonet|]

-- List of outgoing edges of this node
--getOutEdges :: OP.Node -> [(OP.Node,OP.Node)]
--getOutEdges n =
--    map toTup $ case n of
--        (OP.Des (OP.Decision gn)) -> convert $ OP.gEdges gn
--        (OP.Opr (OP.Operator gn)) -> convert $ OP.gEdges gn
--    where
--        toTup e = (n,e)
--        convert (OP.BinaryNode (as, bs)) = as ++ bs
--       convert (OP.NaryNode es) = concat es

-- Get string label for GNode
gLabelStr gn = OP.gLabel gn

nLabelStr (OP.Des (OP.Decision gn)) = gLabelStr gn
nLabelStr (OP.Conf (OP.Configuration gn)) = gLabelStr gn
nLabelStr (OP.Opr (OP.Operator gn)) = gLabelStr gn


getOutEdges :: ImplNode -> [(ImplNode,ImplNode)]
getOutEdges n =
    concat portEdges
    where
        portEdges = map (map (\x -> (n,x))) $ map snd $ inPorts n

-- List of edges in graph, might contain duplicates if there are double edges
-- in graph
getEdgeList :: ImplNode -> [(ImplNode,ImplNode)]
getEdgeList n = out ++ (concat $ map getEdgeList successors)
    where
        out = getOutEdges n
        successors = L.nub $ map snd out


topSort :: Eq a => [(a,a)] -> [a]
topSort [] = []
topSort es =
    noIncoming ++ orphaned ++ (topSort newEdges)
    where
        -- Is n a successor of another node?
        notSucc n = isNothing $ L.find (\x -> (snd x) == n) es
        -- All nodes without incoming edges
        noIncoming = filter (notSucc) $ L.nub $ map fst es
        -- edges that don't start at noIncoming nodes
        newEdges = filter (\x -> notElem (fst x) noIncoming) es
        -- edges that start at noIncoming nodes
        dropped = filter (\x -> elem (fst x) noIncoming) es
        -- Sink nodes (without outgoing edges) that lost their incoming edges
        -- those also vanish from the edges list
        isOrphaned n = isNothing $ L.find (\x -> ((snd x) == n) || ((fst x) == n)) newEdges
        orphaned = L.nub $ filter isOrphaned $ map snd dropped

executeNode [] _ ctx = ("Got stuck :-/", ctx)
executeNode (i:is) ret ctx =
        if not $ null invalues then
            T.trace ("executeNode " ++ show i) (
            -- Node was enabled
            if null p then
                (port, nctx)  -- no outgoing edges -> arrived in sink
            else
                T.trace ("  -> port=" ++ show port) (
                executeNode is newret nctx)
            )
        else
            -- Node not enabled, just skip it
            executeNode is ret ctx
            
            
    where
        (ImplNode n impl p) = i
        (port, nctx) =
            case n of
                (OP.Des _) -> runState (fromJust impl) ctx
                (OP.Opr (OP.Operator gn)) ->
                    -- Rather ugly
                    if (take 4 $ OP.gLabel gn) == "AND" then
                        (andImpl, ctx)
                    else
                        (orImpl, ctx)

        andImpl = pbool $ L.all ((==) "true") invalues
        orImpl = pbool $ L.any ((==) "true") invalues
        invalues = map snd $ filter (\(x,_) -> x == n) ret
        outedges = fromMaybe (error ("Invalid outport " ++ port ++ " in node " ++ (show i))) $ lookup port p
        addret = map (\x -> (inNode x, port)) outedges
        newret = (filter (\(x,_) -> x /= n) ret) ++ addret

main = do
    arpReq <- BS.readFile "packets/arp_request"
    putStrLn "ARP Request"
    putStrLn $ show $ execute arpReq

    icmpReq <- BS.readFile "packets/icmp_request"
    putStrLn "ICMP Request"
    putStrLn $ show $ execute icmpReq

--    putStrLn $ show $ DG.toDot l2EtherClassifyL3
--    putStr "l2EtherValidLengthImpl: "
--    putStrLn $ show $ testIt arp l2EtherValidLengthImpl
    where
        execute p = fst $ executeNode ts [(source,"in")] $ Context p M.empty
        ts = topSort $ getEdgeList sourceImplNode
        testIt p f =
            (op, ctxAttrs ctx)
            where (op,ctx) = runState f $ Context p M.empty
