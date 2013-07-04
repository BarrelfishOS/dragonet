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

unpack16BE :: Word16 -> [Word8]
unpack16BE w =
    [(fromIntegral (shiftR w 8) :: Word8) , (fromIntegral w :: Word8)]


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



sofwareRXImpl = toPort "out"

-----------------------------------------------------------------------------
-- Ethernet

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


-----------------------------------------------------------------------------
-- ARP

l3ARPValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    -- hardcoded for Ethernet/IPv4
    if len - off == 28 then do
        hlen <- readP8 (off + 4)
        plen <- readP8 (off + 5)
        return $ pbool $ ((hlen == 6) && (plen == 4))
    else
        return "false"
        

l3ARPClassifyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    oper <- readP16BE (off + 6)
    return $ case oper of
        1 -> "request"
        2 -> "reply"
        _ -> "drop"


-----------------------------------------------------------------------------
-- IPv4

ipHeaderLen = do
    (AttrI off) <- getAttr "L3Offset"
    ihl <- readP8 off
    return $ 4 * ((fromIntegral ihl :: Int) .&. 0xf)

-- Kind of ugly: convert pairs of bytes to 16bit integers, but use 32bit
-- arithmetic when summing up, in the end combine the higher and lower 16 bits
ipChecksum :: [Word8] -> Word16
ipChecksum p = cxsm
    where
        padded = if (length p) `mod` 2 /= 0 then p ++ [0] else p
        convertSingle a b = fromIntegral (convert16BE a b) :: Word32
        convert [] = []
        convert (a:b:rest) = (convertSingle a b):(convert rest)
        s32 = sum $ convert padded
        foldInt i = ((i .&. 0xffff) + (shiftR i 16))
        cxsm32 = xor 0xffff $ foldInt $ foldInt s32
        cxsm = fromIntegral cxsm32 :: Word16

l3IPv4ValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    if ((len - off) < 20) then
        toPort "false"
    else do
        hlen <- ipHeaderLen
        toPort $ pbool $ (hlen >= 20 && (len - off) >= hlen)

-- For now we just make sure the packet is not fragmented
l3IPv4ValidReassemblyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    fragOff <- readP16BE $ off + 6
    toPort $ pbool $ ((fragOff .&. 0x2000) == 0 && (fragOff .&. 0x1fff) == 0)


l3IPv4ValidVersionImpl = do
    (AttrI off) <- getAttr "L3Offset"
    ver <- readP8 off
    toPort $ pbool ((shiftR ver 4) == 4)

l3IPv4ValidLengthImpl = do
    (AttrI off) <- getAttr "L3Offset"
    len <- packetLen
    ipLen <- readP16BE (off + 2)    
    toPort $ pbool ((fromIntegral ipLen :: Int) + off <= len)

l3IPv4ValidTTLImpl = toPort "true"

l3IPv4ValidChecksumImpl = do
    (AttrI off) <- getAttr "L3Offset"
    hlen <- ipHeaderLen
    pkt <- readP off hlen
    toPort $ pbool $ (ipChecksum pkt) == 0

l3IPv4ClassifyImpl = do
    (AttrI off) <- getAttr "L3Offset"
    hlen <- ipHeaderLen
    setAttr "L4Offset" $ AttrI $ off + hlen
    proto <- readP8 (off + 9)
    toPort $ case proto of
        0x01 -> "icmp"
        0x06 -> "tcp"
        0x11 -> "udp"
        _ -> "drop"


-----------------------------------------------------------------------------
-- IPv6

l3IPv6ValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- ICMP
l3ICMPValidHeaderLengthImpl = toPort "true"


-----------------------------------------------------------------------------
-- UDP

ipv4Pseudoheader p len = do
    (AttrI off) <- getAttr "L3Offset"
    sIP <- readP (off + 12) 4
    dIP <- readP (off + 16) 4
    return (sIP ++ dIP ++ [0] ++ [p] ++ (unpack16BE len))

l4UDPValidHeaderLengthImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    toPort $ pbool ((len - off) >= 8)

l4UDPValidLengthImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    udpLen <- readP16BE (off + 4)
    toPort $ pbool (len >= (fromIntegral udpLen) + off)

l4UDPValidChecksumImpl = do
    (AttrI off) <- getAttr "L4Offset"
    len <- packetLen
    cxsm <- readP16BE (off + 6)
    pkt <- readP off (len - off)
    pheader <- ipv4Pseudoheader 0x11 (fromIntegral (len - off))
    toPort $ pbool (cxsm == 0 || (ipChecksum (pheader ++ pkt)) == 0)


-----------------------------------------------------------------------------
-- TCP

l4TCPValidHeaderLengthImpl = toPort "true"   


    


-- Sinks
packetDropImpl = toPort "Packet dropped!"
l3ARPRequestImpl = toPort "Got ARP request!"
l3ARPResponseImpl = toPort "Got ARP response!"
l3ICMPOutImpl = toPort "Got ICMP packet!"
l4TCPOutImpl = toPort "Got TCP packet!"
l4UDPOutImpl = toPort "Got UDP packet!"










-- get LPG
[dragonetImpl_f|lpgImpl.dragonet|]


-----------------------------------------------------------------------------
-- Simulation


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

-- Find predecessor for specfied node in edge list
getPredecessors :: Eq a => a -> [(a,a)] -> [a]
getPredecessors n e =
    L.nub $ map fst $ filter (\(_,x) -> x == n) e

-- Topological sort on edge list representation
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


-- Execute graph. First parameter is expected to be a topologically sorted
-- list of the graph nodes, the second list is used to store the enablement
-- indication, so (enabled node, origin, port), and the last argument is the
-- context to start with. Returned is the port name returned by the
-- implementation of the last sink node and the last state.
--
-- Basic idea: use topologically sorted list of nodes, this way all predecessors
-- will have been calculated when arriving at a node (if enabled). The output
-- port taken for a node will be stored in a list, used for implemented AND/OR
-- nodes.
executeNode :: [ImplNode] -> [(ImplNode,ImplNode,String)] -> Context -> (String,Context)
executeNode [] _ ctx = ("Got stuck :-/", ctx)
executeNode (i:is) ret ctx =
        if not $ null invalues then -- Node was enabled
            T.trace ("executeNode " ++ show i ++ " ") (
            if null p then (port, nctx)  -- no outgoing edges -> arrived in sink
            else T.trace ("  -> port=" ++ show port) (executeNode is newret nctx)
            )
        else -- Node not enabled, just skip it
            executeNode is ret ctx
            
    where
        (ImplNode n impl p) = i
        ((port, nctx), newret) =
            case n of
                (OP.Des _) -> (runState (fromJust impl) ctx, updateRet)
                (OP.Opr (OP.Operator gn)) ->
                    if (take 3 $ OP.gLabel gn) == "AND" then andImpl else orImpl


        -- "Inputs" for this node
        inPValues = filter (\(x,_,_) -> x == i) ret
        invalues = map (\(_,_,x) -> x) inPValues

        invalidPort pt = if pt == "" then [] else error ("Invalid outport " ++ pt ++ " in node " ++ (show i))
        -- Outgoing edges for selected port
        outedges = fromMaybe (invalidPort port) $ lookup port p
        addret = map (\x -> (x, i, port)) outedges
        updateRet = (filter (\(x,_,_) -> x /= i) ret) ++ addret

        orImpl =
            if (L.any ((==) "true") invalues) then (("true",ctx),updateRet)
            else if depsMet then (("false",ctx),updateRet) else (("",ctx),ret)
        andImpl =
            if (L.any ((==) "false") invalues) then (("false",ctx),updateRet)
            else if depsMet then (("true",ctx),updateRet) else (("",ctx),ret)

        -- Check if all predecessors enabled node (required for operator nodes)
        depsMet =
            all (\x -> isJust $ L.find (\(y,z,_) -> (y == i) && (x == z)) inPValues) $
                getPredecessors i graphEdges




graphEdges = getEdgeList sofwareRXImplNode

main = do
    arpReq <- BS.readFile "packets/arp_request"
    putStrLn "ARP Request"
    putStrLn $ show $ execute arpReq
    putStrLn $ show $ getPredecessors l2EtherValidImplNode graphEdges

    icmpReq <- BS.readFile "packets/icmp_request"
    putStrLn "\n\nICMP Request"
    putStrLn $ show $ execute icmpReq

    icmpResp <- BS.readFile "packets/icmp_response"
    putStrLn "\n\nICMP Request"
    putStrLn $ show $ execute icmpResp

    dnsQry <- BS.readFile "packets/dns_query"
    putStrLn "\n\nDNS Request (udp checksum broken)"
    putStrLn $ show $ execute dnsQry

    dnsResp <- BS.readFile "packets/dns_response"
    putStrLn "\n\nDNS Response (udp checksum good)"
    putStrLn $ show $ execute dnsResp


    where
        execute p = fst $ executeNode ts [(sofwareRXImplNode,sofwareRXImplNode,"in")] $ Context p M.empty
        ts = topSort $ graphEdges
        testIt p f =
            (op, ctxAttrs ctx)
            where (op,ctx) = runState f $ Context p M.empty
