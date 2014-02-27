module Dragonet.Implementation(
    Implementation,
    Packet,
    AttrValue(..),
    Context(..),
    GlobalState(..),
    ImplM,

    SocketType(..),
    UDPContext(..),
    TCPContext(..),
    PortTableUDP(..),
    PortTableTCP(..),
    TCPState(..),

    emptyPacket,
    initContext,
    initSimState,
    emptyGS,

    getCtx, putCtx,
    getGS, putGS,
    forkPkt, getPacket,

    packetLen,
    setAttr', setAttr, getAttr, getAttrM, dropAttr,

    convert16BE,
    unpack16BE,
    unpack32BE,
    pack32BE,

    readPsafe,
    readP8safe,
    readP16BEsafe,
    readP32BEsafe,
    readP,
    readPX,
    readP8,
    readP16BE,
    readP32BE,

    writeP,
    writeP8,
    writeP16BE,
    writeP32BE,
    insertP,

    -- For managing UDP ports
    findPortMappingUDP,
    findPortMappingUDP1,
    readPortMappingsUDP,
    writePortMappingsUDP,
    removePortMappingUDP,
    addPortMappingUDP,

    -- For managing TCP ports
    getPortMappingTCP,
    findPortMappingTCP1,
    readPortMappingsTCP,
    writePortMappingsTCP,
    removePortMappingTCP,
    addPortMappingTCP,
    findPortMappingTCPSocket,

    getSpecificSocketTCP,
    getSocketsTCP,
    addSocketTCP,

    -- updating socket state
    updateSocketTCP,
    updateSocketTCPState,
    updateSocketTCPSynNo,
    updateSocketTCPAckNo,
    updateSocketTCPPayload,
    decideFlags,

    -- For managing TCP ports without monad
    addPortMappingTCPGS,

    debug,
) where


import qualified Data.ByteString as BS
import qualified Util.ConcState as CS
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Bits as BITS


type Packet = BS.ByteString

emptyPacket :: Packet
emptyPacket = BS.empty

data AttrValue = AttrS String | AttrI Int | AttrD [Word8] | AttrW32 Word32
        | AttrW16 Word16 | AttrW8 Word8 | AttrB Bool
    deriving (Show, Eq)

data Context = Context {
    ctxPacket :: Packet,
    ctxAttrs  :: M.Map String AttrValue,
    ctxDebug  :: [String]
} deriving (Show, Eq)

--type FunType = (CS.ConcSM GlobalState Context [Char])
type FunType = Int

type ImplM a = CS.ConcSM GlobalState Context a
type Implementation = ImplM String

data SocketType =  ListenSocket | ConnectSocket | CloseSocket | AcceptSocket
    deriving (Show, Eq)



-- UDPContext remembers protocol specific information
data UDPContext =  UDPContext {
    socketStateUDP  :: SocketType,
    srcPortUDP      :: Word16,
    dstPortUDP      :: Word16
    }
    deriving (Show, Eq)

data TCPState = TCPClosed | TCPListen | TCPConnect | TCPSynSent
                | TCPSynRecved | TCPEstablished | TCPCloseWait
                | TCPLastAck | TCPFinWait1 | TCPFinWait2 | TCPClosing
                | TCPTimeWait
                deriving (Show, Eq)

-- TCPContext remembers TCP protocol specific information
data TCPContext =  TCPContext {
    socketStateTCP  :: SocketType,
    srcPortTCP      :: Word16,
    dstPortTCP      :: Word16,
    seqNoTCP        :: Word32,
    ackNoTCP        :: Word32,
    urgentNoTCP     :: Word16,
    windowTCP       :: Word16,
    payloadTCP      :: BS.ByteString,
    payloadLenTCP   :: Int,
    tcpState        :: TCPState
    }
    deriving (Show, Eq)

decideFlags :: TCPContext -> Word8
decideFlags sock = flags
    where
    fin = if (elem (tcpState sock) [TCPCloseWait, TCPLastAck]) then 1 else 0
    syn = if (elem (tcpState sock) [TCPListen, TCPSynRecved, TCPConnect]) then 1 else 0
    rst = 0
    psh = 0
    ack = if ((not $ elem (tcpState sock) [TCPListen,  TCPConnect]) &&
        (ackNoTCP sock /= 0)) then 1 else 0
    urg = if ((not $ elem (tcpState sock) [TCPListen, TCPConnect]) &&
        (urgentNoTCP sock /= 0)) then 1 else 0
    flags = (BITS.shift fin 0) .|. (BITS.shift syn 1) .|.
            (BITS.shift rst 2) .|. (BITS.shift psh 3) .|.
            (BITS.shift ack 4) .|. (BITS.shift urg 5)




--PortTable: One entry for each port.
--  This tells us if the port is open, which application owns the port
--      list of sockets associated with port
--      (muliple sockets can exist in case of listen socket)

-- FIXME: parameterize this datatype so that I can reuse it for both TCP and UDP.
data PortTableUDP = PortTableUDP {
        ptPortNoTCPUDP :: Int, -- Port number
        ptcontextUDP :: [UDPContext], -- one or more connections
        ptAppUDP :: String, -- Application name (for debugging)
        ptImplUDP :: Implementation -- Function to call
    }

instance Eq PortTableUDP where
 (PortTableUDP a _ _ _) == (PortTableUDP b _ _ _) = a == b

instance Show PortTableUDP where
  show (PortTableUDP a _ appName _) = (show a) ++ " " ++ (show appName)

data PortTableTCP = PortTableTCP {
        ptPortNoTCP :: Int, -- Port number
        ptcontextTCP :: [TCPContext], -- one or more connections
        ptAppTCP :: String, -- Application name (for debugging)
        ptImplTCP :: Implementation -- Function to call
    }

instance Eq PortTableTCP where
 (PortTableTCP a _ _ _) == (PortTableTCP b _ _ _) = a == b

instance Show PortTableTCP where
  show (PortTableTCP a _ appName _) = (show a) ++ " " ++ (show appName)


data GlobalState = GlobalState {
    gsDebug :: [String],
    gsTXQueue :: [Packet],
    gsARPPending :: [(Word32,Context)],
    gsARPCache :: M.Map Word32 [Word8],
    gsUDPPorts :: [PortTableUDP],
    gsTCPPorts :: [PortTableTCP]
} deriving (Show)


initContext :: Packet -> Context
initContext p = Context {
        ctxPacket = p,
        ctxAttrs = M.empty,
        ctxDebug = [] }

initSimState :: GlobalState -> Packet -> (GlobalState,Context)
initSimState gs p = (gs, initContext p)

emptyGS :: GlobalState
emptyGS = GlobalState {
        gsDebug = [],
        gsTXQueue = [],
        gsARPPending = [],
        gsARPCache = M.empty,
        gsUDPPorts = [],
        gsTCPPorts = []
    }


getCtx :: ImplM Context
getCtx = CS.getLS

putCtx :: Context -> ImplM ()
putCtx = CS.putLS

getGS :: ImplM GlobalState
getGS = CS.getGS

putGS :: GlobalState -> ImplM ()
putGS = CS.putGS

getPacket :: ImplM Packet
getPacket = do { ctx <- getCtx ; return (ctxPacket ctx) }

putPacket :: Packet -> ImplM ()
putPacket pkt = do { ctx <- getCtx ; putCtx (ctx { ctxPacket = pkt }) }



packetLen :: ImplM Int
packetLen = do
    ctx <- getCtx
    return $ BS.length $ ctxPacket ctx

setAttr' :: String -> AttrValue -> Context -> Context
setAttr' n v (Context p a g) = Context p (M.insert n v a) g

setAttr :: String -> AttrValue -> ImplM ()
setAttr n v = do
    ctx <- getCtx
    putCtx $ setAttr' n v ctx

getAttr :: String -> ImplM AttrValue
getAttr n = do
    a <- getAttrM n
    ctx <- getCtx
    gs <- getGS
    case a of
        Nothing -> error ("getAttr failed to find attribute: " ++
                        (show n) ++ ",\n ctx = " ++ (show ctx) ++
                        ",\n gs = " ++ (show gs))
        Just x -> return x
--    return $ fromJust a

getAttrM :: String -> ImplM (Maybe AttrValue)
getAttrM n = do
    ctx <- getCtx
    return $ attr ctx
    where
        attr c = M.lookup n $ ctxAttrs c

dropAttr :: String -> ImplM ()
dropAttr n = do
    (Context p a g) <- getCtx
    putCtx $ Context p (M.delete n a) g


forkPkt :: ImplM Context -> ImplM ()
forkPkt = CS.fork






convert16BE :: Word8 -> Word8 -> Word16
convert16BE w1 w2 =
    (shiftL (fromIntegral w1 :: Word16) 8) .|.
        (fromIntegral w2 :: Word16)


unpack16BE :: Word16 -> [Word8]
unpack16BE w =
    [(fromIntegral (shiftR w 8) :: Word8) , (fromIntegral w :: Word8)]

unpack32BE :: Word32 -> [Word8]
unpack32BE w =
    [(fromIntegral (shiftR w 24) :: Word8),
        (fromIntegral (shiftR w 16) :: Word8),
        (fromIntegral (shiftR w 8) :: Word8) ,
        (fromIntegral w :: Word8)]

pack32BE :: [Word8] -> Word32
pack32BE ps
    | length ps /= 4 = error "Expect list length of 4"
    | otherwise = convert32BE w1 w2 w3 w4
    where [w1,w2,w3,w4] = ps

convert32BE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
convert32BE w1 w2 w3 w4 =
    (shiftL (fromIntegral w1 :: Word32) 24) .|.
        (shiftL (fromIntegral w2 :: Word32) 16) .|.
        (shiftL (fromIntegral w3 :: Word32) 8) .|.
        (fromIntegral w4 :: Word32)



readPsafe :: Int -> Int -> ImplM [Word8]
readPsafe len offset = do
    pkt <- getPacket
    return $ BS.unpack $ BS.take len $ BS.drop offset pkt

readP8safe :: Int -> ImplM (Maybe Word8)
readP8safe offset = do
    ws <- readPsafe 1 offset
    case ws of
        w:[] -> return (Just w)
        _ -> return Nothing

readP16BEsafe :: Int -> ImplM (Maybe Word16)
readP16BEsafe offset = do
    ws <- readPsafe 2 offset
    case ws of
        w1:w2:[] -> return (Just $ convert16BE w1 w2)
        _ -> return Nothing

readP32BEsafe :: Int -> ImplM (Maybe Word32)
readP32BEsafe offset = do
    ws <- readPsafe 4 offset
    case ws of
        w1:w2:w3:w4:[] -> return (Just $ convert32BE w1 w2 w3 w4)
        _ -> return Nothing




readP :: Int -> Int -> ImplM [Word8]
readP len offset = do
    pkt <- getPacket
    let l = BS.unpack $ BS.take len $ BS.drop offset pkt
    if (length $ l) /= len then
        error ("Invalid read: " ++ (show (length $ l))
            ++ " /= " ++ (show len))
    else
        return l

readPX :: Int -> Int -> ImplM [Word8]
readPX len offset = do
    pkt <- getPacket
    let l = BS.unpack $ BS.take len $ BS.drop offset pkt
--    if (length $ l) /= len then
--        error ("Invalid read: " ++ (show (length $ l))
--            ++ " /= " ++ (show len))
--    else
    return l




readP8 :: Int -> ImplM Word8
readP8 offset = do
    w:[] <- readP 1 offset
    return w

readP16BE :: Int -> ImplM Word16
readP16BE offset = do
    w1:w2:[] <- readP 2 offset
    return (convert16BE w1 w2)

readP32BE :: Int -> ImplM Word32
readP32BE offset = do
    w1:w2:w3:w4:[] <- readP 4 offset
    return (convert32BE w1 w2 w3 w4)




writeP :: [Word8] -> Int -> ImplM ()
writeP dat offset = do
    pkt <- getPacket
    let dat' = BS.pack  dat
    if BS.length pkt < offset + BS.length dat' then
        error "Attempt to write outside of packet boundaries" else return ()
    putPacket (BS.take offset pkt `BS.append` dat' `BS.append`
        BS.drop (offset + BS.length dat') pkt)

writeP8 :: Word8 -> Int -> ImplM ()
writeP8 val = writeP [val]

writeP16BE :: Word16 -> Int -> ImplM ()
writeP16BE val = writeP (unpack16BE val)

writeP32BE :: Word32 -> Int -> ImplM ()
writeP32BE val = writeP (unpack32BE val)

-- Insert len bytes at location off.
insertP :: Int -> Int -> ImplM ()
insertP len off = do
    pkt <- getPacket
    let (pre,suf) = BS.splitAt off pkt
    let zs = BS.pack $ replicate len 0
    putPacket (pre `BS.append` zs `BS.append` suf)



------------------------------
-- UDP port management

readPortMappingsUDP :: ImplM [PortTableUDP]
readPortMappingsUDP = do
    gs <- getGS
    return $ gsUDPPorts gs

writePortMappingsUDP :: [PortTableUDP] -> ImplM ()
writePortMappingsUDP pm = do
    gs <- getGS
    putGS $ gs { gsUDPPorts = pm }


findPortMappingUDP1 :: Int -> ImplM (Maybe Implementation)
findPortMappingUDP1 p = do
        mappings <- readPortMappingsUDP
        -- get portNo. only list, and make sure that it has this port
        --      if not, return Nothing
        --  else
        --      locate the port no. again
        let found = filter (\x -> ((ptPortNoTCPUDP x) == p) ) mappings
            usedPorts = map ptPortNoTCPUDP found
            ans = if usedPorts == [] then Nothing
                    else Just (ptImplUDP (head found))
        return $ ans


findPortMappingUDP :: Int -> ImplM ([Implementation])
findPortMappingUDP p = do
        mappings <- readPortMappingsUDP
        let found = filter (\x -> ((ptPortNoTCPUDP x) == p) ) mappings
        return $ map ptImplUDP found

removePortMappingUDP :: Int -> ImplM ()
removePortMappingUDP p = do
        mappings <- readPortMappingsUDP
        let mappings' = filter (\x -> ((ptPortNoTCPUDP x) /= p) ) mappings
        writePortMappingsUDP mappings'

addPortMappingUDP :: Int -> String -> SocketType -> Implementation -> ImplM ()
addPortMappingUDP p appName stype imp = do
        mappings <- readPortMappingsUDP
        let usock = UDPContext stype 0 (fromIntegral p)
            mappings' = mappings ++ [(PortTableUDP p [usock] appName imp)]
        writePortMappingsUDP mappings'


debug :: String -> ImplM ()
debug s = do
    --ctx <- getCtx
    --putCtx $ ctx { ctxDebug = (ctxDebug ctx) ++ [s] }
    gs <- getGS
    putGS $ gs { gsDebug = gsDebug gs ++ [s] }


------------------------------
-- Socket and TCP state management

readPortMappingsTCP :: ImplM [PortTableTCP]
readPortMappingsTCP = do
    gs <- getGS
    return $ gsTCPPorts gs

writePortMappingsTCP :: [PortTableTCP] -> ImplM ()
writePortMappingsTCP pm = do
    gs <- getGS
    putGS $ gs { gsTCPPorts = pm }

findPortMappingTCP1 :: Int -> ImplM (Maybe (String, Implementation))
findPortMappingTCP1 p = do
        mappings <- readPortMappingsTCP
        -- get portNo. only list, and make sure that it has this port
        --      if not, return Nothing
        --  else
        --      locate the port no. again
        let found = filter (\x -> ((ptPortNoTCP x) == p) ) mappings
            ans = if found == [] then Nothing
                    else Just ((ptAppTCP (head found)) , (ptImplTCP (head found)))
        return $ ans

getPortMappingTCP :: Int -> ImplM ([PortTableTCP])
getPortMappingTCP p = do
        mappings <- readPortMappingsTCP
        let found = filter (\x -> ((ptPortNoTCP x) == p) ) mappings
        return found

findPortMappingTCPSocket :: Int -> ImplM (Maybe PortTableTCP)
findPortMappingTCPSocket p = do
        found <- getPortMappingTCP p
        case found of
                [] -> return Nothing
                x:[] -> return $ Just x
                _   -> error ("More than on port mapping block present "
                                    ++ (show found))


removePortMappingTCP :: Int -> ImplM ()
removePortMappingTCP p = do
        mappings <- readPortMappingsTCP
        let mappings' = filter (\x -> ((ptPortNoTCP x) /= p) ) mappings
        writePortMappingsTCP mappings'


addPortMappingTCP :: Int -> String -> SocketType -> TCPState ->
                        Implementation -> ImplM ()
addPortMappingTCP p appName stype tcpS imp = do
        mappings <- readPortMappingsTCP
        let usock = TCPContext stype 0 (fromIntegral p) 0 0 0 11 emptyPacket 0 tcpS
            mappings' = mappings ++ [(PortTableTCP p [usock] appName imp)]
        writePortMappingsTCP mappings'



-- Update the portMapping for given port number with new portMapping
updatePortMappingTCP :: PortTableTCP -> ImplM ()
updatePortMappingTCP pttcp = do
        mappings <- readPortMappingsTCP
        let mappings' = map (\x -> if ((ptPortNoTCP x) == (ptPortNoTCP pttcp))
                            then pttcp else x)
                            mappings
        writePortMappingsTCP mappings'

-- Return all TCP contexts associated with given port
getSocketsTCP ::  Word16  -> ImplM ([TCPContext])
getSocketsTCP dport = do
        socks <- findPortMappingTCPSocket (fromIntegral dport)
        return (maybe [] ptcontextTCP socks)
--        m <- getPortMappingTCP (fromIntegral dport)
--        return $ ptcontextTCP m


-- Find specific socket which which matches both source and destination port
--getSpecificSocketTCP :: Word16 -> Word16 -> ImplM ([TCPContext])
getSpecificSocketTCP :: Word16 -> Word16 -> ImplM (Maybe TCPContext)
getSpecificSocketTCP sport dport = do
        socks <- getSocketsTCP dport
        let matched = filter (\x -> ((srcPortTCP x) == sport) &&
                                ((dstPortTCP x) == dport)) socks
            ans = case matched of
                [] -> Nothing
                x:[] -> Just x
                _  -> error ("More than one matching ports found "
                        ++ show (matched))
{-        debug ("specificSocket for source:" ++ (show sport)
            ++ ", dest:" ++ (show dport)
            ++ ", for state: " ++ (show socks)
            ++ ", \n\nis ===> " ++ (show ans)
            )
-}
        return ans

-- Add new socket such with given source and destionation port numbers
addSocketTCP :: Word16 -> Word16 -> TCPContext -> ImplM()
addSocketTCP sport dport ctcp = do
    -- Find portMapping
    mappings <- getPortMappingTCP (fromIntegral dport)
    let m = case mappings of
            [] -> error "No portMapping present to add socket into it."
            x:[] -> x
            _ -> error "More than one portMapping present for same port no."

        socks = ptcontextTCP m
        matched = filter (\x -> ((srcPortTCP x) == sport) &&
                                ((dstPortTCP x) == dport)) socks
        ans = case matched of
            [] -> socks ++ [ctcp]
            _   -> error ("socket for specified ports exist "
                                    ++ (show ans))
    debug "addSocketTCP: new connection added!"
    updatePortMappingTCP (m {ptcontextTCP = ans})


-- Add new socket such with given source and destionation port numbers
updateSocketTCP :: Word16 -> Word16 -> TCPContext -> ImplM()
updateSocketTCP sport dport ctcp = do
    -- Find portMapping
    mappings <- getPortMappingTCP (fromIntegral dport)
    let m = case mappings of
            [] -> error "No portMapping present to add socket into it."
            x:[] -> x
            _ -> error "More than one portMapping present for same port no."

        socks = ptcontextTCP m
        matched = filter (\x -> ((srcPortTCP x) == sport) &&
                                ((dstPortTCP x) == dport)) socks
        ans = case matched of
            []   -> error ("socket for specified ports does not exist ")
            x:[] -> map (\x -> ( if (((srcPortTCP x) == sport) &&
                                ((dstPortTCP x) == dport)) then ctcp else x))
                                socks
            x:xs -> error ("More than one sockets present for same port mappings"
                            ++ show (ans))
    debug "update: connection updated!"
    updatePortMappingTCP (m {ptcontextTCP = ans})


updateSocketTCPState :: Word16 -> Word16 -> TCPState -> ImplM()
updateSocketTCPState sport dport st = do
   Just sock <- getSpecificSocketTCP sport dport
   updateSocketTCP sport dport (sock {tcpState = st})

updateSocketTCPAckNo :: Word16 -> Word16 -> Word32 -> ImplM()
updateSocketTCPAckNo sport dport ackno = do
   Just sock <- getSpecificSocketTCP sport dport
   updateSocketTCP sport dport (sock {ackNoTCP = ackno})

updateSocketTCPSynNo :: Word16 -> Word16 -> Word32 -> ImplM()
updateSocketTCPSynNo sport dport synno = do
   Just sock <- getSpecificSocketTCP sport dport
   updateSocketTCP sport dport (sock {seqNoTCP = synno})

updateSocketTCPPayload :: Word16 -> Word16 -> [Word8] -> ImplM()
updateSocketTCPPayload sport dport payload = do
   Just sock <- getSpecificSocketTCP sport dport
   let payloadlen = length payload
       payloadBS = BS.pack payload
   updateSocketTCP sport dport (sock { payloadTCP = payloadBS,
                payloadLenTCP = payloadlen})


------------------------------
-- TCP management without monads

wPortMappingsTCP :: GlobalState -> [PortTableTCP] -> GlobalState
wPortMappingsTCP gs pm =  gs { gsTCPPorts = pm }

fPortMappingTCP1 :: GlobalState -> Int -> Maybe Implementation
fPortMappingTCP1 gs p = ans
    where
    mappings = gsTCPPorts gs
    found = filter (\x -> ((ptPortNoTCP x) == p) ) mappings
    usedPorts = map ptPortNoTCP found
    ans = if usedPorts == [] then Nothing
          else Just (ptImplTCP (head found))

fPortMappingTCP :: GlobalState -> Int -> [Implementation]
fPortMappingTCP gs p = ans
    where
        mappings = gsTCPPorts gs
        found = filter (\x -> ((ptPortNoTCP x) == p) ) mappings
        ans = map ptImplTCP found

rmPortMappingTCP :: GlobalState -> Int -> GlobalState
rmPortMappingTCP gs p = ans
    where
        mappings = gsTCPPorts gs
        mappings' = filter (\x -> ((ptPortNoTCP x) /= p) ) mappings
        ans = gs { gsTCPPorts = mappings'}

addPortMappingTCPGS :: GlobalState -> Int -> String -> SocketType ->
        TCPState -> Implementation -> GlobalState
addPortMappingTCPGS gs p appName stype tcpS imp = ans
    where
        mappings = gsTCPPorts gs
        usock = TCPContext stype 0 (fromIntegral p) 0 0 0 11 emptyPacket 0 tcpS
        mappings' = mappings ++ [(PortTableTCP p [usock] appName imp)]
        ans = gs { gsTCPPorts = mappings'}


