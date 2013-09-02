module Dragonet.Implementation(
    Implementation,
    Packet,
    AttrValue(..),
    Context(..), 
    GlobalState(..),
    SimState(..),
    ContextID,
    ImplM,

    initSimState,
    emptyGS,

    getGS,
    putGS,
    forkPkt,

    packetLen,
    setAttr,
    getAttr,

    convert16BE,
    unpack16BE,
    unpack32BE,
    pack32BE,

    readPsafe,
    readP8safe,
    readP16BEsafe,
    readP32BEsafe,
    readP,
    readP8,
    readP16BE,
    readP32BE,

    writeP,
    writeP8,
    writeP16BE,
    writeP32BE,
    insertP,

    debug,
) where


import qualified Data.ByteString as BS
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Data.Bits

type Packet = BS.ByteString

data AttrValue = AttrS String | AttrI Int
    deriving Show

data Context = Context {
    ctxPacket :: Packet,
    ctxAttrs  :: M.Map String AttrValue,
    ctxDebug  :: [String]
} deriving Show

data GlobalState = GlobalState {
    gsDebug :: [String]
} deriving Show

type ContextID = Int

data SimState = SimState {
    ssContexts :: M.Map ContextID Context,
    ssCurCtx :: ContextID,
    ssNextCtx :: ContextID,
    ssForked :: M.Map ContextID String,
    ssGState :: GlobalState
}

type Implementation = State SimState String

type ImplM a = State SimState a

initContext :: Packet -> Context
initContext p = Context {
        ctxPacket = p,
        ctxAttrs = M.empty,
        ctxDebug = [] }

initSimState :: GlobalState -> Packet -> SimState
initSimState gs p = SimState {
        ssContexts = M.singleton 0 ctx,
        ssCurCtx = 0,
        ssNextCtx = 1,
        ssForked = M.empty,
        ssGState = gs }
    where ctx = initContext p

emptyGS :: GlobalState
emptyGS = GlobalState {
        gsDebug = []
    }



getCtx :: ImplM Context
getCtx = do
    ss <- get
    return (fromJust $ M.lookup (ssCurCtx ss) $ ssContexts ss)

putCtx :: Context -> ImplM ()
putCtx ctx = do
    ss <- get
    put $ ss { ssContexts = M.insert (ssCurCtx ss) ctx (ssContexts ss) }

getGS :: ImplM GlobalState
getGS = do { ss <- get ; return (ssGState ss) }

putGS :: GlobalState -> ImplM ()
putGS gs = do { ss <- get ; put (ss { ssGState = gs }) }

getPacket :: ImplM Packet
getPacket = do { ctx <- getCtx ; return (ctxPacket ctx) }

putPacket :: Packet -> ImplM ()
putPacket pkt = do { ctx <- getCtx ; putCtx (ctx { ctxPacket = pkt }) }



packetLen :: ImplM Int
packetLen = do
    ctx <- getCtx
    return $ BS.length $ ctxPacket ctx

setAttr :: String -> AttrValue -> ImplM ()
setAttr n v = do
    (Context p a g) <- getCtx
    putCtx $ Context p (M.insert n v a) g

getAttr :: String -> ImplM AttrValue
getAttr n = do
    ctx <- getCtx
    return $ attr ctx
    where
        attr c = fromJust $ M.lookup n $ ctxAttrs c

forkPkt :: ImplM String -> ImplM ()
forkPkt fp = do
    ss <- get
    let ctx = initContext BS.empty
    let oldCid = ssCurCtx ss
    let newCid = ssNextCtx ss
    put $ ss {  -- Enable new context
            ssCurCtx = newCid,
            ssNextCtx = newCid + 1,
            ssContexts = M.insert newCid ctx $ ssContexts ss }
    port <- fp  -- Execute new handler
    ss' <- get
    put (ss' { -- Switch back to original context
        ssCurCtx = oldCid,
        ssForked = M.insert newCid port $ ssForked ss' })






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
        error "Invalid read"
    else
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




writeP :: Int -> [Word8] -> ImplM ()
writeP offset dat = do
    pkt <- getPacket
    let dat' = BS.pack  dat
    if BS.length pkt < offset + BS.length dat' then
        error "Attempt to write outside of packet boundaries" else return ()
    putPacket (BS.take offset pkt `BS.append` dat' `BS.append`
        BS.drop (offset + BS.length dat') pkt)

writeP8 :: Int -> Word8 -> ImplM ()
writeP8 off val = writeP off [val]

writeP16BE :: Int -> Word16 -> ImplM ()
writeP16BE off val = writeP off $ unpack16BE val

writeP32BE :: Int -> Word32 -> ImplM ()
writeP32BE off val = writeP off $ unpack32BE val

insertP :: Int -> Int -> ImplM ()
insertP off len = do
    pkt <- getPacket
    let (pre,suf) = BS.splitAt off pkt
    let zs = BS.pack $ replicate len 0
    putPacket (pre `BS.append` zs `BS.append` suf)



debug :: String -> ImplM ()
debug s = do
    --ctx <- getCtx
    --putCtx $ ctx { ctxDebug = (ctxDebug ctx) ++ [s] }
    gs <- getGS
    putGS $ gs { gsDebug = gsDebug gs ++ [s] }
