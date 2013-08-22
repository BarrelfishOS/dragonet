module Dragonet.Implementation(
    Implementation,
    Packet,
    AttrValue(..),
    Context(..), 
    GlobalState(..),
    ImplM,

    emptyGS,

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
    ctxState  :: GlobalState
} deriving Show

data GlobalState = GlobalState {
    gsDebug :: [String]
} deriving Show

type Implementation = State Context String

type ImplM a = State Context a


emptyGS :: GlobalState
emptyGS = GlobalState {
        gsDebug = []
    }


packetLen :: State Context Int
packetLen = do
    ctx <- get
    return $ BS.length $ ctxPacket ctx

setAttr :: String -> AttrValue -> State Context ()
setAttr n v = do
    (Context p a g) <- get
    put $ Context p (M.insert n v a) g

getAttr :: String -> State Context AttrValue
getAttr n = do
    ctx <- get
    return $ attr ctx
    where
        attr c = fromJust $ M.lookup n $ ctxAttrs c







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



readPsafe :: Int -> Int -> State Context [Word8]
readPsafe len offset = do
    ctx <- get
    return $ BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8safe :: Int -> State Context (Maybe Word8)
readP8safe offset = do
    ws <- readPsafe 1 offset
    case ws of
        w:[] -> return (Just w)
        _ -> return Nothing

readP16BEsafe :: Int -> State Context (Maybe Word16)
readP16BEsafe offset = do
    ws <- readPsafe 2 offset
    case ws of
        w1:w2:[] -> return (Just $ convert16BE w1 w2)
        _ -> return Nothing

readP32BEsafe :: Int -> State Context (Maybe Word32)
readP32BEsafe offset = do
    ws <- readPsafe 4 offset
    case ws of
        w1:w2:w3:w4:[] -> return (Just $ convert32BE w1 w2 w3 w4)
        _ -> return Nothing




readP :: Int -> Int -> State Context [Word8]
readP len offset = do
    ctx <- get
    if (length $ l ctx) /= len then
        error "Invalid read"
    else
        return (l ctx)
    where
        l ctx = BS.unpack $ BS.take len $ BS.drop offset $ ctxPacket ctx

readP8 :: Int -> State Context Word8
readP8 offset = do
    w:[] <- readP 1 offset
    return w

readP16BE :: Int -> State Context Word16
readP16BE offset = do
    w1:w2:[] <- readP 2 offset
    return (convert16BE w1 w2)

readP32BE :: Int -> State Context Word32
readP32BE offset = do
    w1:w2:w3:w4:[] <- readP 4 offset
    return (convert32BE w1 w2 w3 w4)


debug :: String -> ImplM ()
debug s = do
    ctx <- get
    let gs = ctxState ctx
    put $ ctx { ctxState = gs { gsDebug = gsDebug gs ++ [s] } }
