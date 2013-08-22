module Dragonet.Implementation.IPv4(
    versionRd, ihlRd, lengthRd, identificationRd, flagsRd, fragmentRd, ttlRd,
    protocolRd, checksumRd, sourceIPRd, destIPRd,
    flagsDF, flagsMF,
    protocolICMP, protocolTCP, protocolUDP,
    headerMinLen, headerLen, headerOff, payloadOff, payloadLen, checksum,
    pseudoheader, ipFromString,
) where

import Dragonet.Implementation
import Util.Misc
import Data.Word
import Data.Bits

headerMinLen :: Int
headerMinLen = 20

headerLen :: ImplM Int
headerLen = do { ihl <- ihlRd ; return ((fromIntegral ihl) * 4) }

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L3Offset" ; return i }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }

payloadLen :: ImplM Int
payloadLen = do
    h <- headerLen
    l <- lengthRd
    return ((fromIntegral l) - h)

payloadOff :: ImplM Int
payloadOff = do { i <- headerOff ; l <- headerLen ; return (i + l) }



versionOff :: ImplM Int
versionOff = fieldOff 0

versionRd :: ImplM Word8
versionRd = do
    o <- versionOff
    b <- readP8 o
    return (shift (b .&. 0xf0) (-4))


ihlOff :: ImplM Int
ihlOff = fieldOff 0

ihlRd :: ImplM Word8
ihlRd = do
    o <- ihlOff
    b <- readP8 o
    return (b .&. 0xf)


lengthOff :: ImplM Int
lengthOff = fieldOff 2

lengthRd :: ImplM Word16
lengthRd = lengthOff >>= readP16BE


identificationOff :: ImplM Int
identificationOff = fieldOff 4

identificationRd :: ImplM Word16
identificationRd = identificationOff >>= readP16BE


flagsOff :: ImplM Int
flagsOff = fieldOff 6

flagsRd :: ImplM Word8
flagsRd = do
    o <- flagsOff
    b <- readP8 o
    return (shift (b .&. 0xe0) (-5))


fragmentOff :: ImplM Int
fragmentOff = fieldOff 6

fragmentRd :: ImplM Word16
fragmentRd = do
    o <- fragmentOff
    b <- readP16BE o
    return (b .&. 0x1fff)


ttlOff :: ImplM Int
ttlOff = fieldOff 8

ttlRd :: ImplM Word8
ttlRd = ttlOff >>= readP8


protocolOff :: ImplM Int
protocolOff = fieldOff 9

protocolRd :: ImplM Word8
protocolRd = protocolOff >>= readP8


checksumOff :: ImplM Int
checksumOff = fieldOff 10

checksumRd :: ImplM Word16
checksumRd = checksumOff >>= readP16BE


sourceIPOff :: ImplM Int
sourceIPOff = fieldOff 12

sourceIPRd :: ImplM Word32
sourceIPRd = sourceIPOff >>= readP32BE


destIPOff :: ImplM Int
destIPOff = fieldOff 16

destIPRd :: ImplM Word32
destIPRd = destIPOff >>= readP32BE



flagsDF :: Word8
flagsDF = 0x2

-- TODO: Check if this is right (might be 0x1)
flagsMF :: Word8
flagsMF = 0x4



protocolICMP :: Word8
protocolICMP = 0x01

protocolTCP :: Word8
protocolTCP = 0x06

protocolUDP :: Word8
protocolUDP = 0x11





-- Kind of ugly: convert pairs of bytes to 16bit integers, but use 32bit
-- arithmetic when summing up, in the end combine the higher and lower 16 bits
checksum :: [Word8] -> Word16
checksum p = cxsm
    where
        padded = if (length p) `mod` 2 /= 0 then p ++ [0] else p
        convertSingle a b = fromIntegral (convert16BE a b) :: Word32
        convert [] = []
        convert (a:b:rest) = (convertSingle a b):(convert rest)
        convert _ = undefined
        s32 = sum $ convert padded
        foldInt i = ((i .&. 0xffff) + (shiftR i 16))
        cxsm32 = xor 0xffff $ foldInt $ foldInt s32
        cxsm = fromIntegral cxsm32 :: Word16

-- IPv4 pseudoheader
pseudoheader :: ImplM [Word8]
pseudoheader = do
    src <- sourceIPRd
    dst <- destIPRd
    proto <- protocolRd
    len <- payloadLen
    let l = fromIntegral len
    return (unpack32BE src ++ unpack32BE dst ++ [0,proto] ++ unpack16BE l)


-- Convert IP as string to 32-bit word
ipFromString :: String -> Maybe Word32
ipFromString s
    | length parts /= 4 = Nothing
    | otherwise = Just $ pack32BE (map read parts :: [Word8])
    where parts = splitBy '.' s :: [String]

