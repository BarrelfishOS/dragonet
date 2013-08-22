module Dragonet.Implementation.Ethernet(
    destRd, sourceRd, etypeRd, checksumRd,
    etypeIPV4, etypeIPV6, etypeARP,
    macFromString,
) where

import Dragonet.Implementation
import Util.Misc
import Data.Word


headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L2Offset" ; return i }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }


destOff :: ImplM Int
destOff = fieldOff 0

destRd :: ImplM [Word8]
destRd = destOff >>= readP 6


sourceOff :: ImplM Int
sourceOff = fieldOff 6

sourceRd :: ImplM [Word8]
sourceRd = sourceOff >>= readP 6


etypeOff :: ImplM Int
etypeOff = fieldOff 12

etypeRd :: ImplM Word16
etypeRd = etypeOff >>= readP16BE


checksumOff :: ImplM Int
checksumOff = do { len <- packetLen ; return (len - 4) }

checksumRd :: ImplM Word32
checksumRd = checksumOff >>= readP32BE



etypeIPV4 :: Word16
etypeIPV4 = 0x0800

etypeIPV6 :: Word16
etypeIPV6 = 0x86DD

etypeARP :: Word16
etypeARP = 0x0806



-- Parse MAC address from String
macFromString :: String -> Maybe [Word8]
macFromString s
    | length parts /= 6 = Nothing
    | otherwise = Just $ (map read parts :: [Word8])
    where parts = splitBy ':' s :: [String]

