module Dragonet.Implementation.Ethernet(
    destRd, sourceRd, etypeRd, checksumRd,
    destWr, sourceWr, etypeWr, checksumWr,

    etypeIPV4, etypeIPV6, etypeARP,
    headerLen, macFromString, macBroadcast,
) where

import Dragonet.Implementation
import Util.Misc
import Data.Word
import Data.Maybe

headerLen :: Int
headerLen = 14

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L2Offset" ; return i }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }


destOff :: ImplM Int
destOff = fieldOff 0

destRd :: ImplM [Word8]
destRd = destOff >>= readP 6

destWr :: [Word8] -> ImplM ()
destWr mac = if length mac /= 6 then error "Invalid MAC length"
                else destOff >>= writeP mac


sourceOff :: ImplM Int
sourceOff = fieldOff 6

sourceRd :: ImplM [Word8]
sourceRd = sourceOff >>= readP 6

sourceWr :: [Word8] -> ImplM ()
sourceWr mac = if length mac /= 6 then error "Invalid MAC length"
                else sourceOff >>= writeP mac


etypeOff :: ImplM Int
etypeOff = fieldOff 12

etypeRd :: ImplM Word16
etypeRd = etypeOff >>= readP16BE

etypeWr :: Word16 -> ImplM ()
etypeWr v = etypeOff >>= writeP16BE v


checksumOff :: ImplM Int
checksumOff = do { len <- packetLen ; return (len - 4) }

checksumRd :: ImplM Word32
checksumRd = checksumOff >>= readP32BE

checksumWr :: Word32 -> ImplM ()
checksumWr v = checksumOff >>= writeP32BE v



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
    | otherwise = Just $ (map parsePart parts :: [Word8])
    where
        parts = splitBy ':' s :: [String]
        parsePart p = read ("0x" ++ p)

macBroadcast :: [Word8]
macBroadcast = fromJust $ macFromString "ff:ff:ff:ff:ff:ff"


