module Dragonet.Implementation.UDP(
    sourcePortRd, destPortRd, lengthRd, checksumRd,
    headerLen, headerOff, payloadOff, payloadLen,
) where

import Dragonet.Implementation
import Data.Word


headerLen :: Int
headerLen = 8

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L4Offset" ; return i }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }

payloadLen :: ImplM Int
payloadLen = do { l <- lengthRd ; return (fromIntegral l) }

payloadOff :: ImplM Int
payloadOff = do { o <- headerOff ; return (o + headerLen) }



sourcePortOff :: ImplM Int
sourcePortOff = fieldOff 0

sourcePortRd :: ImplM Word16
sourcePortRd = sourcePortOff >>= readP16BE


destPortOff :: ImplM Int
destPortOff = fieldOff 2

destPortRd :: ImplM Word16
destPortRd = destPortOff >>= readP16BE


lengthOff :: ImplM Int
lengthOff = fieldOff 4

lengthRd :: ImplM Word16
lengthRd = lengthOff >>= readP16BE


checksumOff :: ImplM Int
checksumOff = fieldOff 6

checksumRd :: ImplM Word16
checksumRd = checksumOff >>= readP16BE

