module Dragonet.Implementation.ICMP(
    typeRd, codeRd, checksumRd, miscRd,
    typeWr, codeWr, checksumWr, miscWr,

    headerLen, headerOff, payloadLen, payloadOff,
    typeEchoRequest, typeEchoReply,
) where

import Dragonet.Implementation
import Data.Word

headerLen :: Int
headerLen = 8

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L4Offset" ; return i }

payloadOff :: ImplM Int
payloadOff = fieldOff 8

payloadLen :: ImplM Int
payloadLen = do { len <- packetLen ; off <- payloadOff ; return (len - off) }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }



typeOff :: ImplM Int
typeOff = fieldOff 0

typeRd :: ImplM Word8
typeRd = typeOff >>= readP8

typeWr :: Word8 -> ImplM ()
typeWr v = typeOff >>= writeP8 v


codeOff :: ImplM Int
codeOff = fieldOff 1

codeRd :: ImplM Word8
codeRd = codeOff >>= readP8

codeWr :: Word8 -> ImplM ()
codeWr v = codeOff >>= writeP8 v


checksumOff :: ImplM Int
checksumOff = fieldOff 2

checksumRd :: ImplM Word16
checksumRd = checksumOff >>= readP16BE

checksumWr :: Word16 -> ImplM ()
checksumWr v = checksumOff >>= writeP16BE v


miscOff :: ImplM Int
miscOff = fieldOff 4

miscRd :: ImplM Word32
miscRd = miscOff >>= readP32BE

miscWr :: Word32 -> ImplM ()
miscWr v = miscOff >>= writeP32BE v


typeEchoRequest :: Word8
typeEchoRequest = 0x08

typeEchoReply :: Word8
typeEchoReply = 0x00

