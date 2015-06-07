-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.


module Dragonet.Implementation.TCP(
    sourcePortRd, destPortRd,
    sourcePortWr, destPortWr,
    seqNoRd, seqNoWr,
    ackNoRd, ackNoWr,
    windowRd,  windowWr,
    checksumRd,  checksumWr,
    urgentRd,  urgentWr,
    flagsRd, flagsWr,
    dORd, dOWr,
    isSYN, isFIN, isACK, isRST, isPSH, isURG,
    setSYN, setFIN, setACK, setRST, setPSH, setURG,

    getLength,
    headerLen, headerLenMin,  headerOff, payloadOff, payloadLen,
    getPayload,

) where

import Dragonet.Implementation
import Data.Word
import Data.Bits

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L4Offset" ; return i }

getLength :: ImplM Int
getLength = do { (AttrI i) <- getAttr "L4PayloadLen" ; return i }

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }

-- generic read field
genOffset :: Int -> ImplM Int
genOffset v = fieldOff v

-- Generic version for reading and writing specific part
gen32Rd :: Int -> ImplM Word32
gen32Rd offset = (genOffset offset) >>= readP32BE

gen16Rd :: Int -> ImplM Word16
gen16Rd offset = (genOffset offset) >>= readP16BE

gen8Rd :: Int -> ImplM Word8
gen8Rd offset = (genOffset offset) >>= readP8



gen32Wr :: Int -> Word32 -> ImplM ()
gen32Wr offset p = (genOffset offset) >>= writeP32BE p

gen16Wr :: Int -> Word16 -> ImplM ()
gen16Wr offset p = (genOffset offset) >>= writeP16BE p

gen8Wr :: Int -> Word8 -> ImplM ()
gen8Wr offset p = (genOffset offset) >>= writeP8 p


-- source port manipulation
sourcePortRd = gen16Rd 0
sourcePortWr = gen16Wr 0

-- destination port manipulation
destPortRd = gen16Rd 2
destPortWr = gen16Wr 2

-- sequence number manuplation
seqNoRd =  gen32Rd 4
seqNoWr = gen32Wr 4

-- Ack number manipulation
ackNoRd = gen32Rd 8
ackNoWr = gen32Wr 8

-- window size manipulation
windowRd = gen16Rd 14
windowWr = gen16Wr 14

-- checksum manipulation
checksumRd = gen16Rd 16
checksumWr = gen16Wr 16

-- urgent ptr manipulation
urgentRd = gen16Rd 18
urgentWr = gen16Wr 18

-- data offset manipulation
dooff = 12

dORd2 = gen8Rd 12

dORd :: ImplM Word8
dORd =  do
    b <- gen8Rd dooff
    return (shift (b .&. 0xf0) (-4)) -- bin(0xf0) = 0b11110000

dOWr :: Word8 -> ImplM ()
dOWr v = do
--    orig <- dORd
    let o = dooff
        v' = shift (v .&. 0x0f) (4)
    b <- gen8Rd o
    let val = ((b .&. 0x0f) .|. v')
--    debug ("##### DO == " ++ (show val))
    writeP8 val  o -- bin(0x0f) = '0b00001111'

headerLenMin :: Int
headerLenMin = 20

-- Header lenth of RX packet which is currently under processing
headerLen :: ImplM Int
headerLen = do
    ihl <- dORd
    return ((fromIntegral ihl) * 4)

-- Payload len of RX packet
payloadLen :: ImplM Int
payloadLen = do
    h <- headerLen
    l <- getLength
    return ((fromIntegral l) - h)

-- Payload offset of received packet
payloadOff :: ImplM Int
payloadOff = do { i <- headerOff ; l <- headerLen ; return (i + l) }

getPayload :: ImplM ([Word8])
getPayload = do
    poff <- payloadOff
    plen <- payloadLen
    readPX plen poff


-- flags manipulation
flagsRd2 = gen8Rd 13

flagsOff = 13
flagsRd :: ImplM Word8
flagsRd =  do
    let o = flagsOff
    b <- gen8Rd o
    --return b
    return (b .&. 0x3f) -- 0x3f = 0b00111111

flagsWr :: Word8 -> ImplM ()
flagsWr v = do
    let o = flagsOff
    b <- gen8Rd o
    writeP8 ((b .&. 0xc0) .|. v) o -- bin(0xc0) = 0b11000000


-- Add payload in TX packet
{-addPayload :: BS.ByteString -> ImplM ()
addPayload payload = do
    let offset = headerLenMin
    insertData offset payload
-}

-- Detailed flags

isGenFlagSet :: Int -> ImplM Bool
isGenFlagSet bp = do
    flags <- flagsRd
    return (testBit flags bp)

setGenFlag :: Int -> ImplM ()
setGenFlag bp = do
    flags <- flagsRd
    flagsWr (setBit flags bp)

isFIN = isGenFlagSet 0
setFIN = setGenFlag 0

isSYN = isGenFlagSet 1
setSYN = setGenFlag 1

isRST = isGenFlagSet 2
setRST = setGenFlag 2

isPSH = isGenFlagSet 3
setPSH = setGenFlag 3

isACK = isGenFlagSet 4
setACK = setGenFlag 4

isURG = isGenFlagSet 5
setURG = setGenFlag 5



-- Detailed flags
--synBitPos = 1
--isSyn :: ImplM Bool
--isSyn = do
--    flags <- flagsRd
--    return (testBit flags synBitPos)
--
--setSyn :: ImplM ()
--setSyn = do
--    flags <- flagsRd
--    flagsWr (setBit flags synBitPos)
--
