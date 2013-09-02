module Dragonet.Implementation.ARP(
    htypeRd, ptypeRd, hlenRd, plenRd, operRd,
    shaRd, spaRd, thaRd, tpaRd,
    headerMinLen, headerLen, headerOff, allocLen,
    operRequest, operReply, htypeEthernet, ptypeIPV4,
) where

import Dragonet.Implementation
import Data.Word

headerMinLen :: Int
headerMinLen = 8

headerLen :: ImplM Int
headerLen = do { hlen <- hlenRd ; plen <- plenRd ; return $ allocLen hlen plen }

headerOff :: ImplM Int
headerOff = do { (AttrI i) <- getAttr "L3Offset" ; return i }

allocLen :: Word8 -> Word8 -> Int
allocLen hlen plen = headerMinLen + 2 * (fromIntegral hlen + fromIntegral plen)

fieldOff :: Int -> ImplM Int
fieldOff o = do { i <- headerOff ; return (i + o) }


htypeOff :: ImplM Int
htypeOff = fieldOff 0

htypeRd :: ImplM Word16
htypeRd = htypeOff >>= readP16BE


ptypeOff :: ImplM Int
ptypeOff = fieldOff 2

ptypeRd :: ImplM Word16
ptypeRd = ptypeOff >>= readP16BE


hlenOff :: ImplM Int
hlenOff = fieldOff 4

hlenRd :: ImplM Word8
hlenRd = hlenOff >>= readP8


plenOff :: ImplM Int
plenOff = fieldOff 5

plenRd :: ImplM Word8
plenRd = plenOff >>= readP8


operOff :: ImplM Int
operOff = fieldOff 6

operRd :: ImplM Word16
operRd = operOff >>= readP16BE


shaOff :: ImplM Int
shaOff = fieldOff 8

shaRd :: ImplM [Word8]
shaRd = do
    o <- shaOff
    l <- hlenRd
    readP (fromIntegral l) o


spaOff :: ImplM Int
spaOff = do
    sha <- shaOff
    hlen <- hlenRd
    return (sha + (fromIntegral hlen))

spaRd :: ImplM [Word8]
spaRd = do
    o <- spaOff
    l <- plenRd
    readP (fromIntegral l) o


thaOff :: ImplM Int
thaOff = do
    spa <- spaOff
    plen <- plenRd
    return (spa + (fromIntegral plen))

thaRd :: ImplM [Word8]
thaRd = do
    o <- thaOff
    l <- hlenRd
    readP (fromIntegral l) o


tpaOff :: ImplM Int
tpaOff = do
    tha <- thaOff
    hlen <- hlenRd
    return (tha + (fromIntegral hlen))

tpaRd :: ImplM [Word8]
tpaRd = do
    o <- tpaOff
    l <- plenRd
    readP (fromIntegral l) o


operRequest :: Word16
operRequest = 0x0001

operReply :: Word16
operReply = 0x0002


htypeEthernet :: Word16
htypeEthernet = 0x0001

ptypeIPV4 :: Word16
ptypeIPV4 = 0x0800


