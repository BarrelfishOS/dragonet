-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.Implementation.ARP(
    htypeRd, ptypeRd, hlenRd, plenRd, operRd,
    shaRd, spaRd, thaRd, tpaRd,
    htypeWr, ptypeWr, hlenWr, plenWr, operWr,
    shaWr, spaWr, thaWr, tpaWr,

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

htypeWr :: Word16 -> ImplM ()
htypeWr v = htypeOff >>= writeP16BE v


ptypeOff :: ImplM Int
ptypeOff = fieldOff 2

ptypeRd :: ImplM Word16
ptypeRd = ptypeOff >>= readP16BE

ptypeWr :: Word16 -> ImplM ()
ptypeWr v = ptypeOff >>= writeP16BE v


hlenOff :: ImplM Int
hlenOff = fieldOff 4

hlenRd :: ImplM Word8
hlenRd = hlenOff >>= readP8

hlenWr :: Word8 -> ImplM ()
hlenWr v = hlenOff >>= writeP8 v


plenOff :: ImplM Int
plenOff = fieldOff 5

plenRd :: ImplM Word8
plenRd = plenOff >>= readP8

plenWr :: Word8 -> ImplM ()
plenWr v = plenOff >>= writeP8 v


operOff :: ImplM Int
operOff = fieldOff 6

operRd :: ImplM Word16
operRd = operOff >>= readP16BE

operWr :: Word16 -> ImplM ()
operWr v = operOff >>= writeP16BE v


shaOff :: ImplM Int
shaOff = fieldOff 8

shaRd :: ImplM [Word8]
shaRd = do
    o <- shaOff
    l <- hlenRd
    readP (fromIntegral l) o

shaWr :: [Word8] -> ImplM ()
shaWr v = shaOff >>= writeP v


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

spaWr :: [Word8] -> ImplM ()
spaWr v = spaOff >>= writeP v


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

thaWr :: [Word8] -> ImplM ()
thaWr v = thaOff >>= writeP v


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

tpaWr :: [Word8] -> ImplM ()
tpaWr v = tpaOff >>= writeP v


operRequest :: Word16
operRequest = 0x0001

operReply :: Word16
operReply = 0x0002


htypeEthernet :: Word16
htypeEthernet = 0x0001

ptypeIPV4 :: Word16
ptypeIPV4 = 0x0800


