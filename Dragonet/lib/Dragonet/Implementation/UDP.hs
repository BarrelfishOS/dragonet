-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.Implementation.UDP(
    sourcePortRd, destPortRd, lengthRd, checksumRd,
    sourcePortWr, destPortWr, lengthWr, checksumWr,
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

sourcePortWr :: Word16 -> ImplM ()
sourcePortWr p = sourcePortOff >>= writeP16BE p


destPortOff :: ImplM Int
destPortOff = fieldOff 2

destPortRd :: ImplM Word16
destPortRd = destPortOff >>= readP16BE

destPortWr :: Word16 -> ImplM ()
destPortWr p = destPortOff >>= writeP16BE p



lengthOff :: ImplM Int
lengthOff = fieldOff 4

lengthRd :: ImplM Word16
lengthRd = lengthOff >>= readP16BE

lengthWr :: Word16 -> ImplM ()
lengthWr l = lengthOff >>= writeP16BE l



checksumOff :: ImplM Int
checksumOff = fieldOff 6

checksumRd :: ImplM Word16
checksumRd = checksumOff >>= readP16BE

checksumWr :: Word16 -> ImplM ()
checksumWr c = checksumOff >>= writeP16BE c


