#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

--module Conditions (
module Main (
    main
) where


import qualified Data.Data as DD

-- List of all the tests/computations which can happen on incoming packets
-- presence of these tags in any module will show that the module is capable of
-- performing this perticular computation
data Tests = L2ValidLen
        | L2ValidType
        | L2ValidCRC
        | L2ValidDest
        | L2ValidSrc
        | L3IPv4ValidVersion
        | L3IPv4ValidLength
        | L3IPv4ValidTTL
        | L3IPv4ValidProtocol -- clasifies the next level protocol
        | L3IPv4ValidChecksum
        | L3IPv4ValidSrc
        | L3IPv4ValidDest
        | L3IPv4ValidReassembly
        | L3IPv6ValidVersion
        | L3IPv6ValidLength
        | L3IPv6ValidHops
        | L3IPv6ValidSrc
        | L3IPv6ValidDest
        | L3IPv6ValidProtocol -- clasifies the next level protocol
        | L4UDPValidSrc
        | L4UDPValidDest
        | L4UDPValidLength
        | L4UDPValidChecksum
        | L4TCPValidSrc
        | L4TCPValidDest
        | L4TCPValidLength
        | L4TCPValidChecksum
        | L4TCPValidSequence
        | L4TCPValidAckNo
        | L4TCPValidAck
        | L4TCPValidSyn
        | L4TCPValidFin
        | L4TCPValidFlags
        | L4TCPValidWindow
        | L4TCPValidUrgent
        | L4TCPValidOffset
        | L4TCPValidState
--        | IsFlow Proto SrcIP DstIP SrcPort DstPort -- for flow filtering
        | ToKernelMemory -- packet copy to kernel memory
        | ToUserMemory -- packet copy to user memory
        deriving (Show, Eq, Ord, DD.Typeable, DD.Data)


data Module = Module {
        name :: String
        , tests :: [Tests]
    } deriving (Show, Eq, Ord, DD.Typeable, DD.Data)


-- main function (just for testing purposes)
main :: IO()
main = do
        putStrLn out1
        putStrLn lineBreak
        putStrLn out2
    where
        lineBreak = "\n\n"
        -- m = L4TCPValidAckNo
        m = Module "Ethernet" [L2ValidLen, L2ValidCRC]
        out1 = show m
        out2 = show $ DD.typeOf (m)
