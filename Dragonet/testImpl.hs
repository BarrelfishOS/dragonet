#!/usr/bin/env runhaskell

import Dragonet.ProtocolGraph
import Dragonet.DotGenerator
import Dragonet.Implementation
import Dragonet.Implementation.Algorithm

import qualified Data.ByteString as BS

import LPGImpl


main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    writeFile "lpg.dot" $ toDotClustered lpgT lpgClusters

    putStrLn "DNS Response (udp checksum good)"
    dnsResp <- BS.readFile "packets/dns_response"
    let st = execute lpg dnsResp emptyGS
    putStrLn $ unlines $ gsDebug st
    where
        lpgT = pgSetType GTLpg lpg

