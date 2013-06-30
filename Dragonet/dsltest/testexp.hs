#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import DragonetDSL
import qualified Operations as OP
import DotGenerator as DG

[dragonet|
node source {
  port out[l2EtherClassified] }

node packetDrop { }

node packetValid { }

boolean l2EtherClassified {
    port true[l2EtherValidUnicast l2EtherValidMulticast l2EtherValidBroadcast
              l2EtherValidSrc l2EtherValidType]
    port false[packetDrop] }


boolean l2EtherValidUnicast {
    port true false[l2EtherValidDest] }

boolean l2EtherValidMulticast {
    port true false[l2EtherValidDest] }

boolean l2EtherValidBroadcast {
    port true false[l2EtherValidDest] }

or l2EtherValidDest {
    port true false[l2EtherValid] }

boolean l2EtherValidSrc {
    port true false[l2EtherValid] }

and l2EtherValid {
    port true false[l3IPValid] }

boolean l2EtherValidType {
    port true[l3IPv4Classified l3IPv6Classified]
    port false[packetDrop] }




boolean l3IPv4Classified {
    port true[l3IPv4ValidDest l3IPv4ValidRessembly l3IPv4ValidVersion
              l3IPv4ValidLength l3IPv4ValidTTL l3IPv4ValidChecksum
              l3IPv4ValidSrc l3IPv4ValidProtocol]
    port false[packetDrop] }

boolean l3IPv4ValidDest {
    port true false[l3IPv4Valid] }

boolean l3IPv4ValidRessembly {
    port true false[l3IPv4Valid] }
   
boolean l3IPv4ValidVersion {
    port true false[l3IPv4Valid] }
   
boolean l3IPv4ValidLength {
    port true false[l3IPv4Valid] }
   
boolean l3IPv4ValidTTL {
    port true false[l3IPv4Valid] }
   
boolean l3IPv4ValidChecksum {
    port true false[l3IPv4Valid] }
   
boolean l3IPv4ValidSrc {
    port true false[l3IPv4Valid] }

boolean l3IPv4ValidProtocol {
    port true false[l3IPClassified] }
  
and l3IPv4Valid {
    port true false[l3IPValid] }




boolean l3IPv6Classified {
    port true[l3IPv6ValidDest l3IPv6ValidVersion l3IPv6ValidLength
              l3IPv6ValidHops l3IPv6ValidSrc l3IPv6ValidProtocol]
    port false[packetDrop] }

boolean l3IPv6ValidDest {
    port true false[l3IPv6Valid] }

boolean l3IPv6ValidVersion {
    port true false[l3IPv6Valid] }

boolean l3IPv6ValidLength {
    port true false[l3IPv6Valid] }

boolean l3IPv6ValidHops {
    port true false[l3IPv6Valid] }

boolean l3IPv6ValidSrc {
    port true false[l3IPv6Valid] }

boolean l3IPv6ValidProtocol {
    port true false[l3IPClassified] }

and l3IPv6Valid {
    port true false[l3IPValid] }




or l3IPClassified {
    port true[l4UDPClassified l4TCPClassified]
    port false[packetDrop] }

or l3IPValid {
    port true false[l4Valid] }




boolean l4UDPClassified {
    port true[l4UDPValidChecksum l4UDPValidSrc l4UDPValidDest l4UDPValidLength
              l4Classified]
    port false[packetDrop l4Classified] }

boolean l4UDPValidChecksum {
    port true false[l4UDPValid] }

boolean l4UDPValidSrc {
    port true false[l4UDPValid] }

boolean l4UDPValidDest {
    port true false[l4UDPValid] }

boolean l4UDPValidLength {
    port true false[l4UDPValid] }

and l4UDPValid {
    port true false[l4Valid] }




boolean l4TCPClassified {
    port true[l4TCPValidOffset l4TCPValidValidUrgent l4TCPValidWindow
              l4TCPValidFlags l4TCPValidFin l4TCPValidSyn l4TCPValidAck
              l4TCPValidAckNo l4TCPValidSequence l4TCPValidSrc l4TCPValidDest
              l4TCPValidLength l4TCPValidChecksum l4TCPValidState l4Classified]
    port false[packetDrop] }

boolean l4TCPValidOffset {
    port true false[l4TCPValid] }

boolean l4TCPValidValidUrgent {
    port true false[l4TCPValid] }

boolean l4TCPValidWindow {
    port true false[l4TCPValid] }

boolean l4TCPValidFlags {
    port true false[l4TCPValid] }

boolean l4TCPValidFin {
    port true false[l4TCPValid] }

boolean l4TCPValidSyn {
    port true false[l4TCPValid] }

boolean l4TCPValidAck {
    port true false[l4TCPValid] }

boolean l4TCPValidAckNo {
    port true false[l4TCPValid] }

boolean l4TCPValidSequence {
    port true false[l4TCPValid] }

boolean l4TCPValidSrc {
    port true false[l4TCPValid] }

boolean l4TCPValidDest {
    port true false[l4TCPValid] }

boolean l4TCPValidLength {
    port true false[l4TCPValid] }

boolean l4TCPValidChecksum {
    port true false[l4TCPValid] }

boolean l4TCPValidState {
    port true false[l4TCPValid] }

boolean l4TCPValid {
    port true false[l4Valid] }




or l4Classified {
    port true[toQueue0Core0]
    port false[packetDrop]}

or l4Valid {
    port true false[toDefaultKernelProcessing] }




boolean toQueue0Core0 {
    port true false[toDefaultKernelProcessing] }

and toDefaultKernelProcessing {
    port true[packetValid]
    port false[packetDrop] }

|]

main = do
    putStrLn (DG.toDot source)
