{-

Models the processing performed by e1000 RX path on incoming packet.

Aim:
  --Input: Incoming packet
  --        Configuration
  --Output:
  --    Packet in classified queue
  --    Notification of packet generation
  --    Status of packet in descriptor (TODO)
-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.Maybe

-- Ethernet address
data EtherAddr = EtherAddr BS.ByteString deriving (Show, Eq)

-- Contents of packet
data PacketData = PacketData {
        bytes :: BS.ByteString
        } deriving (Show, Eq)

-- unclassified/unprocessed packet
data UnknownPacket = UnknownPacket {
    packetData :: PacketData
    } deriving (Show, Eq)

-- Invalid packet with reason for invalidation
data InvalidPacket = InvalidPacket {
    invalidData :: PacketData
    , reason :: String
    } deriving (Show, Eq)

-- Ethernet packet
data EthernetPacket = EthernetPacket {
    etherData :: PacketData
    , srcAddr :: EtherAddr
    , dstAddr :: EtherAddr
    , pktLen  :: Integer
    , pktType :: Integer
    } deriving (Show, Eq)


getNextPacket :: UnknownPacket
-- The packet holding bytes
-- FIXME: Stupid packet, make it more realasitic
getNextPacket =
   UnknownPacket $ PacketData $ BS.pack (replicate 64 (53 :: W.Word8))

main = print $ getNextPacket

{-

Is it a valid packet?
Which queue it belongs?
Which CPU will be interrupted?
What is the state of the system?  (packet count, status, etc)

Combined data type,
-}

{-

type Packet = [Integer]
type Hash = Integer
type QueueID = Integer
type Index = Int
type CoreID = Integer
type RedirectTbl = [(QueueID, CoreID)]

-- TCP Hashing function
-- Currently, it just adds all bytes to get the hash
-- FIXME: Dummy hash function
hashTCP :: Packet -> Hash
hashTCP [] = 0
hashTCP (x:xs) = x + hashTCP xs

-- Select proper hash function based on the packet classification
-- Currently only TCP is supported
-- Need to add more
selectHashFunction :: Packet -> Maybe (Packet -> Hash)
selectHashFunction _ = Just hashTCP

-- Get the packet hash by applying proper hashing function for given packet
hashPacket :: Packet -> Hash
hashPacket p =
    let hashFunction = selectHashFunction p in
    case hashFunction of
        Nothing -> 0
        Just fn -> fn p

-- Given hash, convert into an index for Redirection Table
-- This function will use last 5 bits of the hash
hashToIndex :: Hash -> Index
hashToIndex hash = fromIntegral (hash `mod` 127)

-- Returns a default entry in redirection table
lookupDefaultEntry :: RedirectTbl -> QueueID
lookupDefaultEntry redirectionTable =
        toInteger ( fst (redirectionTable!!(hashToIndex 0)))

-- For given lookup-table and hash, give the QueueID to which packet should go
lookupRedirectionTable :: RedirectTbl -> Hash -> QueueID
lookupRedirectionTable redirectionTable hash_value =
        toInteger ( fst (redirectionTable!!(hashToIndex hash_value)))

-- For given configuration, redirection-table and packet,
--  find out the queueId which should get the packet
classifyPacket :: Map.Map [Char] Bool -> RedirectTbl -> Packet -> QueueID
classifyPacket conf_map rdt p =
    let conf = Map.lookup "enableMultiQueue" conf_map in
    case conf of
        Nothing -> lookupDefaultEntry rdt
        Just a -> lookupRedirectionTable rdt (hashPacket p)

-- A test function to apply the algorithm on dummy packet
handlePacket :: Integer
handlePacket =
    let mm = Map.fromList([("enableChecksum", False),
                        ("enableMultiQueue", True)])

    -- The packet holding bytes
    -- FIXME: Sample values
        pkt = [1..64]

    -- Table specifying which hash should go to which (queue, core)
    -- FIXME: Sample values
        redirectionTable = replicate 127 (1, 1)
    in classifyPacket mm redirectionTable pkt

main = print $ handlePacket

-}


