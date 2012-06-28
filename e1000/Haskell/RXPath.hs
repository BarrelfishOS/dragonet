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
import qualified Data.Bits as Bits
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
    srcAddr :: EtherAddr
    , dstAddr :: EtherAddr
    , pktLen  :: W.Word16
    , payload :: PacketData
    , pktCRC :: [W.Word8]
    , pktType :: Integer
    } deriving (Show, Eq)

-- Union of different valid packets
data MyPacket = EtherPkt EthernetPacket
            | InvalidPkt InvalidPacket
    deriving (Show, Eq)


processPacket :: UnknownPacket -> MyPacket
processPacket pkt = InvalidPkt $ InvalidPacket (packetData pkt) "test invalid"

-- Invalidate the given packet with given error message
invalidatePacket :: UnknownPacket -> String -> MyPacket
invalidatePacket pkt msg =
        InvalidPkt $ InvalidPacket (packetData pkt) msg

-- Checks if the packet has valid CRC checksum
invalidCRC :: UnknownPacket -> Bool
invalidCRC pkt = False -- FIXME: Actually calculate CRC checksum

-- Check if the given address is broadcast or not (ie: ff:ff:ff:ff:ff:ff )
isBroadcastPacket :: EtherAddr -> Bool
isBroadcastPacket ethAddr = ( ethAddr == EtherAddr (BS.replicate 6 0xff) )

-- Check if packet is multicast
-- ie: belongs to one of the group to which this machine belongs
isMulticastPacket :: EtherAddr -> Bool
isMulticastPacket ethAddr = False -- FIXME: actually check if packet is

-- Check if the given address is anycast or not (ie: belongs to this machine)
isAnycastPacket :: EtherAddr -> Bool
isAnycastPacket ethAddr = True

-- Check if packet is intented for this machine
invalidIncoming :: EthernetPacket -> Bool
invalidIncoming ethPkt
    | isBroadcastPacket $ dstAddr ethPkt = False
    | isMulticastPacket $ dstAddr ethPkt = False
    | isAnycastPacket $ dstAddr ethPkt = False
    | otherwise = True

-- gets specified part of the payload ([including start, excluding end])
-- index starts at zero
-- Number of elements returned = (end - start)
subList :: Int -> Int -> PacketData -> BS.ByteString
subList start end payload =
   BS.take (end - start) $ BS.drop start $ bytes payload


-- Convert [Word8] of size 2 into Word16
convert2Word16 :: [W.Word8] -> W.Word16
convert2Word16 (w1:w2:[]) = (Bits.shiftL (fromIntegral w1 :: W.Word16) 8)
                                + (fromIntegral w2 :: W.Word16)
convert2Word16 _ = error "incorrect length of array"

-- Convert into Ethernet packet
toEthernetPkt :: UnknownPacket -> EthernetPacket
toEthernetPkt pkt = let
    len = BS.length $ bytes $ packetData pkt in
    EthernetPacket {
    srcAddr = EtherAddr $ subList 0 6 $ packetData pkt --first 6 octets
    , dstAddr = EtherAddr $ subList 6 12 $ packetData pkt -- next 6 octets
    , pktLen = convert2Word16 $ BS.unpack $ subList 12 14 $ packetData pkt -- packet length
    , payload = PacketData $ subList 18 (len - 4) $ packetData pkt -- data octates
    , pktCRC = BS.unpack $ subList (len - 4) len $ packetData pkt -- last 4 octets
    , pktType = 0
    }

-- Checks if the packet has valid length
-- FIXME: put the actual values for MAX/MIN ethernet packet sizes
invalidLength :: UnknownPacket -> Bool
invalidLength pkt
    | len >= 1532 = True
    | len < 60 = True
    | otherwise = False
    where len = toInteger $ BS.length $ bytes $ packetData pkt

validatePacket :: UnknownPacket -> MyPacket
validatePacket pkt
    | invalidLength pkt = invalidatePacket pkt "invalid packet length"
    | invalidCRC pkt = invalidatePacket pkt "invalid CRC Checksum"
    | invalidIncoming ethPkt = invalidatePacket pkt
                                        "not intented for this machine"
    | otherwise = EtherPkt ethPkt
    where ethPkt = toEthernetPkt pkt

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: UnknownPacket
getNextPacket =
--   UnknownPacket $ PacketData $ BS.pack (replicate 64 (53 :: W.Word8))
   UnknownPacket $ PacketData $ BS.pack ([50..120] :: [W.Word8])

-- main function which prints the fate of the next packet
main = print $ validatePacket getNextPacket

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


