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
data IPAddr = IPAddr BS.ByteString deriving (Show, Eq)

-- Contents of packet
data PacketData = PacketData {
        bytes :: BS.ByteString
        } deriving (Show, Eq)

-- unclassified/unprocessed packet
data UnknownPacket = UnknownPacket {
    packetData :: PacketData
    } deriving (Show, Eq)

-- #################### L2 packet ####################
-- Ethernet packet
data EthernetPacket = EthernetPacket {
    srcAddr :: EtherAddr
    , dstAddr :: EtherAddr
    , pktLen  :: W.Word16
    , payload :: PacketData
    , pktCRC :: [W.Word8]
    , originalPkt :: UnknownPacket
    } deriving (Show, Eq)

-- Generic L2 packet type with tags for types
data L2Packet = BroadcastPkt EthernetPacket
              | UnicastPkt EthernetPacket
              | MulticastPkt EthernetPacket
              | InvalidPktL2 {
                    invalidPktL2 :: EthernetPacket
                    , reasonL2 :: String
                    }
    deriving (Show, Eq)

-- #################### L3 packet ####################
-- IPv4Packet
data IPv4Packet = IPv4Packet {
    srcIP :: IPAddr
    , dstIP :: IPAddr
    , protocol :: W.Word8
    , headerChecksum :: [W.Word8]
    , originalL2 :: L2Packet
    } deriving (Show, Eq)

-- L3 packet type
data L3Packet = IPv4Pkt L2Packet
              | IPv6Pkt L2Packet
              | InvalidPktL3 {
                invalidPktL3 :: L2Packet
                , reasonL3 :: String
                }
    deriving (Show, Eq)

-- #################### Ethernet packet parsing ####################

-- gets specified part of the payload ([including start, excluding end])
-- index starts at zero
-- Number of elements returned = (end - start)
subList :: Int -> Int -> PacketData -> BS.ByteString
subList start end payload =
   BS.take (end - start) $ BS.drop start $ bytes payload

-- Convert [Word8] of size 2 into Word16
convert2Word32 :: [W.Word8] -> W.Word32
convert2Word32 (w1:w2:w3:w4:[]) =
    (Bits.shiftL (fromIntegral w1 :: W.Word32) 24)
    + (Bits.shiftL (fromIntegral w2 :: W.Word32) 16)
    + (Bits.shiftL (fromIntegral w3 :: W.Word32) 8)
    + (fromIntegral w4 :: W.Word32)
convert2Word32 _ = error "incorrect length of array"


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
        --srcAddr = first 6 octets
        srcAddr = EtherAddr $ subList 0 6 $ packetData pkt
        -- dstAddr = next 6 octets
        , dstAddr = EtherAddr $ subList 6 12 $ packetData pkt
        -- packet length
        , pktLen =convert2Word16 $ BS.unpack $ subList 12 14 $ packetData pkt
        -- data octates
        , payload = PacketData $ subList 18 (len - 4) $ packetData pkt
        -- last 4 octets
        , pktCRC = BS.unpack $ subList (len - 4) len $ packetData pkt
        -- Original packet (for reference purposes)
        , originalPkt = pkt
    }


-- #################### Ethernet packet validation ####################
--
-- Checks if the packet has valid CRC checksum
invalidCRC :: EthernetPacket -> Bool
invalidCRC pkt = False -- FIXME: Actually calculate CRC checksum

-- Checks if the packet has valid length
-- FIXME: put the actual values for MAX/MIN ethernet packet sizes
-- FIXME: Use length from Ethernet header instead of array length
invalidLength :: EthernetPacket -> Bool
invalidLength pkt
    | len >= 1532 = True
    | len < 60 = True
    | otherwise = False
    where len = toInteger $ BS.length $ bytes $ packetData $ originalPkt pkt


-- Check if the given address is broadcast or not (ie: ff:ff:ff:ff:ff:ff )
isBroadcastPacket :: EtherAddr -> Bool
isBroadcastPacket ethAddr = ( ethAddr == EtherAddr (BS.replicate 6 0xff) )

-- Check if packet is multicast
-- ie: belongs to one of the group to which this machine belongs
isMulticastPacket :: EtherAddr -> Bool
isMulticastPacket ethAddr = False -- FIXME: actually check if packet is

-- Classify L2 packet into Uni/Multi/Broadcast type
toL2Packet :: EthernetPacket -> L2Packet
toL2Packet ethPkt
    | isBroadcastPacket $ dstAddr ethPkt = BroadcastPkt ethPkt
    | isMulticastPacket $ dstAddr ethPkt = MulticastPkt ethPkt
    | otherwise = UnicastPkt ethPkt

-- Check if the given address belongs to this machine
isMyUnicastAddr :: EtherAddr -> Bool
isMyUnicastAddr ethAddr =
        -- FIXME: Check given address in list of NIC MAC addresses
        True

-- Check if the given address belongs to this machine
isMyMulticastGroupAddr :: EtherAddr -> Bool
isMyMulticastGroupAddr ethAddr =
        -- FIXME: Check given address in list of groups this NIC is member of
        True


-- Classified L2 packet will be checked if it belongs to this machine or not
validL2Destination :: L2Packet -> Bool
validL2Destination (BroadcastPkt ethPkt) = True
validL2Destination (MulticastPkt ethPkt) =
                                    isMyMulticastGroupAddr $ dstAddr ethPkt
validL2Destination (UnicastPkt ethPkt) = isMyUnicastAddr $ dstAddr ethPkt
validL2Destination (InvalidPktL2 _ _) = False
    --  error "should not happen!!"
    -- As packet is not validated yet, it can't be marked as invalid

-- Validate given Ethernet packet and convert it into L2Packet for
-- further processing
validateL2Packet :: EthernetPacket -> L2Packet
validateL2Packet ethPkt
    | invalidLength ethPkt = InvalidPktL2 { invalidPktL2 = ethPkt
                             , reasonL2 = "invalid packet length" }
    | invalidCRC ethPkt = InvalidPktL2 { invalidPktL2 = ethPkt
                             , reasonL2 = "invalid CRC Checksum" }
    | not $ validL2Destination l2Pkt = InvalidPktL2 { invalidPktL2 = ethPkt
                             , reasonL2 = "not intented for this machine"}
    | otherwise = l2Pkt
    where l2Pkt = toL2Packet ethPkt


-- Get payload from L2 packet
getL2Payload :: L2Packet -> PacketData
getL2Payload (BroadcastPkt ethPkt) = payload ethPkt
getL2Payload (UnicastPkt ethPkt) = payload ethPkt
getL2Payload (MulticastPkt ethPkt) = payload ethPkt
getL2Payload (InvalidPktL2 ethPkt _) =
                    error "invalid L2 packet in L3 processing"

{-
-- Check if given protocol is IPv4
isIPv4 :: PacketData -> Bool
isIPv4 l2payload =
    convert2Word32 $ subList 0 4 l2payload

-- #################### L3 packet parsing ####################
toL3Layer :: L2Packet -> L3Packet
toL3Layer l2Pkt
    | isIPv4 l2Payload
    where l2Payload = getL2Payload l2Pkt
-}
-- #################### Packet generator ####################

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: UnknownPacket
getNextPacket =
--   UnknownPacket $ PacketData $ BS.pack (replicate 64 (53 :: W.Word8))
   UnknownPacket $ PacketData $ BS.pack ([50..120] :: [W.Word8])

-- #################### Main module ####################

-- main function which prints the fate of the next packet
main = print $ validateL2Packet $ toEthernetPkt getNextPacket

-- #################### EOF ####################

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


