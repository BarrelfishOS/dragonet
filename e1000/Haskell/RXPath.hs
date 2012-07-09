{-

Model for processing performed by e1000 RX path on incoming packet.

Aim:
  --Input: Incoming packet
  --       Configuration
  --
  --Output:
  --    Classified packet
  --    QueueID where packet will go
  --    CoreID where notification will be sent
-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.Bits as Bits
import qualified NICState as NS

-- Ethernet address
data EtherAddr = EtherAddr BS.ByteString deriving (Show, Eq)
data IPAddr = IPAddr BS.ByteString deriving (Show, Eq)
data PortNo = PortNo W.Word16 deriving (Show, Eq)

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
-- For IPv4 protocol
data IPv4Packet = IPv4Packet {
    srcIPv4 :: IPAddr
    , dstIPv4 :: IPAddr
    , protocolv4 :: W.Word8
    , headerChecksumv4 :: [W.Word8]
    , payloadL3v4 :: PacketData
    , originalL2v4 :: L2Packet
    } deriving (Show, Eq)

-- For IPv6 protocol
data IPv6Packet = IPv6Packet {
    srcIPv6 :: IPAddr
    , dstIPv6 :: IPAddr
    , protocolv6 :: W.Word8
    , payloadL3v6 :: PacketData
    , originalL2v6 :: L2Packet
    } deriving (Show, Eq)

-- L3 packet type which groups together ipv4, ipv6 and invalid packets
data L3Packet = IPv4Pkt IPv4Packet
              | IPv6Pkt IPv6Packet
              | InvalidPktL3 {
                invalidPktL3 :: L2Packet
                , reasonL3 :: String
                }
    deriving (Show, Eq)

-- #################### L4 packet ####################

-- Unreliable datagram protocol (UDP)
data UDPPacket =  UDPPacket {
    srcPortUDP :: PortNo
    , dstPortUDP :: PortNo
    , checksumUDP :: [W.Word8]
    , payloadUDP :: PacketData
    , originalL3UDP :: L3Packet
    } deriving (Show, Eq)

-- Reliable streaming protocol (TCP)
data TCPPacket =  TCPPacket {
    srcPortTCP :: PortNo
    , dstPortTCP :: PortNo
    , flagsTCP :: W.Word8
    , checksumTCP :: [W.Word8]
    , payloadTCP :: PacketData
    , originalL3TCP :: L3Packet
    } deriving (Show, Eq)

-- Other protocols
data OtherL4Packet = OtherL4Packet {
    payloadOtherL4 :: PacketData
    , originalL3OtherL4 :: L3Packet
    } deriving (Show, Eq)


-- L4 packet type which groups together TCP, UDP and Other protocols
data L4BasePacket = UDPPkt UDPPacket
            | TCPPkt TCPPacket
            | OtherL4Pkt OtherL4Packet
    deriving (Show, Eq)


-- L4 packet recording invalid packets
data L4Packet =  L4BasePkt L4BasePacket
            | InvalidPktL4 {
                invalidPktL4 :: L4BasePacket
                , reasonL4 :: String }
        deriving (Show, Eq)


-- #################### Redirection table ####################

type QueueID = Integer  -- ID for the hardware queue
type CoreID = Integer  -- ID for the CPU which can accept notification

-- Mechanism to convert hash into index for redirection Table.
type Hash = Integer -- Hash of the packet

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

-- #################### L3 packet parsing ####################

-- Get the version of L3 protocol used
getIPVersion :: PacketData -> W.Word8
getIPVersion l2payload =
                Bits.shiftR ((BS.unpack $ bytes l2payload)!!0) 4

toL3Layer :: L2Packet -> L3Packet
toL3Layer l2Pkt =
    case (getIPVersion l2Payload) of
        4 -> IPv4Pkt IPv4Packet {
                srcIPv4 = IPAddr $ subList 12 16 $ l2Payload
                , dstIPv4 = IPAddr $ subList 16 20 $ l2Payload
                , protocolv4 = (BS.unpack $ bytes l2Payload) !! 9
                , headerChecksumv4 =
                            BS.unpack $ subList 10 12 $ l2Payload
                , payloadL3v4 = PacketData $ BS.drop 20
                                    $ bytes l2Payload
                , originalL2v4 = l2Pkt
            }
        6 -> IPv6Pkt IPv6Packet {
                srcIPv6 = IPAddr $ subList 12 16 $ l2Payload
                , dstIPv6 = IPAddr $ subList 16 20 $ l2Payload
                , protocolv6 = (BS.unpack $ bytes l2Payload) !! 9
                , payloadL3v6 = PacketData $ BS.drop 20 $ bytes l2Payload
                -- FIXME: Find out proper value for IPv6 payload start
                , originalL2v6 = l2Pkt
            }
        _ -> InvalidPktL3 {
                invalidPktL3 = l2Pkt
                , reasonL3 = "Invalid L3 protocol type"
            }
    where l2Payload = getL2Payload l2Pkt

-- Validates the checksum of IPv4 packet
-- FIXME: Currently it assumes that checksum is correct
checksumIPv4 :: IPv4Packet -> Bool
checksumIPv4 ipv4Pkt = True

-- invalidates the packet if the IPv4 checksum is wrong
ipv4ValidateChecksum :: IPv4Packet -> L3Packet
ipv4ValidateChecksum ipv4Pkt =
    case (checksumIPv4 ipv4Pkt) of
        True -> IPv4Pkt ipv4Pkt
        False -> InvalidPktL3 {
                    invalidPktL3 = originalL2v4 ipv4Pkt
                    , reasonL3 = "Invalid checksum"
                }


-- Validate "something/everything" about IPv6 packet
-- FIXME: Currently it assumes that packet is correct
checkSomethingIPv6 :: IPv6Packet -> Bool
checkSomethingIPv6 ipv6Pkt = True

-- invalidates the packet if something is wrong with IPv6 packet
ipv6ValidateSomething :: IPv6Packet -> L3Packet
ipv6ValidateSomething ipv6Pkt =
    case (checkSomethingIPv6 ipv6Pkt) of
        True -> IPv6Pkt ipv6Pkt
        False -> InvalidPktL3 {
                    invalidPktL3 = originalL2v6 ipv6Pkt
                    , reasonL3 = "Something is invalid!"
                }

-- validate L3 level of packet
validateL3Packet :: L3Packet -> L3Packet
validateL3Packet (IPv4Pkt ipv4Pkt) = ipv4ValidateChecksum ipv4Pkt
validateL3Packet (IPv6Pkt ipv6Pkt) = ipv6ValidateSomething ipv6Pkt
validateL3Packet (InvalidPktL3 invalidpktl3 reasonl3) = InvalidPktL3 {
        invalidPktL3 = invalidpktl3
        , reasonL3 = reasonl3
        }


-- Get payload from L3 packet
getL3Payload :: L3Packet -> PacketData
getL3Payload (IPv4Pkt ipv4Pkt) = payloadL3v4 ipv4Pkt
getL3Payload (IPv6Pkt ipv6Pkt) = payloadL3v6 ipv6Pkt
getL3Payload (InvalidPktL3 _ _) =
                    error "invalid L3 packet in L4 processing"

-- Get L4 protocol from L3 packet
getL4Protocol :: L3Packet -> W.Word8
getL4Protocol (IPv4Pkt ipv4Pkt) = protocolv4 ipv4Pkt
getL4Protocol (IPv6Pkt ipv6Pkt) = protocolv6 ipv6Pkt
getL4Protocol (InvalidPktL3 _ _) =
                    error "invalid L3 packet in L4 processing"


-- #################### L4 packet parsing ####################

-- Convert given L3 packet into L4 packet
toL4Layer :: L3Packet -> L4BasePacket
toL4Layer l3Pkt =
    case (getL4Protocol l3Pkt) of
        -- UDP protocol
        77 -> UDPPkt UDPPacket {
                srcPortUDP = PortNo $ convert2Word16 $ BS.unpack
                    $ subList 0 2 l3Payload
                , dstPortUDP = PortNo $ convert2Word16 $ BS.unpack
                    $ subList 2 4 l3Payload
                , checksumUDP = BS.unpack $ subList 6 8 l3Payload
                , payloadUDP = PacketData $ BS.drop 8
                                    $ bytes l3Payload
                , originalL3UDP = l3Pkt
            }
        -- TCP protocol
        78 -> TCPPkt TCPPacket {
                srcPortTCP = PortNo $ convert2Word16 $ BS.unpack
                    $ subList 0 2 $ l3Payload
                , dstPortTCP = PortNo $ convert2Word16 $ BS.unpack
                    $ subList 2 4 $ l3Payload
                , flagsTCP = (BS.unpack $ subList 13 14 l3Payload)!!1
                , checksumTCP = BS.unpack $ subList 16 18 l3Payload
                , payloadTCP = PacketData $ BS.drop 20
                                    $ bytes l3Payload
                , originalL3TCP = l3Pkt
            }

        -- Other protocols
        _ -> OtherL4Pkt OtherL4Packet {
                payloadOtherL4 = PacketData $ bytes l3Payload
                , originalL3OtherL4 = l3Pkt
            }
    where l3Payload = getL3Payload l3Pkt


-- ------------------------ UDP protocol --------------------

-- Validates the checksum of UDP packet
-- FIXME: Currently it assumes that checksum is correct
checkUDPChecksum :: UDPPacket -> Bool
checkUDPChecksum udpPkt = True

-- invalidates the packet if the UDP checksum is wrong
udpValidateChecksum :: UDPPacket -> L4Packet
udpValidateChecksum udpPkt =
    case (checkUDPChecksum udpPkt) of
        True ->  L4BasePkt $ UDPPkt udpPkt
        False -> InvalidPktL4 {
                    invalidPktL4 = UDPPkt udpPkt
                    , reasonL4 = "Invalid UDP checksum"
                }


-- Validate UDP packet
-- Currently only one test is there, but more can be easily added
validateUDPPacket :: UDPPacket -> L4Packet
validateUDPPacket udpPkt = udpValidateChecksum udpPkt

-- ------------------------ TCP protocol --------------------

-- Validates the checksum of TCP packet
-- FIXME: Currently it assumes that checksum is correct
checkTCPChecksum :: TCPPacket -> Bool
checkTCPChecksum tcpPkt = True

-- invalidates the packet if the TCP checksum is wrong
tcpValidateChecksum :: TCPPacket -> L4Packet
tcpValidateChecksum tcpPkt =
    case (checkTCPChecksum tcpPkt) of
        True ->  L4BasePkt $ TCPPkt tcpPkt
        False -> InvalidPktL4 {
                    invalidPktL4 = TCPPkt tcpPkt
                    , reasonL4 = "Invalid TCP checksum"
                }


-- Validate TCP packet
-- Currently only one test is there, but more can be easily added
validateTCPPacket :: TCPPacket -> L4Packet
validateTCPPacket tcpPkt = tcpValidateChecksum tcpPkt

-- ------------------------ Other L4 level protocols ---------------

-- Validate packets of other protocols
-- Currently there are no tests, but they can be easily added.
validateOtherL4Packet :: OtherL4Packet -> L4Packet
validateOtherL4Packet otherPkt = L4BasePkt $ OtherL4Pkt otherPkt

-- Validate the given L4Base packet
validateL4Packet :: L4BasePacket -> L4Packet
validateL4Packet (UDPPkt udpPkt) = validateUDPPacket udpPkt
validateL4Packet (TCPPkt tcpPkt) = validateTCPPacket tcpPkt
validateL4Packet (OtherL4Pkt otherPkt) = validateOtherL4Packet otherPkt

-- #################### Packet validation ####################

-- Validate given packet
validatePacket ::NS.NICState -> UnknownPacket -> (NS.NICState, L4Packet)
validatePacket nicState pkt = (ns, l4Pkt) where
                l4Pkt = validateL4Packet $ toL4Layer
                        $ validateL3Packet $ toL3Layer
                        $ validateL2Packet $ toEthernetPkt pkt
                ns = NS.incrementPacketCount nicState

-- #################### Packet classification ####################

-- logic for hashing the byte array
-- Currently, it just adds all bytes to get the hash
-- FIXME: Dummy hash function
hashSum :: [W.Word8] -> Hash
hashSum [] = 0
hashSum (x:xs) = (fromIntegral x) + (hashSum xs)

-- hash the given packet data
hashPacketData :: PacketData -> Hash
hashPacketData pktData = hashSum $ BS.unpack $ bytes pktData

-- Hashes UDP packet
hashUDPPacket :: UDPPacket -> Hash
hashUDPPacket udpPkt = hashPacketData $ payloadUDP udpPkt

-- Hashes TCP packet
hashTCPPacket :: TCPPacket -> Hash
hashTCPPacket tcpPkt = hashPacketData $ payloadTCP tcpPkt

-- Hashes other types of packets
hashOtherL4Packet :: OtherL4Packet -> Hash
hashOtherL4Packet othrL4Pkt = hashPacketData $ payloadOtherL4 othrL4Pkt

-- Return the hash for L4 level packets
hashL4Packet :: L4BasePacket -> Hash
hashL4Packet (UDPPkt udpPkt) = hashUDPPacket udpPkt
hashL4Packet (TCPPkt tcpPkt) = hashTCPPacket tcpPkt
hashL4Packet (OtherL4Pkt othrL4Pkt) = hashOtherL4Packet othrL4Pkt

-- Hash given packet, if packet is invalid, then raise error
hashPacket :: L4Packet -> Hash
hashPacket (L4BasePkt l4Pkt) = hashL4Packet l4Pkt
hashPacket (InvalidPktL4 _ _) = error "hashing invalid pkt"


-- #################### Packet generator ####################

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: UnknownPacket
getNextPacket =
   UnknownPacket $ PacketData $ BS.pack ([50..120] :: [W.Word8])

-- #################### Main module ####################

-- main function which prints the fate of the next packet
-- main = print $ validatePacket getNextPacket
main = print $ (show l4Pkt)
           ++  ", Hash is " ++ (show hash)
           ++ ", Selected queue is " ++ (show queue)
           ++ ", Selected core is " ++ (show core)
           ++ ", no. of packets processed " ++ (show pktCount)
    where
        nicState = NS.initNICState
        (nicState2, l4Pkt) = validatePacket nicState getNextPacket
        hash = hashPacket l4Pkt
        queue = NS.lookupQueue nicState2 hash
        core = NS.lookupCore nicState2 hash
        pktCount = NS.packetCount nicState2

-- #################### EOF ####################
