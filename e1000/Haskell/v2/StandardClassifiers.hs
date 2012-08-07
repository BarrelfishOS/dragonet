module StandardClassifiers (
    checksumCRC
    , isValidLength
    , classifyL2
    , isValidUnicast
    , isValidMulticast
    , isValidBroadcast
    , checksumIPv4
    , checksumIPv6
    , classifyL3
    , isValidTCP
    , isValidUDP
    , applyFilter
) where

import qualified NICState as NS
import qualified DecisionTree as DT


-- #################### Classifier placeholders ####################

-- Validate L2 packet
checksumCRC :: NS.NICState -> DT.Packet -> DT.CResult
checksumCRC nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
checksumCRC nicstate _ = DT.InvalidState "invalid type packet passed to checksumCRC"

-- Validate L2 length
isValidLength :: NS.NICState -> DT.Packet -> DT.CResult
isValidLength nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
isValidLength nicstate _ = DT.InvalidState "invalid type packet passed to isValidLength"

-- classify L2 packet
classifyL2 :: NS.NICState -> DT.Packet -> DT.CResult
classifyL2 nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
classifyL2 nicstate _ = DT.InvalidState "invalid type packet passed to classifyL2"


-- Validate isValidUnicast packet
isValidUnicast :: NS.NICState -> DT.Packet -> DT.CResult
isValidUnicast nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
isValidUnicast nicstate _ = DT.InvalidState "invalid type packet passed to isValidUnicast"

-- Validate isValidMulticast packet
isValidMulticast :: NS.NICState -> DT.Packet -> DT.CResult
isValidMulticast nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
isValidMulticast nicstate _ = DT.InvalidState "invalid type packet passed to isValidMulticast"

-- Validate isValidBroadcast packet
isValidBroadcast :: NS.NICState -> DT.Packet -> DT.CResult
isValidBroadcast nicstate (DT.RawPacket pktContents) = DT.ValidAction 1
                -- FIXME: Assuming valid packet
isValidBroadcast nicstate _ = DT.InvalidState "invalid type packet passed to isValidBroadcast"



-- Check if it is valid IPv4 packet
checksumIPv4 :: NS.NICState -> DT.Packet -> DT.CResult
checksumIPv4 nicstate (DT.L2Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
checksumIPv4 nicstate _ = DT.InvalidState "invalid type packet passed to isValidv4"

-- Check if it is valid IPv6 packet
checksumIPv6 :: NS.NICState -> DT.Packet -> DT.CResult
checksumIPv6 nicstate (DT.L2Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
checksumIPv6 nicstate _ = DT.InvalidState "invalid type packet passed to isValidv6"

-- Check if it is valid L3 packet
classifyL3 :: NS.NICState -> DT.Packet -> DT.CResult
classifyL3 nicstate (DT.IPv4Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
classifyL3 nicstate (DT.IPv6Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
classifyL3 nicstate _ = DT.InvalidState "invalid type packet passed to classifyL3"

-- Check if it is valid TCP packet
isValidTCP :: NS.NICState -> DT.Packet -> DT.CResult
isValidTCP nicstate (DT.L3Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
isValidTCP nicstate _ = DT.InvalidState "invalid type packet passed to isValidTCP"

-- Check if it is valid UDP packet
isValidUDP :: NS.NICState -> DT.Packet -> DT.CResult
isValidUDP nicstate (DT.L3Packet pkt) = DT.ValidAction 1 -- FIXME: Assuming valid packet
isValidUDP nicstate _ = DT.InvalidState "invalid type packet passed to isValidUDP"

-- Selects queue after applying filters based on packet type
applyFilter :: NS.NICState -> DT.Packet -> DT.CResult
applyFilter nicstate (DT.TCPPacket pkt) = DT.ValidAction 1 -- FIXME: get hash and select queue
applyFilter nicstate (DT.UDPPacket pkt) = DT.ValidAction 1 -- FIXME: get hash and select queue
applyFilter nicstate _ = DT.ValidAction 0 -- Default queue (when no other filter matches)


