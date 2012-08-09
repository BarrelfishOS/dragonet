module LPGModules (
    mNIC
    , mEthernet
    , mIPv4
    , mIPv6
    , mIPProto
    , mTCP
    , mUDP
    , mICMP
    , mTCPPCB
    , mUDPPCB
    , mSocket
    , mAPP
) where

import qualified NICState as NS
import qualified DecisionTree as DT


-- Module to simulates simple NIC
mNIC :: NS.NICState -> DT.Packet -> DT.CResult
mNIC nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mNIC nicstate _ = error "invalid type packet passed to mNIC"

-- Module to represent Ethernet processing
mEthernet :: NS.NICState -> DT.Packet -> DT.CResult
mEthernet nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mEthernet nicstate _ = error "invalid type packet passed to mEthernet"

-- Module to represent IP protocol processing
mIPProto :: NS.NICState -> DT.Packet -> DT.CResult
mIPProto nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mIPProto nicstate _ = error "invalid type packet passed to mIPProto"

-- Module to represent IPv4 protocol processing
mIPv4 :: NS.NICState -> DT.Packet -> DT.CResult
mIPv4 nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mIPv4 nicstate _ = error "invalid type packet passed to mIPv4"

-- Module to represent IPv6 protocol processing
mIPv6 :: NS.NICState -> DT.Packet -> DT.CResult
mIPv6 nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mIPv6 nicstate _ = error "invalid type packet passed to mIPv6"

-- Module to represent TCP protocol processing
mTCP :: NS.NICState -> DT.Packet -> DT.CResult
mTCP nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mTCP nicstate _ = error "invalid type packet passed to mTCP"

-- Module to represent UDP protocol processing
mUDP :: NS.NICState -> DT.Packet -> DT.CResult
mUDP nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mUDP nicstate _ = error "invalid type packet passed to mUDP"

-- Module to represent ICMP protocol processing
mICMP :: NS.NICState -> DT.Packet -> DT.CResult
mICMP nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mICMP nicstate _ = error "invalid type packet passed to mICMP"

-- Module to represent TCP PCB processing
mTCPPCB :: NS.NICState -> DT.Packet -> DT.CResult
mTCPPCB nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mTCPPCB nicstate _ = error "invalid type packet passed to mTCPPCB"

-- Module to represent TCP PCB processing
mUDPPCB :: NS.NICState -> DT.Packet -> DT.CResult
mUDPPCB nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mUDPPCB nicstate _ = error "invalid type packet passed to mUDPPCB"

-- Module to represent TCP PCB processing
mSocket :: NS.NICState -> DT.Packet -> DT.CResult
mSocket nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mSocket nicstate _ = error "invalid type packet passed to mSocket"



-- Module to represent TCP PCB processing
mAPP :: NS.NICState -> DT.Packet -> DT.CResult
mAPP nicstate (DT.RawPacket pktContents) = DT.ValidAction 0
mAPP nicstate _ = error "invalid type packet passed to mAPP"



