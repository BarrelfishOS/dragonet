{-

Models the processing performed by e1000 RX path on incoming packet.

Aim:
  --Input: Incoming packet
  --        Configuration
  --Output:
  --    Packet in classified queue
  --    Notification of packet generation
  --    Status of packet in descriptor

What needs to capture
    -- Incoming packet
    -- Calculations performed
    -- Decisions made
    --  Classifications made.

What question this code needs to answer?
  -- Which queue does this code belongs
-}

let pkt = [1..64]
-- define pkt_status
if conf_mac_filter then status_mac_filter =
-- If conf_mac_filter then add tuple returned by apply_mac_filter (return value
-- can be true, false, NULL)
--
-- pkt_status
--      key value store
--      for every decision and computation on packet, it stores the result
--
-- checksum_calculation
--      A function which takes packet as input and returns a hash (a number)
--      which is a checksum of packet.
--
-- validate_checksum  :: checksum_fun -> packet -> bool
--      uses checksum_calculation to calculate checksum and returns
--      true/false based on if the checksum matched or not.
--
-- hash_packet :: hash_type -> packet -> hash
-- Runs the hash function based on the requested hash_type and returns the
-- hash
--
-- classify :: packet -> hash_type
-- returns a hashing function
--
--
-- eg:
--  hash_packet classify pkt pkt
--
-- FIXME: Make sure that if the packet has differnt type than expected then
-- hash value is all zeros.
--
--
