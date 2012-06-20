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

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Maybe

hashTCP :: [Integer] -> Integer
hashTCP [] = 0
hashTCP (x:xs) = x + hashTCP xs

selectHashFunction :: [Integer] -> Maybe ([Integer] -> Integer)
selectHashFunction _ = Just hashTCP

-- This function will use ast 5 bits of the hash, and return value at that entry
lookupRedirectionTable :: [(Integer, Integer)] -> Integer -> Integer
lookupRedirectionTable redirectionTable hash_value = toInteger ( fst (redirectionTable!!fromIntegral hash_value))

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust: Nothing"

hashPacket :: [Integer] -> Integer
hashPacket p =
    if Data.Maybe.isNothing (selectHashFunction p)
        then 0
    else Data.Maybe.fromJust (selectHashFunction p) p


classifyPacket :: Map.Map [Char] Bool -> [(Integer, Integer)]  -> [Integer] -> Integer
classifyPacket conf_map rdt p = lookupRedirectionTable rdt (hashPacket p)

simpleCall :: Integer
simpleCall =
    let mm = Map.fromList([("enableChecksum", False),
                        ("enableMultiQueue", True)])

    -- The packet holding bytes
    -- FIXME: Sample values
        pkt = [1..64]

    -- Table specifying which hash should go to which queue/core
    -- FIXME: Sample values
        redirectionTable = replicate 127 (1, 1)
    in classifyPacket mm redirectionTable pkt

main = print $ simpleCall

-- module main where
--    main = putStrLn queueID

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
