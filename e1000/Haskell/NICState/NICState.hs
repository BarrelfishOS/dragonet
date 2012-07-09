{- How Redirection table should be implemented?
 - init: creates state including empty redirection table
 -
 - lookupHash :: given hash returns table state
 -          if nothing matches, should return default value
 -
 -  addEntry :: for given Hash, add the tuple
 -
 -  deleteEntry :: for given hash, remove the tuple
 -
 -
-}

-- module Main (main) where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe

type QueueID = Integer  -- ID for the hardware queue
type CoreID = Integer  -- ID for the CPU which can accept notification

-- Redirection table holding the mapping between hash values and CPU cores
type RedirectTbl = [(QueueID, CoreID)]

-- Mechanism to convert hash into index for redirection Table.
type Hash = Integer -- Hash of the packet
type Index = Integer -- Index within Redirection Table

data QueueState = QueueState {
        , queueID :: QueueID
        , coreID :: CoreID
    } deriving (Show, Eq)

data NICState = NICState {
        queueState :: Map.Map [QueueState]
        , useMultiQueue :: Maybe.Maybe Bool
    } deriving (Show, Eq)


-- Initialize the NICState to empty state
initNICState :: NICState
initNICState = NICState {
            queueState = Map.empty
            , useMultiQueue = False
       }

-- Controls MultiQueue by turning it on or off
controlMultiQueue :: NICState -> Bool -> NICState
controlMultiQueue nicState False = nicState {
                        useMultiQueue = Maybe.Nothing
                    }
controlMultiQueue nicState True = nicState {
                        useMultiQueue = Maybe.Just True
                    }



-- Add queue setting into NICState
updateQueueElement :: NICState -> QueueState -> Index -> NICState
addElement nicState qState idx = nicState {
                       queueState = qState
                    }


