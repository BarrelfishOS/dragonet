{- How Redirection table should be implemented?
 - init: creates state including empty redirection table
 -
 - lookupHash :: given hash returns table state
 -          if nothing matches, should return default value
 -
 -  updateQueueElement :: for given index, update the queue assignment
 -
 -  deleteEntry :: for given index, remove mapping from Queue lookup table
 -
 -
-}

module NICState (
    QueueState
    , NICState
    , initNICState
    , controlMultiQueue
    , updateQueueElement
    , lookupQueue
    , lookupCore
) where

-- import qualified Data.Map as Map

type QueueID = Integer  -- ID for the hardware queue
type CoreID = Integer  -- ID for the CPU which can accept notification

-- Mechanism to convert hash into index for redirection Table.
type Hash = Integer -- Hash of the packet
type Index = Integer -- Index within Redirection Table

data QueueState = QueueState {
        queueID :: QueueID
        , coreID :: CoreID
    } deriving (Show, Eq)

data NICState = NICState {
        queueState ::[QueueState]
        , useMultiQueue :: Bool
    } deriving (Show, Eq)


-- Returns number of queues in system
getQueueCount :: Integer
getQueueCount = 12 -- 127


-- DefaultQueue mapping
-- To be used in initialization
getDefaultQueue :: QueueState
getDefaultQueue = QueueState {
            queueID = 0
            , coreID = 0
            }

-- Initialize the NICState to empty state
initNICState :: NICState
initNICState = NICState {
            queueState = replicate (fromInteger getQueueCount) getDefaultQueue
            , useMultiQueue = False
       }

-- Controls MultiQueue by turning it on or off
controlMultiQueue :: NICState -> Bool -> NICState
controlMultiQueue nicState False = nicState {
                        useMultiQueue = False
                    }
controlMultiQueue nicState True = nicState {
                        useMultiQueue = True
                    }

-- Replace specified queue configuration with new new configuration
replaceQueueElement :: [QueueState] -> Index -> QueueState -> [QueueState]
replaceQueueElement qList idx qState = a ++ (qState:b) where
        (a, (_:b)) = splitAt (fromInteger idx) qList


-- update queue settings for given queue
updateQueueElement :: NICState -> Index -> QueueState  -> NICState
updateQueueElement nicState idx qState = nicState {
                       queueState = replaceQueueElement
                            (queueState nicState) idx qState
                    }


-- Given hash, convert into an index for Redirection Table
-- This function will use last x bits of the hash (based on no. of queues)
hashToIndex :: Hash -> Index
hashToIndex hash = fromIntegral (hash `mod` getQueueCount)

-- lookupHash :: given hash returns table state
--          if nothing matches, should return default value
lookupHash :: NICState -> Hash -> QueueState
lookupHash nicState hash = (queueState nicState) !! idx
    where
        idx = fromInteger $ hashToIndex hash


-- ##################### Dealing with NIC state ###########

-- For given lookup-table and hash, give the QueueID to which packet should go
lookupQueue :: NICState -> Hash -> QueueID
lookupQueue nicState hash_value =
       queueID (lookupHash nicState hash_value)

-- For given lookup-table and hash, give the core which should
-- receive the notification
lookupCore :: NICState -> Hash -> QueueID
lookupCore nicState hash_value =
       coreID (lookupHash nicState hash_value)



{-
-- main function for testing purposes
main = print $ (show nicStateF)
    where
        nicState1 = controlMultiQueue initNICState True
        nicStateF = updateQueueElement nicState1 2 (QueueState 2 3)
-}


