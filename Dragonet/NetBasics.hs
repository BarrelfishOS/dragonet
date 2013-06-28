#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - All the possible computations which can happen in the Network processing.
 -
 -
 -}

--module Main (
module NetBasics (
    DesLabel(..)
    , ConfLabel(..)
    , OpLabel(..)
    ,
) where


type L2Address = Integer
type L3Address = Integer
type L4Address = Integer

-- for flow filtering
data Protocol = NONEProtocol
    | Ethernet
    | IEEE80211
    | IPv4
    | IPv6
    | ICMP
    | UDP
    | TCP
    | ANYProtocol
    deriving (Show, Eq, Ord, Enum)

type Qid = Integer
type CoreID = Integer
type FilterID = Integer

data Layer = L1 -- hardware
        | L2 -- Ethernet
        | L3 -- IP layer
        | L4 -- TCP/UDP layer
        | L5 -- Application
        deriving (Show, Eq, Ord)

data DesLabel = DesLabel String
    deriving (Show, Eq)

data ConfLabel = ConfLabel String
    deriving (Show, Eq)

data OpLabel = OpLabel String
    deriving (Show, Eq)





