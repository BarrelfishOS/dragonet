-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Util.GraphMonad(
    GraphMonad,
    runOn,
    getGr,
    withGr,
    newNode,
    newEdge,
    delEdge
) where

import Data.Graph.Inductive hiding (delEdge)
import Control.Monad.State

type GraphMonad a b c = State (Gr a b) c

runOn :: GraphMonad a b c -> Gr a b -> (c, Gr a b)
runOn =  runState

getGr :: GraphMonad a b (Gr a b)
getGr = get

putGr :: Gr a b -> GraphMonad a b ()
putGr = put

withGr :: (Gr a b -> c) -> GraphMonad a b c
withGr fn = do
    g <- getGr
    return $ fn g

newNode :: a -> GraphMonad a b Node
newNode label = do
    g <- getGr
    let n = head $ newNodes 1 g
    putGr $ insNode (n,label) g
    return n

newEdge :: (Node,Node,b) -> GraphMonad a b ()
newEdge edge = do
    g <- getGr
    putGr $ insEdge edge g

delEdge :: Eq b => (Node,Node,b) -> GraphMonad a b ()
delEdge edge = do
    g <- getGr
    putGr $ delLEdge edge g


