-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Util.XTimeIt(
    doTimeIt,
    dontTimeIt,
)
where

import System.TimeIt (timeIt, timeItT)

-- use this one so that we can globally disable it
doTimeIt s op =  do
    (t, ret) <- timeItT op
    putStrLn $ "[XTIME] " ++ s ++"  "++ (show t) ++ "s"
    return ret

dontTimeIt s op =  do
    ret <- timeItT op
    return ret
