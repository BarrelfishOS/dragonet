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
