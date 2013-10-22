{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import qualified Data.ByteString as BS
import Control.Monad.State

import Dragonet.Implementation
import LPGImplTH
import LPGImpl


main :: IO ()
main = do
    let ex = generateFCall lpg "lpg"
    putStrLn $ pprint ex
    let f = $(return $ generateFCall lpg "lpg")
    dnsResp <- BS.readFile "packets/dns_response"
    let gst = emptyGS
    let (_,x) = runState f $ initSimState gst dnsResp
    putStrLn $ unlines $ gsDebug $ ssGState x
    return ()

