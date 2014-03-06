{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import qualified Data.ByteString as BS

import qualified Util.ConcState as CS
import Dragonet.Implementation
import Dragonet.DotGenerator
import LPGImplTH
import LPGImpl
import qualified LPGEx1 as LPG1

main :: IO ()
main = do
    putStrLn "Generating .dot files..."
    writeFile "lpg.dot" $ toDotClustered LPG1.lpg LPG1.lpgClusters

    let ex = generateFCall LPG1.lpg "lpg"
    putStrLn $ pprint ex
    let f = $(return $ generateFCall LPG1.lpg "lpg")
    dnsResp <- BS.readFile "packets/dns_response"
    let gst = emptyGS
    let (g,_) = CS.runConcSM f $ initSimState gst dnsResp
    putStrLn $ unlines $ gsDebug g
    return ()
