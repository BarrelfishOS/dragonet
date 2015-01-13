
import qualified Dragonet.Configuration as C
import qualified Dragonet.Implementation.IPv4 as IPv4
import Dragonet.DotGenerator (toDot)
import Dragonet.Flows (Flow(..))

import qualified Graphs.E10k as E10k

import Data.Maybe
import Control.Applicative ((<$>))

e10kT_simple = E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
e10kU_simple = fst <$> e10kT_simple
e10kH_simple = snd <$> e10kT_simple

e10kT = E10k.graphH

confs = [
      (("*", 1000), 1)
    , (("*", 1001), 2)
 ]

udpLocalPort = 53
testMkFlow rip rport = FlowUDPv4 {
      flSrcIp   = case rip of
                     "*" -> Nothing
                     otherwise -> Just $ fromJust $ IPv4.ipFromString rip
    , flSrcPort = Just rport
    , flDstIp   = Nothing
    , flDstPort = Just udpLocalPort
}

main = do
    putStrLn "incrementalConfig"
    prgU <- e10kU_simple
    writeFile "tests/incrementalConfig-PRG-U.dot" $ toDot  prgU
    let cnf1 = E10k.mk5TupleFromFl (testMkFlow "*" 1000) 2
        cnf2 = E10k.mk5TupleFromFl (testMkFlow "*" 1001) 3
        prgI_1 = C.applyConfigInc [("RxC5TupleFilter",cnf1)] prgU
        prgI_2 = C.applyConfigInc [("RxC5TupleFilter",cnf2)] prgI_1
    writeFile "tests/incrementalConfig-PRG-I1.dot" $ toDot prgI_1
    writeFile "tests/incrementalConfig-PRG-I2.dot" $ toDot prgI_2
    return ()
