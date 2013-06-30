import DragonetDSL
import qualified Operations as OP
import DotGenerator as DG

[dragonet|
node source {
  port out[classifiedL2Ethernet] }

node packetDrop { }

node packetValid { }

boolean classifiedL2Ethernet {
    port true[l2EtherValidUnicast l2EtherValidMulticast l2EtherValidBroadcast]
    port false[packetDrop] }


boolean l2EtherValidUnicast {
    port true false[l2EtherValidDest] }

boolean l2EtherValidMulticast {
    port true false[l2EtherValidDest] }

boolean l2EtherValidBroadcast {
    port true false[l2EtherValidDest] }

boolean l2EtherValidSrc {
    port true false[l2EtherValidAddr] }

or l2EtherValidDest {
    port true false[l2EtherValidAddr] }

and l2EtherValidAddr {
    port false[packetDrop]
    port true[packetValid] }

|]

main = do
    putStrLn (DG.toDot source)
