#!/bin/bash

startStack() {
./cleanupServer.sh
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${Clients10} --servercores 10 \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}
}

getData() {
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${Clients10} --servercores 10 \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H asiago -T 10.113.4.95 -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} -t ${title} -o ${OUTDIR}
}


OUTDIR="./nsdi_data/Dnet_Intel_Echo/"
Clients10="-C ziger1 -C ziger2 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2--10.113.4.29 -C sbrinz2--10.113.4.129 -C burrata--10.113.4.96 -C burrata--10.113.4.196"

ECHO_SERVER="llvmE10k"
UDP_TEST_NAME="udp_rr"
HWQUEUE=10
CONCURRENTY=32
CLIENTCOUNT=10
PACKETSIZE=64
title="${ECHO_SERVER},CLC_${CLIENTCOUNT},PKT_${PACKETSIZE}"

#startStack
#exit 0

PACKETSIZE=64
title="${ECHO_SERVER},CLC_${CLIENTCOUNT},PKT_${PACKETSIZE}"
getData

PACKETSIZE=1024
title="${ECHO_SERVER},CLC_${CLIENTCOUNT},PKT_${PACKETSIZE}"
getData

