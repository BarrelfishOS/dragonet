#!/bin/bash

startStack() {
./cleanupServer.sh
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}

exit 0
}

check_working_dummy() {
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 1 -l 15 -H asiago -T 10.113.4.95 -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}
exit 0
}



getData() {

title="${ECHO_SERVER},CLC_${CLIENTCOUNT},PKT_${PACKETSIZE}"

OUTDIR="./nsdi_data/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/NIC_Intel/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H asiago -T 10.113.4.95 -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} -t ${title} -o ${OUTDIR}
}


ClientList="-C ziger1 -C ziger2 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2--10.113.4.29 -C sbrinz2--10.113.4.129 -C burrata--10.113.4.96 -C burrata--10.113.4.196"

ClientList="-C burrata -C ziger1 -C ziger2 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "

ECHO_SERVER="llvmE10k"
UDP_TEST_NAME="udp_rr"
HWQUEUE=10
SRVCORES=10
CONCURRENTY=32
PACKETSIZE=1024

CLIENTCOUNT=8
SRVCORES=8

CLIENTCOUNT=32
CLIENTCOUNT=40
CLIENTCOUNT=24
CLIENTCOUNT=16

SRVCORES=10


CLIENTCOUNT=40

set -x
set -e
#startStack

#check_working_dummy

#exit 0

CONCURRENTY=1

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0

CONCURRENTY=32

PACKETSIZE=64
getData

PACKETSIZE=1024
getData


exit 0


## old code, not use anymore. should be removed from next commit onwards

startStackOld() {
./cleanupServer.sh
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${Clients10} --servercores 10 \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}
}

getDataOld() {
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

