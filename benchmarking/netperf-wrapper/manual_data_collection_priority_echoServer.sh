#!/bin/bash

startStack() {
#./cleanupServer.sh
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

title="Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},NIC_Intel,SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./nsdi_data/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/NIC_Intel/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H asiago -T 10.113.4.95 -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"

}

ClientList="-C ziger1 -C ziger2 -C burrata -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "


ECHO_SERVER="llvmE10k"
UDP_TEST_NAME="udp_rr"
HWQUEUE=10

######################
CONCURRENTY=16
######################

PACKETSIZE=1024

CLIENTCOUNT=8
SRVCORES=8

SRVCORES=10


CLIENTCOUNT=24

CLIENTCOUNT=40
CLIENTCOUNT=16

CLIENTCOUNT=32

set -x
set -e

startStack
#check_working_dummy

CONCURRENTY=1

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

CONCURRENTY=16

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

CONCURRENTY=16

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0

