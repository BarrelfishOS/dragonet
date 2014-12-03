#!/bin/bash

startStack() {
#./cleanupServer.sh

title="STARTSTACK_Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme/STARTSTACK/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H babybel2 -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
exit 0
}

check_working_dummy() {

title="DUMMY_Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme/DUMMY/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 1 -l 15 -H babybel2 -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
exit 0
}



getData() {

title="Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H babybel2 -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"

}

#ClientList="-C ziger1 -C ziger2 -C babybel3 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "
ClientList="-C ziger1 -C sbrinz1 "
ClientList="-C ziger1 -C ziger2 -C babybel3 -C appenzeller -C sbrinz1 -C sbrinz2 "
ClientList="-C appenzeller "

NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_Intel"
ECHO_SERVER="llvmE10k"
ECHO_SERVER="dpdk"
SERVERIP=10.113.4.95

UDP_TEST_NAME="memcached_rr"
UDP_TEST_NAME="udp_rr"
HWQUEUE=10

######################
CONCURRENTY=16
######################

PACKETSIZE=1024

CLIENTCOUNT=8

SRVCORES=6
SRVCORES=10
SRVCORES=2


CLIENTCOUNT=24


CLIENTCOUNT=32
CLIENTCOUNT=6
CLIENTCOUNT=10
CLIENTCOUNT=2

set -x
set -e

startStack

#check_working_dummy

CONCURRENTY=16

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

CONCURRENTY=32

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

CONCURRENTY=64

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0

