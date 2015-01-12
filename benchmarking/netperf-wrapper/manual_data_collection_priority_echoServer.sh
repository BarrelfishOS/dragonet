#!/bin/bash

startStack() {
#./cleanupServer.sh

initConcurrency=${CONCURRENCY}

title="STARTSTACK_Priority_Test_${UDP_TEST_NAME},CONCUR_${initConcurrency},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme/STARTSTACK/priority/Test_${UDP_TEST_NAME}/CONCUR_${initConcurrency}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency ${initConcurrency} \
-I 1 -l 5 -H ${SERVERNAME} -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
exit 0
}

check_working_dummy() {

title="DUMMY_Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENCY},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme/DUMMY/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENCY}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENCY} \
-I 1 -l 15 -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
exit 0
}



getData() {

title="Priority_Test_${UDP_TEST_NAME},CONCUR_${CONCURRENCY},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./dpdk_test_deleteme2/priority/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENCY}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENCY} \
-I 3 -l 50 -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"

}

#ClientList="-C ziger1 -C ziger2 -C babybel3 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "
ClientList="-C ziger1 -C sbrinz1 "
ClientList="-C appenzeller "
ClientList="-C ziger1 -C ziger2 -C sbrinz1 -C sbrinz2 -C gruyere -C appenzeller"

ClientList="-C ziger1 -C ziger2 -C gruyere"

NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_Intel"
ECHO_SERVER="llvmE10k"
ECHO_SERVER="dpdk"
SERVERNAME="babybel2"
SERVERIP=10.113.4.95

SERVERNAME="sbrinz1"
SERVERIP=10.113.4.26


UDP_TEST_NAME="udp_rr"
UDP_TEST_NAME="memcached_rr"
HWQUEUE=10
PACKETSIZE=1024

######################

CONCURRENCY=32
CONCURRENCY=1
CONCURRENCY=4
######################


SRVCORES=2
SRVCORES=6
SRVCORES=10
SRVCORES=4
######################


CLIENTCOUNT=32
CLIENTCOUNT=10
CLIENTCOUNT=2
CLIENTCOUNT=8
CLIENTCOUNT=20
CLIENTCOUNT=40
CLIENTCOUNT=4
######################

set -x
set -e

startStack

#check_working_dummy
exit 0

#CONCURRENCY=16

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0

#CONCURRENCY=32

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

CONCURRENCY=64

PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0


