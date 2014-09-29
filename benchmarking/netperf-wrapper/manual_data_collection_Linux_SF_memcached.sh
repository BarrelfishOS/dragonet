#!/bin/bash


startStack() {
#./cleanupServer.sh
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H asiago -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}

exit 0
}

check_working_dummy() {
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 1 -l 15 -H asiago -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}
exit 0
}



getData() {

title="Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},${NIC_TYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="./nsdi_data/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/${NIC_TYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H asiago -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"

}

ClientList="-C ziger1 -C ziger2 -C burrata -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "

SERVERIP="10.113.4.195"
NICTYPE="NIC_SF"
UDP_TEST_NAME="memcached_rr"
ECHO_SERVER="memcached_onload"

HWQUEUE=10

CONCURRENTY=1

PACKETSIZE=1024
CLIENTCOUNT=8
SRVCORES=8


CLIENTCOUNT=32
CLIENTCOUNT=24
CLIENTCOUNT=16

set -x
set -e
SRVCORES=10

CLIENTCOUNT=40

#startStack

#check_working_dummy


PACKETSIZE=64
getData

PACKETSIZE=1024
getData

exit 0

