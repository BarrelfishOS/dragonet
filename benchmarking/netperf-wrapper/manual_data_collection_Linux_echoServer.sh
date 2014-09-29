#!/bin/bash

startStack() {
./cleanupServer.sh
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1   --packet ${PACKETSIZE} --concurrency 1 \
-I 1 -l 5 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT}

exit 0
}

check_working_dummy() {

rm -f deletme_dummyRun.log
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 1 -l 15 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} -L deletme_dummyRun.log
exit 0
}



getData() {

title="Test_${UDP_TEST_NAME},CONCUR_${CONCURRENTY},PKT_${PACKETSIZE},NIC_Intel,SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"

OUTDIR="./nsdi_data/Test_${UDP_TEST_NAME}/CONCUR_${CONCURRENTY}/PKT_${PACKETSIZE}/NIC_Intel/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores 1 --packet ${PACKETSIZE} --concurrency ${CONCURRENTY} \
-I 3 -l 50 -H asiago -T 10.113.4.95 -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
}


get_data_for_conc_1() {

./cleanupServer.sh
./cleanupClient.sh

    CONCURRENTY=1
    PACKETSIZE=64
    getData

    sleep 3

    PACKETSIZE=1024
    getData

    sleep 3
}


get_data_for_conc_32() {

./cleanupServer.sh
./cleanupClient.sh

    CONCURRENTY=32
    PACKETSIZE=64
    getData

    sleep 3

    PACKETSIZE=1024
    getData

    sleep 3
}

get_data_for_conc_16() {

./cleanupServer.sh
./cleanupClient.sh

    CONCURRENTY=16
    PACKETSIZE=64
    getData

    sleep 3

    PACKETSIZE=1024
    getData

    sleep 3
}



ClientList="-C burrata -C ziger1 -C ziger2 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "

ECHO_SERVER="fancyEchoLinux"
ECHO_SERVER="fancyEchoLinuxPoll"
UDP_TEST_NAME="udp_rr"
HWQUEUE=10
SRVCORES=10
CONCURRENTY=1
PACKETSIZE=1024


CLIENTCOUNT=32
CLIENTCOUNT=40
CLIENTCOUNT=24
CLIENTCOUNT=16


SRVCORES=10


set -x
set -e
#startStack

#check_working_dummy
#exit 0

CLIENTCOUNT=40
get_data_for_conc_32
get_data_for_conc_16

sleep 3

CLIENTCOUNT=32
get_data_for_conc_32
get_data_for_conc_16

sleep 3

CLIENTCOUNT=24
get_data_for_conc_32
get_data_for_conc_16

sleep 3

CLIENTCOUNT=16
get_data_for_conc_32
get_data_for_conc_16

sleep 3

CLIENTCOUNT=8
SRVCORES=8
get_data_for_conc_32
get_data_for_conc_16

exit 0


