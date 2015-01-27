#!/bin/bash

OUTPUTDIRPARENT="./dpdk_test_deleteme/"
OUTPUTDIRPARENT="./dpdk_test_memcached_test_10Q/"
OUTPUTDIRPARENT="./dpdk_test_memcached_test1_5Q/"
OUTPUTDIRPARENT="./output_runs_balance/"
OUTPUTDIRPARENT="./output_runs_deletemev2/"
OUTPUTDIRPARENT="./output_runs_posterV3/"
OUTPUTDIRPARENT="./output_runs_8cores/"
OUTPUTDIRPARENT="./output_runs_linuxTest/"
OUTPUTDIRPARENT="./output_runs_linuxTest_deleteme/"

# For debugging of dynamic connection detection
OUTPUTDIRPARENT="./output_runs_dynamic_debug/"

WORKLOADTYPE="priority"
WORKLOADTYPE="priBal"
WORKLOADTYPE="linux"
WORKLOADTYPE="balance"
WORKLOADTYPE="priBal"
WORKLOADTYPE="priSmall"
WORKLOADTYPE="dynamic"

startStack() {
#./cleanupServer.sh

initConcurrency=${LOAD}

title="STARTSTACK_${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${initConcurrency},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="${OUTPUTDIRPARENT}/STARTSTACK/${WORKLOADTYPE}/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${initConcurrency}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES}  --packet ${PACKETSIZE} --concurrency ${initConcurrency} \
-I 1 -l 5 -H ${SERVERNAME} -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog"
#exit 0
}

check_working_dummy() {

dummyRT=1
title="DUMMY_${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="${OUTPUTDIRPARENT}/DUMMY/${WORKLOADTYPE}/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES} --packet ${PACKETSIZE} --concurrency ${LOAD} \
-I 1 -l ${dummyRT} -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog"
#exit 0
}


getData() {

title="${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

REPEAT=3
./netperf-wrapper -d 0 --udp --serverCoreShift 0  ${ClientList} --servercores ${SRVCORES} \
--serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES} --packet ${PACKETSIZE} --concurrency ${LOAD} \
-I ${REPEAT} -l 50 -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} --totalClients ${CLIENTCOUNT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog"
}

######################
SERVERNAME="babybel2"
CLIENTCORES=1
######################


#ClientList="-C ziger1 -C ziger2 -C babybel3 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "
ClientList="-C ziger1 -C sbrinz1 "
ClientList="-C appenzeller "
ClientList="-C ziger1 -C ziger2 -C sbrinz1 -C sbrinz2 -C gruyere -C appenzeller"

ClientList="-C ziger1 -C ziger2 -C gruyere"
ClientList="-C ziger1 -C ziger2"
ClientList="-C sbrinz1 -C sbrinz2"
ClientList="-C ziger1 -C sbrinz1"

ClientList="-C ziger2 -C sbrinz2"
ClientList="-C ziger1 -C ziger2 -C sbrinz1 -C sbrinz2"
ClientList="-C ziger2 "

######################

NICTYPE="NIC_SF"
ECHO_SERVER="memcached_onload"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="memcached_linux"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_Intel"
ECHO_SERVER="dpdk"
SERVERIP=10.113.4.95

######################

#SERVERNAME="sbrinz1"
#SERVERIP=10.113.4.26

######################

UDP_TEST_NAME="udp_rr"
UDP_TEST_NAME="memcached_rr"


######################
PACKETSIZE=64
PACKETSIZE=1024

######################

LOAD=1
LOAD=4
LOAD=32
######################

HWQUEUE=8
HWQUEUE=10
HWQUEUE=5
######################

SRVCORES=8
SRVCORES=10
SRVCORES=5
######################


CLIENTCOUNT=32
CLIENTCOUNT=10
CLIENTCOUNT=2
CLIENTCOUNT=8
CLIENTCOUNT=20
CLIENTCOUNT=40
CLIENTCOUNT=4
######################

show_usage() {
        echo "Please select what you want to run"
        echo "Usage: ${0} [-c -s -l]"
        echo "           -c <n> -->  client count (n clients)"
        echo "           -l <n> -->  load (n concurrent sessions per client)"
        echo "           -s <n> -->  n server cores/threads"
        echo "           -S     -->  start stack"
        echo "           -d     -->  dummy run"
        echo "           -r     -->  Real run"
        echo "           -x     -->  Cleanup involved machines"
        echo "           -X     -->  Delete output/log files"
        echo "           -h     -->  this help"
        echo "Examples (starting stack): ${0} -c 4 -s 4 -C 4 S"
        exit 1
}

if [ $# == 0 ] ;
then
    echo "ERROR: $0: No arguments given"
    show_usage
    exit 1
fi

while getopts ":c:l:s:SdhxXr" opt; do
  case $opt in
    c)
        echo "Setting client-count to $OPTARG (instead of default $CLIENTCOUNT)"
        CLIENTCOUNT=$OPTARG
      ;;
    l)
        echo "Setting load to $OPTARG (instead of default $LOAD)"
        LOAD=$OPTARG
      ;;
    s)
        echo "Setting server-cores/threads to $OPTARG (instead of default $SRVCORES)"
        SRVCORES=$OPTARG
        HWQUEUE=${SRVCORES}
      ;;
    S)
        RUNSTACK="yes"
      ;;
    d)
        RUNDUMMY="yes"
      ;;
    r)
        RUNREAL="yes"
      ;;
    x)
        CLEANUP="yes"
      ;;
    X)
        DELETEFILES="yes"
      ;;

    h)
        show_usage
        exit 0
      ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        show_usage
        exit 1
        ;;
    :)
        echo "Option -$OPTARG requires an argument." >&2
        show_usage
        exit 1
        ;;
  esac
done

if [ "${RUNSTACK}" == "yes" ] ; then
    echo "Starting the stack"
    set -x
    set -e
    startStack
    set +x
    set +e
fi

if [ "${RUNDUMMY}" == "yes" ] ; then
    echo "Assuming that stack is already running, just running dummy for testing"
    set -x
    set -e
    check_working_dummy
    set +x
    set +e
fi

if [ "${RUNREAL}" == "yes" ] ; then
    echo "Assuming that stack is already running, doing real run"
    set -x
    set -e
    getData
    set +x
    set +e
fi


if [ "${DELETEFILES}" == "yes" ] ; then
    echo "Deleting output and log files: ${OUTPUTDIRPARENT}"
    echo "Warning:You might loose all the data from directory ${OUTPUTDIRPARENT}"
    read -p "Are you sure!! (y/n) :" ANSWER
    if [ "${ANSWER}" == "y" ] ;
    then
        set -x
        rm -rf ${OUTPUTDIRPARENT}
        set +x
    else
        echo "You answered ${ANSWER}, so not deleting everything"
    fi
fi
if [ "${CLEANUP}" == "yes" ] ; then
    echo "Cleaning up server: $SERVERNAME"
    ./cleanup.sh $SERVERNAME
    clist=$(echo $ClientList | sed 's/-C//g')
    echo "Cleaning up clients: $clist"
    ./cleanup.sh $clist
fi

