#!/bin/bash

set -x
set -e


runServer() {
    taskset -c  ${CORE_SERVER}  ${LOADSINK} -4 -L ${SERVERIP}
    sleep 2
}

recordServer() {
    ${DSTAT} -tcny --net-packets -N ${IFACE_SERVER} -C ${CORE_SERVER} --bits --json "${SERVER_DSTAT}.json" > ${SERVER_DSTAT}.dstat &
    DSTAT_SERVER_PID=$!
}

recordClient() {
    ${DSTAT} -tcny --net-packets -N ${IFACE_CLIENT} -C ${CORE_CLIENT} --bits --json "${CLIENT_DSTAT}.json" > ${CLIENT_DSTAT}.dstat &
    DSTAT_CLIENT_PID=$!
}

stopRecording() {
    kill ${DSTAT_SERVER_PID}
    kill ${DSTAT_CLIENT_PID}
}

killServer() {
    local toKill=`basename ${LOADSINK}`
    echo "killing ${toKill}"
    killall ${toKill}
}


MACHINE="127.0.0.1"
SERVERIP="127.0.0.1"
CORE_SERVER=3
CORE_CLIENT=1
IFACE_SERVER="lo"
IFACE_CLIENT="lo"

DSTAT="../dstat/dstat"

OUTPUTDIR="../output/"

LOADSINK="../netperf-2.6.0/src/netserver"

SERVERPIP="127.0.0.1"

NPW="../netperf-wrapper/netperf-wrapper"
LOADGEN="../netperf-2.6.0/src/netperf"


mkdir -p  ${OUTPUTDIR}

SERVER_DSTAT=`mktemp --tmpdir=${OUTPUTDIR} dserver_XXXXXX`
CLIENT_DSTAT=`mktemp --tmpdir=${OUTPUTDIR} dclient_XXXXXX`


# kill any previous instances
# killall python || true

killall dstat || true
killServer || true
runServer
recordServer
recordClient
taskset -c ${CORE_CLIENT} ${LOADGEN} -P 0 -l 5 -t UDP_STREAM -fg -H ${SERVERIP} -- -r 8900 -k all
stopRecording || true
killServer || true
echo "Server Logs: ${SERVER_DSTAT}"
echo "Client Logs: ${CLIENT_DSTAT}"
echo "Done..."

