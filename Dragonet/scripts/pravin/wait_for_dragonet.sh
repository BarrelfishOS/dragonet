#!/bin/bash

ensure_running_process() {
    process=${1}
    # Make sure that stack is still running and not crashed by
    # checking its pid
    isRunning=$(pidof ${process} | wc -l)
    if [ "${isRunning}" == "0"  ] ; then
        echo "Stack with name ${process} is not running anymore"
        exit 1
    fi
}

SCRIPTDIR="./scripts/pravin/"
# the tool should get only one number as commandline arg
if [ $# != 2 ] ; then
    echo "ERROR: This script expects two arguments, as number of HW queues, and stackname"
    echo "USAGE: $0  <stack-name> <no. of HW queues>"
    echo "EXAMPLE: $0 stack-sf 1"
    echo "EXAMPLE: $0 stack-e10k 4"
    echo "EXAMPLE: $0 stack-dpdk 4"
fi

stackName=${1}
hwQcount=${2}
pipelineCount=`expr 2 \* ${hwQcount}`

echo "Waiting for ${hwQcount} hw queues ( ${pipelineCount} pipelines)"

set +x
set -e

counter=0

while true;
do
    let counter=counter+1
    dnreadyCount=$(ls -l *.dnready 2> /dev/null | wc -l)
    if [ "${dnreadyCount}" != "0"  ] ; then
        echo "The NIC driver is started and running"
        #cat some.log
        cat *.dnready
        sleep 4
        exit 0
    fi

    ensure_running_process ${stackName}

    if [ ${counter} -gt 160 ] ; then
        echo "The NIC driver has not started yet, giving up"
        exit 1
    fi
    sleep 1
done

echo "Failed for some unknown reason!"
cat some.log
exit 1

