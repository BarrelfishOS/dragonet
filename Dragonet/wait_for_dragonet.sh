#!/bin/bash

# the tool should get only one number as commandline arg
if [ $# != 1 ] ; then
    echo "ERROR: This script expects only one argument, as number of HW queues"
    echo "NOTE: It assumes that there are 2 queues (RX and TX) for every HW queue"
    echo "USAGE: $0 <no. of HW queues>"
    echo "EXAMPLE: $0 1"
    echo "EXAMPLE: $0 4"
fi

hwQcount=${1}
pipelineCount=`expr 2 \* ${hwQcount}`

echo "Waiting for ${hwQcount} hw queues ( ${pipelineCount} pipelines)"

set +x
set -e

while true;
do
    readyCount=$(ls -l *.ready 2> /dev/null | wc -l)
    failedCount=$(ls -l *.failed 2> /dev/null | wc -l)
    #failedCount=`ls -l *.failed | wc -l`
    if [ "${failedCount}" != "0"  ] ; then
        echo "One of the pipeline failed! FailedCount=${failedCount}"
        ls *.failed
        exit
    fi

    if [ "${readyCount}" == "${pipelineCount}"  ] ; then
        echo "All ${readyCount} pipelines are ready!"
        ls *.ready
        exit 0
    fi
    echo "Pipelines ready=${readyCount}, Pipelines failed=${failedCount}"
    sleep 1
done

echo "Failed for some unknown reason!"
exit 1

