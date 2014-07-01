#!/bin/bash

# the tool should get only one number as commandline arg
if [ $# != 2 ] ; then
    echo "ERROR: This script expects only one argument, as number of application slots"
    echo "USAGE: $0  <hw queues> <no. of application slots>"
    echo "EXAMPLE: $0 1 1"
    echo "EXAMPLE: $0 4 4"
fi

hwQueue=${1}
appSlots=${2}

echo "Waiting for ${hwQueue} hardware queues and ${appSlots} application slots"
set +x
set -e

./wait_for_dragonet.sh ${hwQueue}

evCount=1
while true;
do
    readyCount=$(ls -l *.appready 2> /dev/null | wc -l)

    if [ "${readyCount}" == "${appSlots}"  ] ; then
        echo "All ${readyCount} apps connected and ready!"
        ls *.appready
        exit 0
    fi
    echo "Apps ready=${readyCount}"
    evCount=`expr 1 + ${evCount}`

    if [ "${evCount}" == "15"  ] ; then
        echo "ERROR: Event-count ${evCount} reached!, giving up on waiting"
        exit 1
    fi
    sleep 1
done

echo "Failed for some unknown reason!"
exit 1

