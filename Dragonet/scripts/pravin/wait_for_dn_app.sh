#!/bin/bash

SCRIPTDIR="./scripts/pravin/"

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

# the tool should get only one number as commandline arg
if [ $# != 4 ] ; then
    echo "ERROR: incomplete agruments"
    echo "USAGE: $0 <stack-name>  <hw queues> <no. of application slots> <app-name>"
    echo "EXAMPLE: $0 stack-sf 1 1 bench-fancyecho"
    echo "EXAMPLE: $0 stack-e10k 4 4  memcached"
    echo "EXAMPLE: $0 stack-dpdk 4 4  memcached"
fi

stackName=${1}
hwQueue=${2}
appSlots=${3}
appName=${4}

echo "Waiting for ${hwQueue} hardware queues and ${appSlots} application slots"
set +x
set -e

${SCRIPTDIR}/wait_for_dragonet.sh ${hwQueue}  ${stackName}

AppslistFile="allAppslist.appready"

appSlots=`expr 1 + ${appSlots}`
evCount=1
while true;
do
    if [ -e "${AppslistFile}" ] ; then

        readyCount=$(cat "${AppslistFile}" 2> /dev/null | wc -l)

        if [ "${readyCount}" == "${appSlots}"  ] ; then
            ensure_running_process ${stackName}
            ensure_running_process ${appName}
            echo "All ${readyCount} apps connected and ready!"
            cat "${AppslistFile}"
            exit 0
        fi

        if [ "${readyCount}" -ge  "1" ] ; then
            ensure_running_process ${stackName}
        fi

        ensure_running_process ${appName}

        echo "Apps ready=${readyCount}, and is expected to reach "${appSlots}""
        evCount=`expr 1 + ${evCount}`

        if [ ${evCount} -gt 600  ] ; then
            echo "ERROR: Event-count ${evCount} reached!, giving up on waiting"
            exit 1
        fi
    fi
    sleep 1
    echo "Iterating ${evCount}"

done

echo "Failed for some unknown reason!"
exit 1

