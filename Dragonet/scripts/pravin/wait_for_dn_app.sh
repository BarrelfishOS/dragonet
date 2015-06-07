# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

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

check_filters_inserted() {
    fcount=${1}
    local fcountTries=1
    local filterLogFile="stack.filtready"
    while true;
    do
        if [ -f "${filterLogFile}" ] ;
        then

            #filter_count=`cat ${filterLogFile} | grep "\[####-- IMP --####\]" | wc -l`
            filter_count=`cat ${filterLogFile} | wc -l`
            if [ $filter_count -ge $fcount ] ;
            then
                echo "Found enough filters $filter_count (>= $fcount)"
                echo "  assuming that all filters are inserted, continuing ..."
                return
                break
            fi
            echo "Only ${filter_count} filters are inserted, and we want around ${fcount}, sleeping.. "
        else
            echo "The file ${filterLogFile} is not there yet, so no application is connected yet. sleeping.. "
        fi

        fcountTries=`expr 1 + ${fcountTries}`
        if [ ${fcountTries} -gt 4600  ] ; then
            echo "ERROR: filter count retries ${fcountTries} reached!, giving up on waiting"
            exit 1
        fi
        sleep 1
    done
}


# the tool should get only one number as commandline arg
if [ $# != 5 ] ; then
    echo "ERROR: incomplete agruments"
    echo "USAGE: $0 <stack-name>  <hw queues> <no. of application slots> <app-name> <flowCount>"
    echo "EXAMPLE: $0 stack-sf 1 1 bench-fancyecho 1"
    echo "EXAMPLE: $0 stack-e10k 4 4  memcached 200"
    echo "EXAMPLE: $0 stack-dpdk 4 4  memcached 20"
fi

stackName=${1}
hwQueue=${2}
appSlots=${3}
appName=${4}
flowCount=${5}

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

        if [ "${readyCount}" -ge "${appSlots}"  ] ; then
            ensure_running_process ${stackName}
            ensure_running_process ${appName}
            echo "All ${readyCount} apps connected and ready!"
            cat "${AppslistFile}"
            if [ $flowCount -gt 0 ] ; then
                check_filters_inserted  $flowCount
            fi
            echo "sleeping for short 3 secs"
            sleep 3
            exit 0
        fi

        if [ "${readyCount}" -ge  "1" ] ; then
            ensure_running_process ${stackName}
        fi

        ensure_running_process ${appName}

        echo "Apps ready=${readyCount}, and is expected to reach "${appSlots}""
        evCount=`expr 1 + ${evCount}`

        if [ ${evCount} -gt 4600  ] ; then
            echo "ERROR: Event-count ${evCount} reached!, giving up on waiting"
            exit 1
        fi
    fi
    sleep 1
    echo "Iterating ${evCount}"

done

echo "Failed for some unknown reason!"
exit 1

