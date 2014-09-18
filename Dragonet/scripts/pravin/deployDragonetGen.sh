#!/bin/bash

show_usage() {
    echo "USAGE: $0 <stackname> <hwCount>"
    echo "EXAMPLE: $0 tap 1"
    echo "EXAMPLE: $0 sf 4"
    echo "EXAMPLE: $0 e10k 4"
    echo "EXAMPLE: $0 null 4"
}

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    show_usage
    exit 1
fi
APPNAME=stack-"${1}"

if [ -z $2 ] ; then
    echo "ERROR: provide number of HW queues"
    show_usage
    exit 1
fi
HWQUEUES=${2}

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

nohup sudo ./dist/build/${APPNAME}/${APPNAME} ${HWQUEUES} > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to start"
sleep 4

echo "Waiting for Dragonet to be ready"
${SCRIPTDIR}/wait_for_dragonet.sh  ${HWQUEUES} ${APPNAME}
#ls *.ready
sleep 10
echo "Dragonet is ready"
cat some.log
exit 0

