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
STACKNAME=stack-"${1}"

if [ -z $2 ] ; then
    echo "ERROR: provide number of HW queues"
    show_usage
    exit 1
fi
HWQUEUES=${2}

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

EXTRAENV=""
if [ ${STACKNAME} == "stack-sf" ] ; then
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@
EXTRAENV="LD_PRELOAD=/lib/libciul.so.1.1.1"
fi

nohup sudo ${EXTRAENV} ./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to start"
sleep 4

echo "Waiting for Dragonet to be ready"
${SCRIPTDIR}/wait_for_dragonet.sh  ${HWQUEUES} ${STACKNAME}
#ls *.ready
sleep 10
echo "Dragonet is ready"
cat some.log
exit 0

