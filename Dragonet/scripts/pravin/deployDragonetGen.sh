#!/bin/bash

show_usage() {
    echo "USAGE: $0 <stackname> <HWqueues> <cost-fn>"
    echo "EXAMPLE: $0 tap 1 balanced"
    echo "EXAMPLE: $0 sf 4 priority"
    echo "EXAMPLE: $0 e10k 10 priority"
    echo "EXAMPLE: $0 null 5 balanced"
}


if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    show_usage
    exit 1
fi
STACKNAME=stack-"${1}"

shift

if [ -z $1 ] ; then
    echo "ERROR: provide number of HW queues"
    show_usage
    exit 1
fi
HWQUEUES=${1}

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

EXTRAENV=""
if [ ${STACKNAME} == "stack-sf" ] ; then
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@
EXTRAENV="LD_PRELOAD=/lib/libciul.so.1.1.1"
fi

if [ ${STACKNAME} == "stack-dpdk" ] ; then
echo "Using dpdk library"
#EXTRAENV="LD_PRELOAD=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/libintel_dpdk.so"  # this does not work, causes seg-fault
INITCMD="export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/ "
fi


nohup ${INITCMD} ; sudo ${EXTRAENV} ./dist/build/${STACKNAME}/${STACKNAME} $@ > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to start"
sleep 4

echo "Waiting for Dragonet to be ready"
${SCRIPTDIR}/wait_for_dragonet.sh  ${HWQUEUES} ${STACKNAME}
#ls *.ready
sleep 10
echo "Dragonet is ready"
cat some.log
exit 0

