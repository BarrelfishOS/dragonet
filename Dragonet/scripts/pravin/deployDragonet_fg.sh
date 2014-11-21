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
    echo "ERROR: provide the number of HW queues"
    show_usage
    exit 1
fi
HWQUEUES=${1}

SCRIPTDIR="./scripts/pravin/"
sudo rm -rf ./out
${SCRIPTDIR}/deployPrepare.sh

EXTRAENV=""
INITCMD=""
if [ ${STACKNAME} == "stack-sf" ] ; then
EXTRAENV="LD_PRELOAD=/lib/libciul.so.1.1.1"
fi

if [ ${STACKNAME} == "stack-dpdk" ] ; then
echo "Using dpdk library"
#EXTRAENV="LD_PRELOAD=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/libintel_dpdk.so"  # this does not work, causes seg-fault
INITCMD="export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.7.1/build/lib/ "
fi

set -x
set -e

#sudo strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} ${HWQUEUES}
${INITCMD} ; sudo ${EXTRAENV} ./dist/build/${STACKNAME}/${STACKNAME} $@
#${INITCMD} ; sudo strace -fCrtT ${EXTRAENV} ./dist/build/${STACKNAME}/${STACKNAME} $@

# Initialized

