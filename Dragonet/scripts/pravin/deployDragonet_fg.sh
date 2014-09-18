#!/bin/bash
show_usage() {
    echo "USAGE: $0 <stackname> <HWqueues>"
    echo "EXAMPLE: $0 tap 1"
    echo "EXAMPLE: $0 sf 4"
    echo "EXAMPLE: $0 e10k 10"
    echo "EXAMPLE: $0 null 5"
}

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    show_usage
    exit 1
fi
APPNAME=stack-"${1}"

if [ -z $2 ] ; then
    echo "ERROR: provide the number of HW queues"
    show_usage
    exit 1
fi
HWQUEUES=${2}


SCRIPTDIR="./scripts/pravin/"
sudo rm -rf ./out
${SCRIPTDIR}/deployPrepare.sh
#sudo strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} ${HWQUEUES}
sudo ./dist/build/${APPNAME}/${APPNAME} ${HWQUEUES}

# Initialized

