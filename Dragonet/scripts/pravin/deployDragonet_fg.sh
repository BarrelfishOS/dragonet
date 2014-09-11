#!/bin/bash

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    echo "USAGE: $0 <stackname>"
    echo "EXAMPLE: $0 tap"
    echo "EXAMPLE: $0 sf"
    echo "EXAMPLE: $0 e10k"
    echo "EXAMPLE: $0 null"
    exit 1
fi
APPNAME=stack-"${1}"

SCRIPTDIR="./scripts/pravin/"
sudo rm -rf ./out
${SCRIPTDIR}/deployPrepare.sh
#sudo strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} $@
sudo ./dist/build/${APPNAME}/${APPNAME} $@

# Initialized
