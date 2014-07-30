#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

APPNAME="stack-sf"
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} $@
sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@

