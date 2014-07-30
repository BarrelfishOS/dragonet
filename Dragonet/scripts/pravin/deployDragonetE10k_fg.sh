#!/bin/bash

SCRIPTDIR="./scripts/pravin/"

${SCRIPTDIR}/deployPrepare.sh
APPNAME="stack-e10k"
#sudo strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} $@
sudo ./dist/build/${APPNAME}/${APPNAME} $@

# Initialized
