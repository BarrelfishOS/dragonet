#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh
rm -f some.log
hwQcount=${1}

APPNAME="stack-sf"

nohup sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@ > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to be ready"
${SCRIPTDIR}/wait_for_dragonet.sh  ${hwQcount} ${APPNAME}
#ls *.ready
sleep 10
echo "Dragonet is ready"
cat some.log
exit 0

