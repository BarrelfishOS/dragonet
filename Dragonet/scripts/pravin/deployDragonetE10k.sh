# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh
hwQcount=${1}

APPNAME="stack-e10k"
nohup sudo ./dist/build/${APPNAME}/${APPNAME} $@ > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to be ready"
${SCRIPTDIR}/wait_for_dragonet.sh  ${hwQcount} ${APPNAME}
#ls *.ready
sleep 10
echo "Dragonet is ready"
cat some.log
exit 0

