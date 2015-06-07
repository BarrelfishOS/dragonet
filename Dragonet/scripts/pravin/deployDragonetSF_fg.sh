# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

set -x
set -e

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

APPNAME="stack-sf"
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} $@
sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@

