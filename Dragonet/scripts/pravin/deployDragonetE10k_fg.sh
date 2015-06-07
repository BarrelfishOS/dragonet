# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
sudo rm -rf ./out
${SCRIPTDIR}/deployPrepare.sh
APPNAME="stack-e10k"
#sudo strace -fCrtT ./dist/build/${APPNAME}/${APPNAME} $@
sudo ./dist/build/${APPNAME}/${APPNAME} $@

# Initialized
