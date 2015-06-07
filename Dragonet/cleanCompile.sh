# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash
set -x
set -e

find ../ -name "dist" | xargs rm -rf
rm -rf .cabal-sandbox
./prepare_sandbox.sh
cabal build stack-sf stack-e10k-dpdk bench-fancyecho
