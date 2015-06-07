# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

set -x
set -e

sudo tcpdump -n -vv -XX -i eth3 -U -s 1500 'ether host 00:0f:53:07:51:49'


