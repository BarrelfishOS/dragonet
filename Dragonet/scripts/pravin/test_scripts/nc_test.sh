# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

# on client side (ziger2)
nc.traditional -n -vv -s 10.111.4.36 -u -p 5555 10.111.4.37 5556

