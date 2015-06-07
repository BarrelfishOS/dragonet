# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash
nc.traditional -n -vv -s 10.113.4.38 -u -p 5555 10.113.4.71 5556

# ping -I eth3 10.113.4.71
# # to find the proper interface
# ifconfig -a | grep "00:1b:21:8f:18:65"

