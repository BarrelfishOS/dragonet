# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash
RANDOMPORT=51099
# Echo on DNS port works
DNSPORT=51098

# Echo on echo port does not works
ECHOPORT=5556

#nc.traditional -n -vv -u -p 5555 192.168.123.1 "${ECHOPORT}"
nc.traditional -n -vv -u -p 5555 192.168.123.1 "${DNSPORT}"

