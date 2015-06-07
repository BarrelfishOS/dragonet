# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash
# sudo tcpdump -n -i dragonet0 tcp
sudo tcpdump -n -i dragonet0 -U -s 1500 -w - | tee rawPacketLog.log | tcpdump -lnr -

# use following command to analyze the packet logs with tcpdump.
# wireshark rawPacketLog.log
# cat rawPacketLog.log | tcpdump -lnr -

