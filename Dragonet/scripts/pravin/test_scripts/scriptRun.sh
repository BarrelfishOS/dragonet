# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash
sudo ls > /dev/null
sudo ./tuntap &
TUNTAP_PID=$!
sleep 1
./debug_tcpdump.sh &
TCPDUMP_PID=$!
sleep 1
./test_tuntap_nc_tcp.sh
# do other stuff
sudo kill $TCPDUMP_PID
sudo kill $TUNTAP_PID
sudo killall tuntap
sudo killall tcpdump

cat rawPacketLog.log | tcpdump -lnr -

#wireshark rawPacketLog.log
