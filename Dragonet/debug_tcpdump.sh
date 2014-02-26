#!/bin/bash
# sudo tcpdump -n -i dragonet0 tcp
sudo tcpdump -n -i dragonet0 tcp -U -s 1500 -w - | tee rawPacketLog.log | tcpdump -lnr -

# use following command to analyze the packet logs with tcpdump.
# wireshark rawPacketLog.log

