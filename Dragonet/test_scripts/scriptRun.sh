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
