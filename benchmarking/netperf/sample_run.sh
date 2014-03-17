#!/bin/bash

run_bm() {
    dstat -t -n -N ${IFACE}
    ./run_echo_bm.sh -g -m ${REMOTEHOST} -t 5 -p 1000 -b 150
    killall dstat
}

dstat -t -n -N dragonet0
./run_echo_bm.sh -g -m 192.168.123.1 -t 5 -p 1000 -b 150
killall dstat

dstat -t -n -N lo
./run_echo_bm.sh -g -m localhost -t 5 -p 1000 -b 150
killall dstat

dstat -t -n -N eth0
./run_echo_bm.sh -g -m base-station.ethz.ch -t 5 -p 1000 -b 150
killall dstat


