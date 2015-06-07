# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

set -x
set -e

IFNAME="p801p1"

sudo tc qdisc add dev ${IFNAME} root handle 1: prio priomap 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
sleep 1
sudo tc qdisc add dev ${IFNAME} parent 1:1 handle 10: pfifo
sleep 1
sudo tc qdisc add dev ${IFNAME} parent 1:2 handle 20: pfifo
sleep 1
sudo tc qdisc add dev ${IFNAME} parent 1:3 handle 30: pfifo
sleep 1

sudo tc -s qdisc ls dev ${IFNAME}
sleep 3

sudo tc filter add dev ${IFNAME} protocol ip parent 1: prio 1 u32 match ip dst 10.113.4.51/32 match ip dport 8000 0xffff flowid 1:1
sleep 1
sudo tc filter add dev ${IFNAME} protocol ip parent 1: prio 1 u32 match ip dst 10.113.4.57/32 match ip dport 8000 0xffff flowid 1:1
sleep 1


