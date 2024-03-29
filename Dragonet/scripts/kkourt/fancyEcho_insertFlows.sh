# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

nflows=2

fancyecho="./dist/build/bench-fancyecho/bench-fancyecho"
lport=4444 # local (listen) port
rip="192.168.123.100" # remote (dummy ip)
lip="192.168.123.1"   # local ip

flows=""
for rport in $(seq 1 $nflows)
do
	flows="${flows} -F ${lip}:${lport}/${rip}:${rport}"
done

echo ${fancyecho} -a t0 -p $lport $flows -t -q t0
     ${fancyecho} -a t0 -p $lport $flows -t -q t0
