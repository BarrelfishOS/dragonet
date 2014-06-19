#!/bin/bash

# Usage:
# ./concurrent_tpbench.sh <N> <D> <NP>
#   N: Number of instances
#   D: Destination IP
#   NP: Number of ports (must have N % NP == 0)
#
#
# N udp_tpbench instances will be started with the i'th instance pinned to the
# i'th core. The NP ports will be distributed evenly across instances.

N=$1
D=$2
NP=$3

if [ -z $N ] ; then
    echo No N parameter
    exit 1
fi
if [ -z $D ] ; then
    echo No D parameter
    exit 1
fi
if [ -z $NP ] ; then
    echo No NP parameter
    exit 1
fi
if [ "$(($N % $NP))" != "0" ] ; then
    echo "N % NP != 0"
    exit 1
fi

PIDS=""
OUTFS=""
BASEPORT=7
step=$(($N/NP))
for i in $(seq 0 $(($N - 1))) ; do
    f=$(mktemp)
    p=$(($BASEPORT + $i/$step))
    taskset -c $i ./udp_tpbench -w 10 -u 10 -d 10 -t 30 -m 500 $D:$p | grep -v Throughput >$f &
    PIDS="$PIDS $!"
    OUTFS="$OUTFS $f"
done

for p in $PIDS ; do
    wait $p
done

cat $OUTFS
rm -f $OUTFS

