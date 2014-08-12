#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
set -x
set -e
sudo killall stack-tap || true
sudo killall bench-fancyecho || true
sudo killall bench-echo || true
sudo killall stack-e10k || true
sudo killall stack-sf || true
rm -f some.log
sudo bash -c 'echo 128 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages'
#sudo bash -c 'echo 32 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages'
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
for i in `ipcs | grep "root" | grep "666" | grep " 0"  | cut -d' ' -f2` ;
do
	sudo ipcrm -m $i ;
done

rm -f *.ready
rm -f *.failed
rm -f *.appready
rm -f stack.dnready
# FIXME: make sure that there are enough free HUGE pages
cat /proc/meminfo | grep HugePages_Free


