#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
set -x
set -e
sudo killall -9 stack-tap || true
sudo killall -9 stack-dpdk || true
sudo killall bench-fancyecho || true
sudo killall bench-echo || true
sudo killall -9 stack-e10k || true
sudo killall -9 stack-sf || true
rm -f some.log
sudo bash -c 'echo 700 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages'
#sudo bash -c 'echo 32 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages'
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
for i in `ipcs | grep "root" | grep "666" | grep " 0"  | cut -d' ' -f2` ;
do
	sudo ipcrm -m $i ;
done

sudo rm -rf ./out/*
set +x
set +e
# unsetting so that I don't have to see the list of all the hugepages created
# removing hugpages (assuming they are mounted on /mnt/huge/ location)
sudo rm -rf /mnt/huge/dnetHugemap_*
set -x
set -e


rm -f *.ready
rm -f *.failed
rm -f *.appready
rm -f stack.dnready
rm -f *.dnready
rm -f stack.filtready

# FIXME: make sure that there are enough free HUGE pages
cat /proc/meminfo | grep HugePages_Free


