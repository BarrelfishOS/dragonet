#!/bin/bash

set -x
set -e
sudo killall llvm-cgen || true
sudo killall bench-fancyecho || true
sudo killall bench-echo || true
sudo killall llvm-cgen-e10k || true
sudo killall llvm-cgen-sf || true
sudo bash -c 'echo 32 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages'
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


