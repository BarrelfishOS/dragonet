#!/bin/bash
set -x
set -e

sudo killall llvm-cgen || true
sudo killall llvm-cgen-e10k || true
sudo killall llvm-cgen-sf || true
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
for i in `ipcs | grep "root" | grep "666" | grep " 0"  | cut -d' ' -f2` ;
do
	sudo ipcrm -m $i ;
done

cat  /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages
cat /proc/meminfo | grep HugePages
cat /proc/meminfo | grep HugePages_Free
#sudo dist/build/llvm-cgen/llvm-cgen lpgImpl.unicorn
sudo dist/build/llvm-cgen/llvm-cgen $@

