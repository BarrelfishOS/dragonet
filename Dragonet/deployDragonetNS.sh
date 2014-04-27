#!/bin/bash
set -x
set -e
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
cat  /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages
cat /proc/meminfo | grep HugePages
cat /proc/meminfo | grep HugePages_Free
sudo dist/build/llvm-cgen/llvm-cgen lpgImpl.unicorn

