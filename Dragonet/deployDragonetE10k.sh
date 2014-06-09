#!/bin/bash
set -x
set -e
sudo killall llvm-cgen-e10k || true
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*

for i in `ipcs | grep "root" | grep "666" | grep " 0"  | cut -d' ' -f2` ;
do
	sudo ipcrm -m $i ;
done

cat /proc/meminfo | grep HugePages_Free
nohup sudo ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $@ > some.log 2>&1 < /dev/null  &
sleep 5
echo "Done with lunching Dragonet network stack"
exit 0

