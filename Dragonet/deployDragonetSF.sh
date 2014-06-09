#!/bin/bash
set -x
sudo killall llvm-cgen-e10k || true
sudo killall llvm-cgen-sf || true
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
for i in `ipcs | grep "root" | grep "666" | grep " 0"  | cut -d' ' -f2` ;
do
	sudo ipcrm -m $i ;
done

cat /proc/meminfo | grep HugePages_Free
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 gdb ./dist/build/llvm-cgen-sf/llvm-cgen-sf $1
nohup sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/llvm-cgen-sf/llvm-cgen-sf $@ > some.log 2>&1 < /dev/null  &
sleep 5
echo "Done with lunching Dragonet network stack"
exit 0

