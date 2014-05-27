#!/bin/bash
set -x
set -e
sudo killall llvm-cgen-e10k || true
appName="AppEcho"
sudo rm -rf /dev/shm/dragonet*
sudo rm -rf /dev/shm/bulk_pool_*
cat /proc/meminfo | grep HugePages_Free
pwd
file ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k
nohup sudo ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $appName > some.log 2>&1 < /dev/null  &
sleep 3
echo "Done with lunching Dragonet network stack"
exit 0
