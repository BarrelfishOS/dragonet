#!/bin/bash

./deployPrepare.sh
rm -f some.log
hwQcount=${1}

nohup sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/llvm-cgen-sf/llvm-cgen-sf $@ > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to be ready"
./wait_for_dragonet.sh  ${hwQcount}
ls *.ready
sleep 2
echo "Dragonet is ready"
cat some.log
exit 0

