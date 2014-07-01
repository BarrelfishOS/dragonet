#!/bin/bash

./deployPrepare.sh

hwQcount=${1}

nohup sudo ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $@ > some.log 2>&1 < /dev/null  &

echo "Waiting for Dragonet to be ready"
./wait_for_dragonet.sh  ${hwQcount}
ls *.ready
sleep 2
echo "Dragonet is ready"
exit 0


