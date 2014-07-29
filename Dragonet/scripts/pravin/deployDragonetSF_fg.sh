#!/bin/bash

./deployPrepare.sh

#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 strace -fCrtT ./dist/build/llvm-cgen-sf/llvm-cgen-sf $@
sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/llvm-cgen-sf/llvm-cgen-sf $@

