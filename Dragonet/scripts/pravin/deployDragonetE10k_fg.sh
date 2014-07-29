#!/bin/bash

./deployPrepare.sh


#sudo strace -fCrtT ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $@
sudo ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $@

# Initialized
