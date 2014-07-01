#!/bin/bash

./deployPrepare.sh

sudo ./dist/build/llvm-cgen-e10k/llvm-cgen-e10k $@

# Initialized
