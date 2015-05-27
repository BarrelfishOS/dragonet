#!/bin/bash
set -x
set -e

find ../ -name "dist" | xargs rm -rf
rm -rf .cabal-sandbox
./prepare_sandbox.sh
cabal build stack-sf stack-e10k-dpdk bench-fancyecho
