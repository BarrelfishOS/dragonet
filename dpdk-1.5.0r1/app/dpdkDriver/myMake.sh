#!/bin/bash
set -e
set -x
export RTE_TARGET=x86_64-default-linuxapp-gcc
export RTE_SDK=/home/ubuntu/dragonet/dpdk-1.5.0r1
make

