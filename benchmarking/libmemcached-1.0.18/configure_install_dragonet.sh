#!/bin/bash
set -x
set -e
LIBS=-lpthread ./configure --enable-memaslap || true
automake-1.13 --add-missing
make
sudo make install
sudo cp ./libmemcached/.libs/libmemcached.so.11.0.0 /usr/lib/x86_64-linux-gnu/libmemcached.so.11

