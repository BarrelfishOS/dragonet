#!/bin/bash

set -x
set -e
SRCDIR=`dirname $0`
export LD_LIBRARY_PATH=${SRCDIR}/libmemcached/.libs/
${SRCDIR}/clients/.libs/lt-memaslap $@

#export LD_LIBRARY_PATH=/cdrom/casper/mount/repository/libmemcached-1.0.18/
#//cdrom/casper/mount/repository/libmemcached-1.0.18/clients/.libs/lt-memaslap

