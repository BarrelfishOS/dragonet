#!/bin/bash

cabal sandbox init
if [ $? != 0 ]; then
	echo "$0: cabal: ($(which cabal)) does not support sandboxes"
	exit 1
fi

cabal configure --enable-executable-profiling
cabal sandbox add-source ../external/barrelfish
cabal sandbox add-source ../external/shmchan
cabal sandbox add-source ../external/bulktransfer
cabal sandbox add-source ../external/dpdk
cabal sandbox add-source ../external/smtLib
cabal sandbox add-source ../external/Hsmtlib/src
cabal sandbox add-source ./apps

z3dir=$(readlink ../z3)
if [ $? != 0 ]; then
	echo "$0: expecting symbolink link at $(pwd)/../z3"
	exit 1
fi


cabal install z3 --extra-include-dirs=$z3dir/include --extra-lib-dirs=$z3dir/bin
cabal install -j --only-dependencies  --enable-documentation
