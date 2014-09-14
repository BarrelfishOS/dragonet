# you might need something like:
# cabal install z3 --extra-include-dirs=/opt/z3/include/ --extra-lib-dirs=/opt/z3/bin/
# for Z3
cabal sandbox init
cabal configure --enable-executable-profiling
cabal sandbox add-source ../external/barrelfish
cabal sandbox add-source ../external/shmchan
cabal sandbox add-source ../external/bulktransfer
cabal sandbox add-source ../external/dpdk
cabal sandbox add-source ../external/smtLib
cabal sandbox add-source ../external/Hsmtlib/src
cabal sandbox add-source ./apps
cabal install -j --only-dependencies
