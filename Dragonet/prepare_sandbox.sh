cabal sandbox init
cabal sandbox add-source ../external/barrelfish
cabal sandbox add-source ../external/shmchan
cabal sandbox add-source ../external/bulktransfer
cabal sandbox add-source ../external/dpdk
cabal install -j --only-dependencies
