cabal sandbox init
cabal sandbox add-source ../external/bulktransfer
cabal sandbox add-source ../external/dpdk
cabal install -j --only-dependencies
