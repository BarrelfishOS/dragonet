cabal sandbox init
cabal sandbox add-source ../external/barrelfish
cabal sandbox add-source ../external/shmchan
cabal sandbox add-source ../external/bulktransfer
cabal sandbox add-source ../external/dpdk
cabal sandbox add-source ../external/smtLib
cabal sandbox add-source ./apps
cabal install -j --only-dependencies
