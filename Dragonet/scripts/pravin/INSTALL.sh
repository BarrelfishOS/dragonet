# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash


# for Dragonet
sudo apt-get install -y ghc cabal-install clang graphviz pdftk libghc-parsec2-dev libghc-stm-dev minisat
cabal update
cabal install graphviz
cabal install fgl
cabal install MonadRandom
cabal install pretty-show
cabal install mtl
cabal install random


