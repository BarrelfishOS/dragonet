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


