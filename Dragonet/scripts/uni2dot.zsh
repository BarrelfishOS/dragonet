#!/usr/bin/env zsh

# This requires cabal 1.20
cabal exec -- ghc -ilib --make mkDot.hs
./mkDot $*
