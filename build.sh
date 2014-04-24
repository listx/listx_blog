#!/usr/bin/env zsh

cabal sandbox init
cabal build
cp dist/build/blog/blog .
./blog build
./sync.sh
