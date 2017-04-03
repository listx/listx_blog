#!/usr/bin/env bash

pushd c
gcc -c -o gcd.o gcd.c
popd

pushd hs
ghc --make ffi.hs ../c/gcd.o
popd
