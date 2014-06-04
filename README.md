To compile, do:

```
cabal sandbox init
cabal install cabal-install
cabal install --only-dep
```

. This will install Hakyll, Clay, and other dependencies.
Run the `build.sh` script.

A useful trick is to do

```
./blog clean && ./build.sh && ./blog watch
```

for a complete reset.
