# About

This is the source code for [my blog](http://listx.github.io/).

# Compiling

To compile, do:

```
cabal sandbox init
cabal install cabal-install
cabal install --only-dep
```

. This will install Hakyll, Clay, and other dependencies.
Now just run either

```
make
```

for a complete reset.

If you don't want to create the static site directory itself into the `listx.github.io/` directory, just run

```
make nosync
```

instead.
