---
title: Code
---

Here is a list of pet programming projects.
Let me know if any of them suffers from bit rot.

## Games

### simtic

Simtic (C/Linux) is a text-based, dead-simple tic-tac-toe game with an AI that uses minimax search.

- [Git repository](https://github.com/listx/simtic)
- The latest source code in [zip](https://github.com/listx/simtic/zipball/master) or [tar](https://github.com/listx/simtic/tarball/master)

## Libraries

### ZTile

ZTile (Haskell) is a library for dealing with square or hexagon tiles in a game map.

- [Git repository](https://github.com/listx/ztile)
- The latest source code in [zip](https://github.com/listx/ztile/zipball/master) or [tar](https://github.com/listx/ztile/tarball/master)
- [Manual](file/ztile-0.1.0-10-g6211a7c.pdf)[^gendoc]

## Utilities

### auca

Auca (Haskell/Linux) watches for file modifications and execute a given external command.
The name comes from a contraction of "auto" and "call".

- [Git repository](https://github.com/listx/auca)
- The latest source code in [zip](https://github.com/listx/auca/zipball/master) or [tar](https://github.com/listx/auca/tarball/master)
- [Manual](file/auca-0.1.2-0-g7424b49.pdf)

### cabal2pkgbuild

cabal2pkgbuild (Haskell/Arch Linux) is an Arch Linux wrapper around [cblrepo](https://github.com/magthe/cblrepo) which lets you generate PKGBUILD files from [Hackage packages](http://hackage.haskell.org/packages/).

- [Git repository](https://github.com/listx/cabal2pkgbuild)
- The latest source code in [zip](https://github.com/listx/cabal2pkgbuild/zipball/master) or [tar](https://github.com/listx/cabal2pkgbuild/tarball/master)
- [AUR](https://aur.archlinux.org/packages/cabal2pkgbuild-git/)

### floop

Floop (C/Linux) is a PRNG that spits out to STDOUT.
Its main purpose is to generate random bytes to wipe a disk, before using it as an encrypted medium.

- [Git repository](https://github.com/listx/floop)
- The latest source code in [zip](https://github.com/listx/floop/zipball/master) or [tar](https://github.com/listx/floop/tarball/master)
- [AUR](https://aur.archlinux.org/packages/floop-git/)

### nox

Nox (Haskell/Linux) comments/uncomments text from STDIN and prints the result to STDOUT.
The name comes from "no *x*", where *x* is a chunk of source code that you'd want to comment out.

- [Git repository](https://github.com/listx/nox)
- The latest source code in [zip](https://github.com/listx/nox/zipball/master) or [tar](https://github.com/listx/nox/tarball/master)
- [Manual](file/nox-0.1.0-4-gf123c9e.pdf)
- [Sample use case](post/2013-04-30-emacs-unix-filter.html)

[^gendoc]: The manual, provided here as a convenience, can be generated from a cloned git repository of the development sources if you have [TexLive](http://www.tug.org/texlive/) installed on your system.
