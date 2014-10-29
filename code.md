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

### quicksilver

Quicksilver (Haskell) is a program that generates two mods for the real time strategy games *Rome: Total War (2004)* and *Medieval II: Total War (2006)* from the game studio *The Creative Assembly*.
The *qs* command line program generates the `quicksilverRTW` and `quicksilverM2TW` mods from the game data sources.
If any of the sources cannot be found or have a SHA-1 checksum mismatch, nothing is generated.

- [Git repository](https://github.com/listx/quicksilver)
- The latest source code in [zip](https://github.com/listx/quicksilver/zipball/master) or [tar](https://github.com/listx/quicksilver/tarball/master)

While interesting from a developer's perspective (it being written in Haskell, perhaps the only such Haskell tool in existence in the Total War modding community), if you are an actual player of either of these games, you should check out the actual mods that `qs` generates.
For either one, use 7zip to extract everything into your game root directory, and run the mod with the included `.bat` file.
Both have been tested to work with the game binary from [Steam](http://store.steampowered.com/).

- [Public quicksilver mods folder](https://drive.google.com/folderview?id=0BxczAgYXbLfJZjNtcENUekNlMm8&usp=sharing)

## Libraries

### ZTile

ZTile (Haskell) is a library for dealing with square or hexagon tiles in a game map.

- [Git repository](https://github.com/listx/ztile)
- The latest source code in [zip](https://github.com/listx/ztile/zipball/master) or [tar](https://github.com/listx/ztile/tarball/master)
- [Manual](file/ztile-0.1.0-10-g6211a7c.pdf)[^gendoc]

## Utilities

### auca

Auca (Haskell/Linux) watches for file modifications and executes a given external command.
The name comes from a contraction of "auto" and "call".

- [Git repository](https://github.com/listx/auca)
- The latest source code in [zip](https://github.com/listx/auca/zipball/master) or [tar](https://github.com/listx/auca/tarball/master)
- [Manual](file/auca-0.0.1.4-0-gfa4e4dc.pdf)

### cabal2pkgbuild

cabal2pkgbuild (Haskell/Arch Linux) is an Arch Linux wrapper around [cblrepo](https://github.com/magthe/cblrepo) which lets you generate PKGBUILD files from [Hackage packages](http://hackage.haskell.org/packages/).

- [Git repository](https://github.com/listx/cabal2pkgbuild)
- The latest source code in [zip](https://github.com/listx/cabal2pkgbuild/zipball/master) or [tar](https://github.com/listx/cabal2pkgbuild/tarball/master)
- [AUR](https://aur.archlinux.org/packages/cabal2pkgbuild-git/)

### floop

Floop (C/Linux) is a PRNG that writes to STDOUT.
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

### panxor

Panxor (Haskell/Linux) can perform a numerical XOR against an arbitrary number of hashes of arbitrary length in base 2 (binary), 8 (octal), 10 (digital), and 16 (hexadecimal).
It is primarily meant to be used on top of a hashing utility like sha1sum; the idea is to combine all of the SHA1 hashes of the files of concern into 1 single hash by XOR-ing them all together; this way, as long as the number of files and filenames in question don't change, you can instantly tell with a single hash whether something has changed in between.
From this use case, it is like a poor man's git commit hash.

- [Git repository](https://github.com/listx/panxor)
- The latest source code in [zip](https://github.com/listx/panxor/zipball/master) or [tar](https://github.com/listx/panxor/tarball/master)

### emacs

#### kakapo-mode

Simple, stupid indentation.
The kakapo is a species of parrot in New Zealand; intelligence is not its strong suit.

- [Git repository](https://github.com/listx/kakapo-mode)
- The latest source code in [zip](https://github.com/listx/kakapo/zipball/master) or [tar](https://github.com/listx/kakapo/tarball/master)
- [AUR](https://aur.archlinux.org/packages/emacs-kakapo-mode-git/)

[^gendoc]: The manual, provided here as a convenience, can be generated from a cloned git repository of the development sources if you have [TexLive](http://www.tug.org/texlive/) installed on your system.
