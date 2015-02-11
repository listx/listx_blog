---
title: Using the Nix Package Manager for Haskell Development from Arch Linux
tags: programming, haskell, arch, linux
mathjax: off
---

I recently installed and configured NixOS on a laptop, and had to learn how to develop Haskell on there.
The Nix community uses something called `cabal2nix` and `nix-shell` to get the job done.
While things work quite smoothly right now in NixOS, I was wondering if I could do the same on my Arch Linux box.

The answer is yes --- you can easily use Nix to create a 'system sandbox' of sorts (the Nix store) that is completely isolated from Arch's own Haskell packages/GHC.
To be clear, what we are trying to do is install the Nix package manager (which is composed of many satellite programs like `nix-env`, `nix-shell`, etc.) so that we can develop Haskell programs with all the advantages that come with it.

For myself, I have several different Haskell projects, and I wanted to avoid redownloading and recompiling the same packages for each project's Cabal sandbox environment.
Using Nix, I still have the same Cabal sandboxes (one for each project root), but Nix allows all the different sandboxes to share the same packages if the versions and dependencies are the same.
And plus, because the Nix store (where Nix stores everything --- `/nix/store`) is independent of Arch's `pacman` tool, there is no fear of conflict or things breaking whenever you upgrade Arch Linux's own Haskell packages.

# Use the Nix Manual to install Nix

The Nix manual has up-to-date documentation on how to get Nix.

TODO: create groups/users, and start nix-daemon, and also source the shell script (my alias to getnix)

# Install `cabal2nix`

You can list all Nix-packaged packages with `nix-env -qaP`.
For us, we're interested in the `cabal2nix` package.
As of the time of this writing, it is called `nixpkgs.haskellPackages.cabal2nix`.
However, the `haskellPackages` prefix refers to the old system that has been more or less deprecated as of January 2015.
We need to use teh `haskellngPackages` prefix instead.
I know that that `nixpkgs.haskellngPackages.cabal2nix` isn't listed with the `nix-env -qaP` command, but I believe that is for legacy reasons.
You can still install it!
Let's do that now:

```
nix-env -iA nixpkgs.haskellngPackages.cabal2nix
```

.
This will give you the very useful `cabal2nix` binary which you can use to convert any `.cabal` file into something that Nix can understand!

# Nixify your project

## Create a `.cabal` file

If you haven't done so already, create a Cabal file `your_project.cabal` to describe the dependencies in the traditional Haskell way.
This step is mandatory!

## Create a `shell.nix` file

Go to your project's root folder that contains `your_project.cabal`, and do

```
cabal2nix --shell . > shell.nix
```

.
Now just invoke

```
nix-shell
```

and you're set.
You will be dropped into a `bash` instance that has knowledge of the Nix store.
The first time your run `nix-shell`, Nix will identify any missing dependencies and install them for you.

# Local dependencies

If you're like me, you might have a Haskell library you wrote for yourself which is not on Hackage.

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx
# Troubleshooting

## Create a `shell.nix` file

## Plaintext

```
Blah blah blah.
```

## List items

- apple
- banana
- candy

## Raw code insertion

- i toy/parking-space.rb
