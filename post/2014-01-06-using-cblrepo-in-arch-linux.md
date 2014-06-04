---
title: Using cblrepo in Arch Linux
tags: arch, linux, haskell, cblrepo
---

UPDATE 2014-01-24: See [this post](2014-01-24-cabal2pkgbuild.html).

The [cblrepo](https://github.com/magthe/cblrepo) command line utility helps to maintain a set of Haskell packages, and is used as the de facto Hackage package manager in Arch Linux.

At least in Arch Linux, the idea is to first install the base set of available Haskell packages from the [[haskell-core]](https://wiki.archlinux.org/index.php/Haskell_Package_Guidelines) repo.
If you now want to install a package from http://hackage.haskell.org *and* want to track it with Arch Linux's own package manager `pacman`, you have to somehow generate a PKGBUILD from the Hackage package and feed it into `makepkg`, and then call `sudo pacman -U <generated_package>` on it.
You can do this with `cblrepo`, but it takes a number of steps that could get very tedious, very fast:

1. Sync all the packages on Hackage with `cblrepo`, via `cblrepo sync`.
2. Initialize the `cblrepo.db` file, which is generated automatically each time you execute a `cblrepo add ...` command.
3. Add all of the standard packages that comes with installing GHC (`ghc` in Arch Linux) with `cblrepo add --ghc-pkg ...`.
4. Add all the packages installed by yourself from the [haskell-core] repository with `cblrepo add --distro-pkg ...`
5. Add the `<package_name>.cabal` URL from the Hackage package's page, which is in the format `http://hackage.haskell.org/package/<package_name>`.[^search]
6. Run `cblrepo pkgbuild <package_name>`.
7. Go into the generated `haskell-<package_name_lowercased>` directory which contains the `PKGBUILD` we are after, and run `makepkg -s`.
8. Finally run `sudo pacman -U <package_arch_linux_format>` on it to track it with `pacman`.

The problem is that steps 3 and 4 take forever to do by hand because you have to add every single package individually, writing out the version numbers (and, it is something of a mystery unless you know where to look for the crucial information).[^whereisit]
Plus, you have to redo all steps 1 through 7 each time you upgrade `ghc`.
So, naturally, I wrote a script (called `maintain_cblrepo.sh`) to automate things a bit.

```{.numberLines .bash}
#!/usr/bin/env zsh
# LICENSE: PUBLIC DOMAIN
# Usage: ./maintain_cblrepo.sh <HACKAGE_PACKAGES_FILE> <MODE>
usage="Usage: ./maintain_cblrepo.sh <HACKAGE_PACKAGES_FILE> <MODE>"

# Exit immediately if any errors are found
setopt errexit
# Avoid "no matches found" error if a file does not exist; see
# http://www.zsh.org/mla/users/2008/msg01139.html
setopt local_options no_nomatch

if [[ -z $1 ]]; then
	echo $usage
	exit 1
fi
if [[ ! -f $1 ]]; then
	echo "\`$1' does not exist or is not a regular file"
	exit 1
fi
if [[ -z $2 ]]; then
	echo $usage
	exit 1
fi

hackage_url="http://hackage.haskell.org"
hackage_packages_file=($(<$1))

mode=$2

case $mode in
	### Remove any old cblrepo.db file. ###
	(initdb|initdb-sync)
	rm -f cblrepo.db

	# Add packages provided by GHC

	# Pacman provides information about which modules are exposed by installing the
	# 'ghc' package. We put each package into an array.
	provided=($(pacman -Qi ghc | grep Provides | cut -d ":" -f2))

	for p in $provided; do
		# Change the syntax to be compatible with cblrepo. The `cut` command here
		# removes the 'haskell-' prefix for each package, and `sed` here replaces
		# each '=' sign with a ',', as per cblrepo's requirements.
		package=$(echo $p | cut -c9- | sed 's/=/,/')
		command="cblrepo add --ghc-pkg $package"
		# Tell user what we are going to do.
		echo $command
		# Actually execute the command.
		eval $command
	done

	# Add packages installed by the user from [haskell-core] or some other Arch Linux repository
	installed=($(pacman -Qq | grep "^haskell-" | sed 's/^haskell-//'))
	# Filter out those packages that were installed from Hackage using this very
	# same script (in Arch Linux, the hackage packages, once installed, are in
	# the format `haskell-<lowercased_package_name>'). This way, we avoid
	# duplicate definitions and the packages added with --distro-pkg will really
	# be those packages available from the distribution's official haskell
	# repository.
	hackage_lowercased=($hackage_packages_file:l)
	installed_filtered=(${installed:|hackage_lowercased})

	for p in $installed_filtered; do
		version=$(pacman -Q haskell-$p | cut -d " " -f2 | sed 's/-/,/')
		command="cblrepo add --distro-pkg $p,$version"
		echo $command
		eval $command
	done

	if [[ $mode == initdb-sync ]]; then
		# Sync cblrepo with Hackage
		echo -n "Syncing cblrepo with Hackage..."
		cblrepo sync
		echo "done"
	fi

	# Add packages from Hackage
	for hp in $hackage_packages_file; do
		# Grab latest version of package
		cabal_file=$(curl -s $hackage_url/package/$hp | grep -ioE "Cabal source package[)<>/lia href=\"]+\/package\/.+\.cabal" | grep -ioE "\/package.+")
		command="cblrepo add --cbl-url $hackage_url$cabal_file"
		echo $command
		eval $command
	done

	# Link the generated cblrepo.db file into ~/.cblrepo
	ln -sf $PWD/cblrepo.db ~/.cblrepo/cblrepo.db
	;;
	### Generate PKGBUILD files for Hackage packages ###
	(pkgbuild)

	# Remove any old packages.
	echo "Deleting old PKGBUILD directories..."
	rm -rfv haskell-*

	i=1
	for hp in ${hackage_packages_file}; do
		command="cblrepo pkgbuild --patchdir patch $hp"
		echo "($i/${#hackage_packages_file}) $command"
		eval $command
		(( i+=1 ))
	done
	;;
	### Create Arch Linux packages for the Hackage packages ###
	(makepkg)
	for pdir in haskell-*; do
		cd $pdir
		echo $(basename $PWD)
		makepkg -sf
		sudo pacman -U $(basename $PWD)-*.pkg.tar.xz
		cd ..
		echo
		echo "  Finished making/installing package for $pdir"
		echo
	done
	;;
	*)
	echo "Unrecognized <MODE>; valid ones are: initdb initdb-sync pkgbuild makepkg"
	;;
esac
```

A simple `HACKAGE_PACKAGES_FILE` (perhaps named `hackage_pkgs`) might look something like this:
```{.numberLines}
bindings-DSL
bindings-GLFW
GLFW-b
rosezipper
```

Here is sample output from the command `./maintain_cblrepo.sh hackage_pkgs initdb`:
```{.numberLines}
cblrepo add --ghc-pkg array,0.4.0.1
cblrepo add --ghc-pkg base,4.6.0.1
cblrepo add --ghc-pkg binary,0.5.1.1
cblrepo add --ghc-pkg bin-package-db,0.0.0.0
cblrepo add --ghc-pkg bytestring,0.10.0.2
cblrepo add --ghc-pkg containers,0.5.0.0
cblrepo add --ghc-pkg deepseq,1.3.0.1
cblrepo add --ghc-pkg directory,1.2.0.1
cblrepo add --ghc-pkg filepath,1.3.0.1
cblrepo add --ghc-pkg ghc-prim,0.3.0.0
cblrepo add --ghc-pkg haskell2010,1.1.1.0
cblrepo add --ghc-pkg haskell98,2.0.0.2
cblrepo add --ghc-pkg hoopl,3.9.0.0
cblrepo add --ghc-pkg hpc,0.6.0.0
cblrepo add --ghc-pkg integer-gmp,0.5.0.0
cblrepo add --ghc-pkg old-locale,1.0.0.5
cblrepo add --ghc-pkg old-time,1.1.0.1
cblrepo add --ghc-pkg pretty,1.1.1.0
cblrepo add --ghc-pkg process,1.1.0.2
cblrepo add --ghc-pkg template-haskell,2.8.0.0
cblrepo add --ghc-pkg time,1.4.0.1
cblrepo add --ghc-pkg unix,2.6.0.1
cblrepo add --ghc-pkg cabal,1.16.0
cblrepo add --distro-pkg aeson,0.6.2.1,5
cblrepo add --distro-pkg ansi-terminal,0.6.1,1
cblrepo add --distro-pkg ansi-wl-pprint,0.6.7.1,1
cblrepo add --distro-pkg attoparsec,0.10.4.0,4
cblrepo add --distro-pkg base-unicode-symbols,0.2.2.4,27
cblrepo add --distro-pkg base64-bytestring,1.0.0.1,2
cblrepo add --distro-pkg binary,0.7.1.0,1
cblrepo add --distro-pkg blaze-builder,0.3.3.2,1
cblrepo add --distro-pkg blaze-html,0.6.1.2,2
cblrepo add --distro-pkg blaze-markup,0.5.1.6,2
cblrepo add --distro-pkg byteable,0.1.1,1
cblrepo add --distro-pkg cairo,0.12.5.0,1
cblrepo add --distro-pkg case-insensitive,1.1.0.2,1
cblrepo add --distro-pkg cereal,0.4.0.1,1
cblrepo add --distro-pkg cmdargs,0.10.7,1
cblrepo add --distro-pkg colour,2.3.3,3
cblrepo add --distro-pkg conduit,1.0.9.3,3
cblrepo add --distro-pkg cpphs,1.17.1,2
cblrepo add --distro-pkg cryptohash,0.11.1,1
cblrepo add --distro-pkg data-default,0.5.3,3
cblrepo add --distro-pkg data-default-class,0.0.1,2
cblrepo add --distro-pkg data-default-instances-base,0.0.1,2
cblrepo add --distro-pkg data-default-instances-containers,0.0.1,2
cblrepo add --distro-pkg data-default-instances-dlist,0.0.1,3
cblrepo add --distro-pkg data-default-instances-old-locale,0.0.1,2
cblrepo add --distro-pkg digest,0.0.1.2,3
cblrepo add --distro-pkg dlist,0.6.0.1,1
cblrepo add --distro-pkg entropy,0.2.2.4,1
cblrepo add --distro-pkg extensible-exceptions,0.1.1.4,27
cblrepo add --distro-pkg fgl,5.4.2.4,27
cblrepo add --distro-pkg glib,0.12.5.0,1
cblrepo add --distro-pkg gluraw,1.4.0.0,2
cblrepo add --distro-pkg glut,2.5.0.2,1
cblrepo add --distro-pkg graphviz,2999.16.0.0,8
cblrepo add --distro-pkg gtk,0.12.5.0,1
cblrepo add --distro-pkg hashable,1.2.1.0,1
cblrepo add --distro-pkg haskell-src-exts,1.14.0,1
cblrepo add --distro-pkg highlighting-kate,0.5.5.1,3
cblrepo add --distro-pkg hinotify,0.3.6,1
cblrepo add --distro-pkg hlint,1.8.55,1
cblrepo add --distro-pkg hostname,1.0,27
cblrepo add --distro-pkg hs-bibutils,5.0,3
cblrepo add --distro-pkg hscolour,1.20.3,27
cblrepo add --distro-pkg hslua,0.3.10,1
cblrepo add --distro-pkg http,4000.2.10,2
cblrepo add --distro-pkg http-types,0.8.3,2
cblrepo add --distro-pkg hunit,1.2.5.2,2
cblrepo add --distro-pkg json,0.7,9
cblrepo add --distro-pkg lifted-base,0.2.1.1,1
cblrepo add --distro-pkg mmorph,1.0.1,1
cblrepo add --distro-pkg monad-control,0.3.2.2,1
cblrepo add --distro-pkg monadcatchio-mtl,0.3.1.0,1
cblrepo add --distro-pkg monadcatchio-transformers,0.3.1.0,1
cblrepo add --distro-pkg monads-tf,0.1.0.1,27
cblrepo add --distro-pkg mtl,2.1.2,27
cblrepo add --distro-pkg mwc-random,0.13.1.0,1
cblrepo add --distro-pkg nats,0.1.2,1
cblrepo add --distro-pkg network,2.4.2.2,1
cblrepo add --distro-pkg opengl,2.9.1.0,1
cblrepo add --distro-pkg openglraw,1.4.0.0,2
cblrepo add --distro-pkg pandoc,1.12.2.1,4
cblrepo add --distro-pkg pandoc-types,1.12.3,5
cblrepo add --distro-pkg pango,0.12.5.0,1
cblrepo add --distro-pkg parsec,3.1.4,1
cblrepo add --distro-pkg pcre-light,0.4,27
cblrepo add --distro-pkg polyparse,1.9,1
cblrepo add --distro-pkg pqueue,1.2.1,1
cblrepo add --distro-pkg primitive,0.5.1.0,1
cblrepo add --distro-pkg quickcheck,2.6,2
cblrepo add --distro-pkg random,1.0.1.1,27
cblrepo add --distro-pkg regex-base,0.93.2,27
cblrepo add --distro-pkg regex-compat,0.95.1,27
cblrepo add --distro-pkg regex-pcre,0.94.4,1
cblrepo add --distro-pkg regex-posix,0.95.2,27
cblrepo add --distro-pkg regex-tdfa,1.1.8,30
cblrepo add --distro-pkg resourcet,0.4.10,2
cblrepo add --distro-pkg safe,0.3.3,3
cblrepo add --distro-pkg sdl,0.6.5,1
cblrepo add --distro-pkg sdl-image,0.6.1,28
cblrepo add --distro-pkg sdl-ttf,0.6.2,3
cblrepo add --distro-pkg semigroups,0.12.1,1
cblrepo add --distro-pkg sha,1.6.3,1
cblrepo add --distro-pkg split,0.2.2,2
cblrepo add --distro-pkg stm,2.4.2,3
cblrepo add --distro-pkg syb,0.4.1,2
cblrepo add --distro-pkg system-fileio,0.3.11,6
cblrepo add --distro-pkg system-filepath,0.4.8,1
cblrepo add --distro-pkg tagged,0.7,1
cblrepo add --distro-pkg tagsoup,0.13,1
cblrepo add --distro-pkg temporary,1.1.2.4,3
cblrepo add --distro-pkg test-framework,0.8.0.3,3
cblrepo add --distro-pkg test-framework-quickcheck2,0.3.0.2,3
cblrepo add --distro-pkg texmath,0.6.5.2,3
cblrepo add --distro-pkg text,0.11.3.1,1
cblrepo add --distro-pkg transformers,0.3.0.0,27
cblrepo add --distro-pkg transformers-base,0.4.1,27
cblrepo add --distro-pkg uniplate,1.6.12,1
cblrepo add --distro-pkg unix-compat,0.4.1.1,2
cblrepo add --distro-pkg unordered-containers,0.2.3.3,1
cblrepo add --distro-pkg utf8-string,0.3.7,27
cblrepo add --distro-pkg utility-ht,0.0.9,1
cblrepo add --distro-pkg vector,0.10.9.1,1
cblrepo add --distro-pkg void,0.6.1,6
cblrepo add --distro-pkg wl-pprint-text,1.1.0.1,1
cblrepo add --distro-pkg x11,1.6.1.1,5
cblrepo add --distro-pkg x11-xft,0.3.1,31
cblrepo add --distro-pkg xml,1.3.13,4
cblrepo add --distro-pkg xmonad,0.11,7
cblrepo add --distro-pkg xmonad-contrib,0.11.2,2
cblrepo add --distro-pkg yaml,0.8.5.2,4
cblrepo add --distro-pkg zip-archive,0.1.4,1
cblrepo add --distro-pkg zlib,0.5.4.1,2
cblrepo add --cbl-url http://hackage.haskell.org/package/bindings-DSL-1.0.20/bindings-DSL.cabal
cblrepo add --cbl-url http://hackage.haskell.org/package/bindings-GLFW-3.0.3.2/bindings-GLFW.cabal
cblrepo add --cbl-url http://hackage.haskell.org/package/GLFW-b-1.4.6/GLFW-b.cabal
cblrepo add --cbl-url http://hackage.haskell.org/package/rosezipper-0.2/rosezipper.cabal
```

This is infinitely faster than adding each package by hand.
Feel free to copy/paste/modify/redistribute the code in `maintain_cblrepo.sh` to suit your needs.

[^search]: I use a [Hackage-tailored search engine for Firefox](http://mycroftproject.com/search-engines.html?name=hackage) to make package searching on Hackage easier.
[^whereisit]: The command for finding out which packages are provided by GHC is `pacman -Qi ghc`, and you have to look for the `Provides` field.
As for the packages from [haskell-core], you have to do some manual grepping for packages that start with `haskell-` from the command `pacman -Q`.
