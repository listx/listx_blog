# This Makefile is expected to be run inside a nix-shell.

PROJ_ROOT := $(shell git rev-parse --show-toplevel)
BLOG_IP?=10.0.0.46
BLOG_PORT?=8020

all: sync
.PHONY: all

# Check for broken links.
check:
	cabal run -- blog check
.PHONY: check

gen-css:
	cabal build -- base >/dev/null
	cabal exec -- base
.PHONY: gen-css

# JavaScript generated from Rust.
gen-js:
	rustup default stable
	make -C rust-js build
.PHONY: gen-js

sync: build-site
	./sync.sh
.PHONY: sync

cabal-update:
	cabal update
.PHONY: cabal-update

build-binaries:
	cabal build
.PHONY: build-binaries

build-site: build-binaries
	cabal run -- blog rebuild
.PHONY: build-site

watch: build-binaries
	cabal run -- blog watch --host $(BLOG_IP) --port $(BLOG_PORT)
.PHONY: watch

nixpkgs_stable_channel := nixos-24.05
update-deps: nix/sources.json nix/sources.nix
	niv update nixpkgs --branch $(nixpkgs_stable_channel)
	niv update
	touch update-deps
