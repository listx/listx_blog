PROJ_ROOT := $(shell git rev-parse --show-toplevel)
BLOG_IP?=10.0.0.46
BLOG_PORT?=8020

all: sync
.PHONY: all

# Check for broken links.
check:
	nix-shell --command "cabal run -- blog check"
.PHONY: check

gen-css:
	nix-shell --command "cabal build -- base" >/dev/null
	nix-shell --command "cabal exec -- base"
.PHONY: gen-css

# JavaScript generated from Rust.
gen-js:
	#make -C rust-js build
.PHONY: gen-js

sync: build-site
	./sync.sh
.PHONY: sync

cabal-update:
	nix-shell --command "cabal update"
.PHONY: cabal-update

build-binaries:
	nix-shell --command "cabal build"
.PHONY: build-binaries

build-site: build-binaries
	nix-shell --command "cabal run -- blog rebuild"
.PHONY: build-site

watch: build-binaries
	nix-shell --command "cabal run -- blog watch --host $(BLOG_IP) --port $(BLOG_PORT)"
.PHONY: watch
