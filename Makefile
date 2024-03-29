BLOG_IP?=192.168.0.4
BLOG_PORT?=8020

all: sync

# Check for broken links.
check:
	nix-shell --command "cabal run -- blog check"

gen-css:
	nix-shell --command "cabal build -- base" >/dev/null
	nix-shell --command "cabal exec -- base"

# JavaScript generated from Rust.
gen-js:
	#make -C rust-js build

sync: build-site
	./sync.sh

build-binaries:
	nix-shell --command "cabal build"

build-site: build-binaries
	nix-shell --command "cabal run -- blog rebuild"

watch: build-binaries
	nix-shell --command "cabal run -- blog watch --host $(BLOG_IP) --port $(BLOG_PORT)"

.PHONY: check sync gen-css gen-js
