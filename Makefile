BLOG_IP?=192.168.0.4
BLOG_PORT?=8020

all: sync

sync: build-site
	./sync.sh

build-binaries:
	nix-shell --command "cabal build"

build-site: build-binaries
	nix-shell --command "cabal run -- blog rebuild"

watch: build-binaries
	nix-shell --command "cabal run -- blog watch --host $(BLOG_IP) --port $(BLOG_PORT)"

.PHONY: sync
