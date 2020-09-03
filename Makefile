all: sync

sync: build-site
	./sync.sh

build-binaries:
	stack build

build-site: build-binaries
	stack exec -- blog rebuild

watch: build-binaries
	stack exec -- blog watch

.PHONY: sync
