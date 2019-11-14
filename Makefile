sync: rebuild
	./sync.sh

build-binaries:
	stack build

rebuild: build-binaries
	stack exec -- blog rebuild

watch: build-binaries
	stack exec -- blog watch

.PHONY: sync nosync
