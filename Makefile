BLOG_IP?=192.168.0.4
BLOG_PORT?=8020

all: sync

sync: build-site
	./sync.sh

build-binaries:
	stack build

build-site: build-binaries
	stack exec -- blog rebuild

watch: build-binaries
	stack exec -- blog watch --host $(BLOG_IP) --port $(BLOG_PORT)

.PHONY: sync
