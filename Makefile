org_files := $(shell find . -name "*.org" -exec grep -l "^:ID: " {} +)

obj_files := $(org_files:%.org=%.lobj)

archive_file := site.larc

all: tangle weave lint
.PHONY: all

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files)

$(archive_file): $(obj_files)
	lilac archive $^ --out-file $@

tangle: $(archive_file)
	lilac tangle $< --out-dir .
	touch tangle

weave: $(archive_file)
	lilac weave $< --out-dir ./site --write-css --write-js
	touch weave

lint: $(archive_file)
	lilac lint $<
	touch lint

gitconfig:
	git config diff.orderfile .git-orderfile
.PHONY: gitconfig

update:
	niv update nixpkgs --branch nixos-25.11
.PHONY: update

clean:
	rm -rf \
		$(archive_file) \
		$(obj_files) \
		site/*
.PHONY: clean
