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

site.tar.gz: weave
	rm -f site.tar site.tar.gz
	tar cvf site.tar -C legacy-generated-repo --exclude='**/.*' . #
	tar rvf site.tar -C site .
	gzip site.tar

publish:
	curl --oauth2-bearer $(cat .srht-pages-token) \
		-Fcontent=@site.tar.gz \
		https://pages.sr.ht/publish/listx.srht.site
.PHONY: publish

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
