org_files := $(shell find . -path "./syscfg" -prune -o \
	-name "*.org" -exec grep -l "^:ID: " {} +)

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

	#
	make -C syscfg/doc weave
	find syscfg -type f -not -path '*/.*' \
		\( -name '*.html' \
		-o -path 'syscfg/css/*' \
		-o -path 'syscfg/js/*' \) \
		| tar rvf site.tar -T -
	gzip site.tar

SRHT_TOKEN != cat .srht-pages-token 2>/dev/null || echo ""

publish: site.tar.gz syscfg/doc/weave
	@if [ -z "$(SRHT_TOKEN)" ]; then \
		echo "Error: .srht-pages-token is empty or missing." >&2; \
		exit 1; \
	fi
	@echo "Uploading site to Sourcehut Pages..."
	@curl --oauth2-bearer "$(SRHT_TOKEN)" \
		-Fcontent=@site.tar.gz \
		https://pages.sr.ht/publish/listx.srht.site
	touch publish

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
