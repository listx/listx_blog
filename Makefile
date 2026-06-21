org_files := $(shell find . \
	\( -path "./syscfg" -o -path "./codex" \) -prune -o \
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

site.tar.gz: $(archive_file) weave vendor/mathjax-modern-font vendor/google-fonts
	rm -f site.tar site.tar.gz
	git -C legacy-generated-repo reset --hard
	git -C codex reset --hard
	make -C syscfg/doc weave
	cd codex && git submodule update --init --recursive

	#
	find legacy-generated-repo -type f -name "*.html" \
		-exec sed -i 's|https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS-MML_HTMLorMML|/vendor/MathJax/tex-mml-chtml.js|g' {} +
	find site syscfg -type f -name "*.html" \
		-exec sed -i 's|https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js|/vendor/MathJax/tex-mml-chtml.js|g' {} +
	find codex -path 'codex/deps' -prune -o \
		-type f -name "*.html" \
		-exec sed -i 's|https://cdn.jsdelivr.net/npm/mathjax@4.0.0-beta.4/tex-mml-chtml.js|/vendor/MathJax/tex-mml-chtml.js|g' {} +
	
	#
	find codex -path 'codex/deps' -prune -o -type f -name "*.html" -exec \
		sed -i 's|output:[[:space:]]*{|loader: { paths: { "mathjax-modern": "/vendor/mathjax-modern-font" } },\
		output: {|' {} +
	find codex -path 'codex/deps' -prune -o -type f -name "*.html" -exec \
		sed -i 's|src="https://code.jquery.com/jquery-3.6.4.min.js"|src="/vendor/jquery-3.6.4.min.js"|g' {} +
	
	find legacy-generated-repo -type f -name "*.html" -exec \
		sed -i '/<script type="text\/x-mathjax-config">/,/<\/script>/c\
		<script>\
		  window.MathJax = {\
		    tex: {\
		      ams: {\
		        multlineWidth: '\''85%'\''\
		      },\
		      tags: '\''ams'\'',\
		      tagSide: '\''right'\'',\
		      tagIndent: '\''.8em'\''\
		    },\
		    chtml: {\
		      scale: 1.0,\
		      displayAlign: '\''center'\'',\
		      displayIndent: '\''0em'\''\
		    },\
		    svg: {\
		      scale: 1.0,\
		      displayAlign: '\''center'\'',\
		      displayIndent: '\''0em'\''\
		    },\
		    loader: { paths: { "mathjax-modern": "/vendor/mathjax-modern-font" } },\
		    output: {\
		      font: '\''mathjax-modern'\'',\
		      displayOverflow: '\''overflow'\''\
		    }\
		  };\
		</script>' {} +
	find legacy-generated-repo -type f -name "*.html" -exec \
		sed -i 's|src="https://code.jquery.com/jquery-2.1.4.min.js"|src="/vendor/jquery-2.1.4.min.js"|g' {} +
	
	find site syscfg -type f -name "*.html" -exec \
		sed -i '/window\.MathJax[[:space:]]*=[[:space:]]*{/a\
		  loader: { paths: { "mathjax-modern": "/vendor/mathjax-modern-font" } },' {} +

	find site syscfg codex legacy-generated-repo -path 'codex/deps' -prune -o \
		-type f -name "*.html" -exec \
		sed -i \
		's|href="https://fonts\.googleapis\.com[^"]*"|href="/vendor/google-fonts/google-fonts.css"|g' {} +
	
	#
	sed -i 's|Source Serif Pro|Source Serif 4|g' legacy-generated-repo/css/base.css

	#
	tar cvf site.tar -C legacy-generated-repo --exclude='**/.*' .

	#
	tar rvf site.tar -C site .

	#
	find syscfg -type f -not -path '*/.*' \
		\( -name '*.html' \
		-o -path 'syscfg/css/*' \
		-o -path 'syscfg/js/*' \) \
		| tar rvf site.tar -T -

	#
	find -L codex -path 'codex/deps/elisp/lilac/deps' -prune -o \
		-type f \
		-not -path '*/.*' \
		\( -name '*.html' -o -name '*.css' -o -name '*.js' \) \
		| tar rvhf site.tar --hard-dereference --exclude='codex/deps/elisp/lilac/deps' -T -

	#
	tar rvf site.tar vendor
	gzip site.tar

	#
	git -C legacy-generated-repo reset --hard
	git -C codex reset --hard

#
vendor/mathjax-modern-font:
	./get-mathjax-fonts.sh

vendor/google-fonts:
	rm -rf vendor/google-fonts
	./get-google-fonts.sh

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
	git -C legacy-generated-repo reset --hard
	git -C codex reset --hard
	rm -rf \
		$(archive_file) \
		$(obj_files) \
		site.tar site.tar.gz \
		site/* \
.PHONY: clean
