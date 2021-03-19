# Add the -n flag for directories, as otherwise, stray symlinks will be created
# inside the C (config) directory itself.
build:
	wasm-pack build \
		--no-typescript \
		--out-dir js \
		--release \
		--target web
test:
	wasm-pack test \
		--headless \
		--firefox
	# Test golden tests for drawing algorithms.
	./tests/test_bresenham.sh

.PHONY: \
	build \
	test
