#!/usr/bin/env bash

set -euox pipefail

SCRIPT_DIR="$(realpath "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "${SCRIPT_DIR}")"

bresenham_dir="${SCRIPT_DIR}"/../../code/2021-03-15-bresenham-circle-drawing-algorithm
golden="${bresenham_dir}"/golden.yaml

# Caching.
if ! md5sum "${golden}" \
	| grep -q 8a9da8559f0a1148c1fb9ca0d540159a; then
	echo "regenerating the golden file"
	# Test with all radii in the range 1 to 150.
	"${bresenham_dir}"/gen_golden.py 1 150 > "${golden}"
fi

export BRESENHAM_GOLDEN="${golden}"
cargo test
