#!/usr/bin/env bash
set -euo pipefail

FONT_PACKAGE="mathjax-newcm-font"
VERSION="4.1.2"

pushd vendor

if [[ -d "${FONT_PACKAGE}" ]]; then
	echo "${FONT_PACKAGE} directory already exists"
	exit
fi

curl -O "https://registry.npmjs.org/@mathjax/${FONT_PACKAGE}/-/${FONT_PACKAGE}-${VERSION}.tgz"
tar -xzvf "${FONT_PACKAGE}-${VERSION}.tgz"
mv package "${FONT_PACKAGE}"
rm "${FONT_PACKAGE}-${VERSION}.tgz"
