#!/usr/bin/env bash
set -euo pipefail

if [[ -d vendor/google-fonts ]]; then
	echo "vendor/google-fonts directory already exists"
	exit
fi

mkdir -p vendor/google-fonts

curl -fsSL -A 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36' \
'https://fonts.googleapis.com/css2?display=swap&family=Source+Code+Pro:ital,wght@0,400;0,700;1,400;1,700&family=Source+Sans+3:ital,wght@0,400;0,700;1,400;1,700&family=Source+Serif+4:ital,wght@0,400;0,700;1,400;1,700' \
-o vendor/google-fonts/google-fonts.css

# Filter specifically for links ending in .woff2
grep -o 'https://fonts.gstatic.com[^)]*\.woff2' \
	vendor/google-fonts/google-fonts.css |
	sort -u |
	while read -r url; do
		curl -fsSL "$url" -o "vendor/google-fonts/$(basename "$url")"
	done

sed -i -E \
's|https://fonts.gstatic.com/[^)]*/([^/)]+\.woff2)|/vendor/google-fonts/\1|g' \
vendor/google-fonts/google-fonts.css
