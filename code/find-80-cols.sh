find . -type f -exec sh -c "expand -t 8 {} | grep -n \".\{81\}\"" \; -print
