#!/usr/bin/env zsh

date=$(date +%F)
fname="$date-$@.md"
< template/default.md >> $fname
emacs $fname & disown
