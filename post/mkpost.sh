#!/usr/bin/env zsh

date=$(date +%F)
fname="$date-$@.org"
< template/default.org >> $fname
emacs $fname & disown
