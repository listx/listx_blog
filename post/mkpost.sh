#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

if (( $# == 0 )); then
    >&2 echo "\
Usage: mkpost.sh <WORD_1>...<WORD_N>
    where WORD_1...WORD_N are word(s) separated by whitespace.

Example:
    mkpost.sh foo bar baz
"
    exit 1
else
fi

sep='-'
# `:gs/l/r' is a global substitution of `l' for `r'.
joined=$(printf "${sep}%s" ${@:gs/%/%%})
joined=${joined:${#sep}}

fname="$(date +%F)-$(echo $joined | tr '[:upper:]' '[:lower:]').org"
< template/default.org >> $fname
emacs $fname & disown
