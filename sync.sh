#!/usr/bin/env bash

dest='listx.github.io/'
targ='_site/'

if [[ ! -d $targ ]]; then
    echo "\`$targ' directory missing"
    exit 1
elif [[ ! -d $dest ]]; then
    echo "\`$dest' directory missing"
    exit 1
fi

rsync -ahP --no-whole-file --inplace --delete --exclude=.git\* $targ $dest
