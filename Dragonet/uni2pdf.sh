#!/bin/bash

#set -e

if [[ "$1" == "" ]]; then
	echo "Create a pdf file from a unicorn file"
	echo "Usage: $0 (unicorn files)"
	exit 0
fi

# generate dot files
runghc ./mkDot.hs $*

for fname in $* ; do
	./doConvertDot.sh "${fname}.dot"
done

