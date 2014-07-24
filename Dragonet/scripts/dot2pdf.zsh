#!/usr/bin/env zsh

if [ -z "$*" ]; then
	echo "Usage: $0 <dotfiles>"
	exit
fi

set -e
for dotfile in $*; do
	pdffile=${dotfile:r}.pdf
	cmd="dot -Gconcentrate=true -Tpdf -o $pdffile $dotfile"
	echo $cmd
	eval $cmd
done
