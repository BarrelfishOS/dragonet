#!/usr/bin/env zsh

uni2dot="scripts/uni2dot.zsh"
dot2pdf="scripts/dot2pdf.zsh"

set -e

unifiles=$*
dotfiles=""
for f in $*
do
	dotfiles="$dotfiles $f.dot"
done

cmd1="$uni2dot $unifiles"
echo $cmd1
eval $cmd1


cmd2="$dot2pdf $dotfiles"
echo $cmd2
eval $cmd2
