#!/usr/bin/env zsh

# Watch for changes in unicorn files and dot files.
# When a file is updated, create the corresponding pdf

if [ -z "$*" ]; then
	echo "Usage: $0 <dir>"
	exit
fi

idir=$1
inotifywait --format "%f %e" -e modify -e close_write -e moved_to -m $idir  | (while read fname event
do
	echo "Got: $fname ($event)"
	infile="$idir/$fname" # file that changed
	fext="${fname:e}"     # file name extension
	if [ "$fext" = "unicorn" ]; then
		docmd="./scripts/uni2pdf.zsh $infile"
	elif [ "$fext" = "dot" ]; then
		docmd="./scripts/dot2pdf.zsh $infile"
	else
		continue
	fi

	outfile="$unifile.pdf"
	# if output file exists and is newer that $fname, do nothing
	if [ -e "$pdffile" ]; then
		t_pdf=$(stat -c "%Y" $pdffile)
		t_uni=$(stat -c "%Y" $infile)
		if [ $t_pdf -gt $t_uni ]; then
			echo "***** out file older, not compiling"
			continue
		fi
	fi

	sleep .4 # wait until dust settles
	echo "Compiling $unifle via $docmd"
	eval $docmd
	echo
done)
