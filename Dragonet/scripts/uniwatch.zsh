#!/usr/bin/env zsh
# script watch for changes in unicorn files.

if [ -z "$*" ]; then
	echo "Usage: $0 <dir>"
	exit
fi

idir=$1
inotifywait --format "%f %e" -e modify -e close_write -e moved_to -m $idir  | (while read fname event
do
	echo "Got: $fname ($event)"
	# if file does not have a unicorn extension, do nothing
	if [ ! "${fname:e}" = "unicorn" ]; then
		continue
	fi

	unifile="$idir/$fname"
	pdffile="$unifile.pdf"
	# if output file exists and is newer that $fname, do nothing
	if [ -e "$pdffile" ]; then
		t_pdf=$(stat -c "%Y" $pdffile)
		t_uni=$(stat -c "%Y" $unifile)
		if [ $t_pdf -gt $t_uni ]; then
            echo "***** out file older, not compiling"
			continue
		fi
	fi

    sleep .4 # wait until dust settles
	echo "Compiling $unifle:"
	./scripts/uni2pdf.zsh $unifile
	echo
done)
