# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

#set -e

convertDir() {
    dir=$(pwd)
    cd $1
    for f in *.dot ; do
        [ -f $f ] || continue
        justFile=`echo ${f} | sed -e "s/.dot//"`
        pdfFile="${justFile}.pdf"
        svgFile="${justFile}.svg"
        dot -Gconcentrate=true -Tpdf -o "${pdfFile}" "${f}"
        dot -Gconcentrate=true -Tsvg -o "${svgFile}" "${f}"
        #pdfcrop ${pdfFile}
        echo converted "$1/${f} to $1/{${pdfFile} ${svgFile}}"
    done
    cd $dir

    for d in $1/*/ ; do
        if [ -d $d ] ; then
            convertDir $d
        fi
    done
}

if [ "$1" == "" ] ; then
    echo Usage: ./convertDotDir.sh directory
    exit 0
fi

convertDir $1
