#!/bin/bash

#set -e

convertDir() {
    cd $1
    flist=`ls *.dot`
    for f in ${flist} ; do
        justFile=`echo ${f} | sed -e "s/.dot//"`
        pdfFile="${justFile}.pdf"
        svgFile="${justFile}.svg"
        dot -Gconcentrate=true -Tpdf -o "${pdfFile}" "${f}"
        dot -Gconcentrate=true -Tsvg -o "${svgFile}" "${f}"
        #pdfcrop ${pdfFile}
        echo converted "$1/${f} to $1/{${pdfFile} ${svgFile}}"
    done
    cd ..

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
