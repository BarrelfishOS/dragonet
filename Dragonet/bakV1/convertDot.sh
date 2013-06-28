#!/bin/bash

set -e

flist=`ls *.dot`
for f in ${flist} ; do
    justFile=`echo ${f} | sed -e "s/.dot//"`
    pdfFile="${justFile}.pdf"
    pngFile="${justFile}.png"
    dot -Tpng -o "${pngFile}" "${f}"
    dot -Tpdf -o "${pdfFile}" "${f}"
    echo converted "${f} to ${pngFile} and ${pdfFile}"
done

