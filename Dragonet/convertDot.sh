#!/bin/bash

#set -e

flist=`ls *.dot`
for f in ${flist} ; do
    justFile=`echo ${f} | sed -e "s/.dot//"`
    pdfFile="${justFile}.pdf"
    pngFile="${justFile}.png"
    dot -Gconcentrate=true -Tpng -o "${pngFile}" "${f}"
    dot -Gconcentrate=true -Tpdf -o "${pdfFile}" "${f}"
    #pdfcrop ${pdfFile}
    echo converted "${f} to ${pngFile} and ${pdfFile}"
done

