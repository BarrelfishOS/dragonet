# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

#set -e

flist=`ls *.dot`
for f in ${flist} ; do
    justFile=`echo ${f} | sed -e "s/.dot//"`
    pdfFile="${justFile}.pdf"
    pngFile="${justFile}.png"
    svgFile="${justFile}.svg"
    dot -Gconcentrate=true -Tpng -o "${pngFile}" "${f}"
    dot -Gconcentrate=true -Tpdf -o "${pdfFile}" "${f}"
    dot -Gconcentrate=true -Tsvg -o "${svgFile}" "${f}"
    #pdfcrop ${pdfFile}
    echo converted "${f} to ${pngFile} and ${pdfFile} ${svgFile}"
    #echo converted "${f} to ${pdfFile}"
done

