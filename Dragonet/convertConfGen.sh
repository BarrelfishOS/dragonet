#!/bin/bash

#set -e

do_series() {
    for f in *_${1}.dot ; do
        pdfFile=`basename ${f} .dot`.pdf
        dot -Gconcentrate=true -Tpdf -o "${pdfFile}" "${f}"
    done
    pdftk *_${1}.pdf cat output ${1}.pdf
    echo "created ${1}.pdf"
    rm *_${1}.pdf
}

do_series lpg
do_series prg
do_series embedded
do_series constrained
do_series final

