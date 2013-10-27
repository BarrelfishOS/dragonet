#!/bin/bash

libdir="/home/ubuntu/dragonet/dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/"

getelf() {
    dname=$1
    fname=$(basename ${dname})
    fnameCropped=$(echo ${fname} | sed -e 's/.o$//g')
#    echo "filename is ${fname}"
#    echo "fnameCropped is ${fnameCropped}"
    elfimages=$(find ${libdir} -name "*${fnameCropped}.o")
    if [ -z "${elfimages}" ] ; then
        echo "No image found for filename ${fname}"
    else
        echo "${elfimages} found for filename ${fname}"
        echo "${elfimages}" >> mydepsFiltered.txt
    fi
}

for l in `cat $1` ; do
#    echo "fullname is $l"
    getelf $l
done


