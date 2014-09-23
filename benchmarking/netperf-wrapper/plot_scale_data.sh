#!/bin/bash


if [ -z $1 ] ; then
    echo "ERROR: Data directory name not given"
    exit 1
fi

dataDir="${1}"
shift

if [ -z $1 ] ; then
    echo "ERROR: output dir name not given"
    exit 1
fi

outDir="${1}"
shift

if [ -z $1 ] ; then
    echo "ERROR: Packet size not given"
    exit 1
fi

pktSize="${1}"
shift


if [ -z "$1" ] ; then
    echo "ERROR: Title text not given"
    exit 1
fi

titleText="${1}"
shift


mkdir -p ${outDir}

#set -x
#set +e
for dname in `ls ${dataDir}` ;
do
    if [ -d "${dataDir}/${dname}" ] ; then
        echo "generating line for ${dname} located at ${dataDir}/${dname}"
        ./graphGenScale.sh "${dataDir}/${dname}" | grep "#####" > "${outDir}/${dname}.result"
        isEmpty=`wc -l "${outDir}/${dname}.result"`
        if [ "${isEmpty}" == "0" ] ; then
            echo "no data found for ${outDir}/${dname}.result, so removing it"
            rm "${outDir}/${dname}.result"
        fi
    else
        echo "ignoring "${dataDir}/${dname}" as it's  afile "
    fi
done
set -e

echo "now, we try and plot the data"

../plottingTools/plottingScript.py --title "${titleText}" --x "Flows" --y "Transactions per Second(TPS)" --psize ${pktSize}  --save "${outDir}/ScalabilityP${pktSize}.pdf"  `find ${outDir}/ -name '*.result' | sort -r`

