#!/bin/bash

if [ -z $1 ] ; then
    echo "ERROR: directory name not given"
    exit 1
fi
if [ -z $2 ] ; then
    plttype="bboxScale"
    echo "Assuming Aggregate graph"
fi

if [ "$2" == "AVG"  ] ; then
    plttype="bboxScaleAVG"
    echo "Plotting per client average graph"
fi


#./netperf-wrapper -p bboxScale -o ${1}/scale.png -i `find ${1} -name '*.json*' | grep -i 'best' | grep "_SRVI_1_" |  sort`
#./netperf-wrapper -p bboxScale -o ${1}/scale.png -i `find ${1} -name '*.json*' |  sort`
./netperf-wrapper -p ${plttype} -o "${1}/${plttype}scale.png" -i `find ${1} -name '*.json*' |  sort`

echo "Individual Plot is in file ${1}/${1}/${plttype}scale.png"

