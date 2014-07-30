#!/bin/bash
if [ -z $1 ] ; then
    echo "ERROR: directory name not given"
    exit 1
fi
#./netperf-wrapper -f org_table  -i `find ${1} -name '*.json*' | grep -i 'best' | grep "_SRVI_1_" |  sort`
./netperf-wrapper -p bboxScale -o ${1}/scale.png -i `find ${1} -name '*.json*' | grep -i 'best' | grep "_SRVI_1_" |  sort`
#./netperf-wrapper -p bboxScale -o ${1}/scale.png -i `find ${1} -name '*.json*' | grep -i 'best' | sort`

echo "Plot is in file ${1}/scale.png"

