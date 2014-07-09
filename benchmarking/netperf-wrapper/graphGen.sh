#!/bin/bash
if [ -z $1 ] ; then
    echo "ERROR: directory name not given"
    exit 1
fi
./netperf-wrapper -p bbox -o ${1}/scalability-TPS.png -i `find ${1} -name '*.json*' | grep -i 'best' | sort`
./netperf-wrapper -p bboxNet -o ${1}/scalability-BW.png -i `find ${1} -name '*.json*' | grep -i 'best' | sort`

