#!/bin/bash

run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}_${ECHO_SERVER}"
plotpath="${result}.png"
log="${result}.log"
tmpfile="${result}.tmp"
title="${JUMBO}${ECHO_SERVER}_${target_t}_PKT_${PKTSIZE}_B_${BRUST}${ONLOADTEXT}"

#    ./netperf-wrapper -c memcached -C ziger2 -C sbrinz2 -C gruyere -T 10.113.4.95  -H asiago -l 10 -L mylog2.log memcached_rr

   ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} -b ${BRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
./netperf-wrapper -i ${outfile} # -p ${PLOTTYPE} -o ${plotpath}
}


run_bm_tp_rr() {
    target=${1}
    target_t=${2}
    result="${3}/latency_${target_t}.png"
    log="${3}/latency_${target_t}.log"

    ALLRESULTS="${3}/maxTP_${target_t}_${PKTSIZE}/"
    mkdir -p ${ALLRESULTS}

    MYTMPDIR=${ALLRESULTS}
    TMPFILE="${MYTMPDIR}/f1.txt"
    TMPRES="${MYTMPDIR}/f2.txt"
    TMPPLOT="${MYTMPDIR}/f1.png"
    TMPLOG="${MYTMPDIR}/f1.log"
    FINALRESULT="${ALLRESULTS}/finalResult.result"
    SAMARRYFILE=`mktemp`
    rm -f ${SAMARRYFILE}

#    set -x
#    set -e
    CTP=0
    NTP=1
    LBRUST=1
    CBRUST=1
    NBRUST=1
    while [  $NTP -gt $CTP ]; do
#        set -x
#        set -e
        let LBRUST=CBRUST
        let CBRUST=NBRUST
        let NBRUST=CBRUST+CBRUST

        title="${JUMBO}${ECHO_SERVER}_${target_t}_PKT_${PKTSIZE}_B_${CBRUST}${ONLOADTEXT}"

#        ./netperf-wrapper -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        ./netperf-wrapper -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER}   -P ${PKTSIZE} -b ${CBRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        outfile=`cat ${TMPFILE} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
        ./netperf-wrapper -i ${outfile} | tee ${TMPRES}

        let CTP=NTP

        NVTPS=`cat ${TMPRES} | grep "^Valid TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPRES} | grep "^Valid TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        GETMISSES=`cat ${TMPRES} | grep "^total get_misses: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${PKTSIZE}, ${CBRUST}, ${GETMISSES}, ${NVTPS}, ${NTP}" >> ${SAMARRYFILE}
        echo "TP of $NTP for $CBRUST"

        cat ${TMPRES} >> ${FINALRESULT}
        rm -f ${TMPFILE}
        rm -f ${TMPRES}
        rm -f ${TMPPLOT}
    done

    set -x
    set -e

        title="${ECHO_SERVER}_${target_t}_PKT_${PKTSIZE}_B_${LBRUST}${ONLOADTEXT}_BEST"
        ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
    set +x
    set +e
    cat ${SAMARRYFILE} >> ${FINALRESULT}
    rm -f ${SAMARRYFILE}
    sleep 3
    echo "while [  $NTP -gt $CTP ]; do"
}


get_best_latency()
{
PKTSIZE=$1
BRUST=$2
OUTDIR="${OUTDIRP}/LATENCY/"
mkdir -p ${OUTDIR}
# echo latency numbers
run_npf_w_rr ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
}

get_best_tp()
{
PKTSIZE=$1
OUTDIR="${OUTDIRP}/TP_MAX/"
mkdir -p ${OUTDIR}
# for increasing TP on transaction based benchmark
run_bm_tp_rr  ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
}

JUMBO="JUMBO"
JUMBO=""

INTEL_T="10.22.4.95"
INTEL_S_T="10.113.4.95"
SF_T="10.23.4.195"
SF_S_T="10.113.4.195"
PLOTTYPE="bbest"
PLOTTYPE="lbest"

SELTARGET=${SF_S_T}
SELTARGET_T="SF-S"

SELTARGET=${INTEL_S_T}
SELTARGET_T="Intel-S"

UDP_TEST_NAME="udp_rr1"
UDP_TEST_NAME="udp_latency"
UDP_TEST_NAME="udp_rr"
UDP_TEST_NAME="memcached_rr"

ONLOADTYPE="--onloadLatency"
ONLOADTEXT="onloadLatency"

ONLOADTYPE=""
ONLOADTEXT=""

ECHO_SERVER="memcached"

ITERATIONS=5
ITERATIONS=1

DURATION=10

echo "Running BM for server: ${ECHO_SERVER}, Target: [${SELTARGET} , ${SELTARGET_T}]"
echo "Onload: [${ONLOADTYPE}, ${ONLOADTEXT}], Jumbo: [${JUMBO}] "

OUTDIRPP="../memcachedResults/results/${SELTARGET_T}/"

OUTDIRP="${OUTDIRPP}/${ECHO_SERVER}/${UDP_TEST_NAME}/"
mkdir -p ${OUTDIRP}
OUTDIR=${OUTDIRP}

if [ "${JUMBO}" == "" ] ; then
    echo "Wthout JUMBO"
else
    OUTDIRP="${OUTDIRP}/${JUMBO}/"
fi

if [ "${ONLOADTYPE}" == "" ] ; then
    echo "Wthout onload"
else
    OUTDIRP="${OUTDIRP}/${ONLOADTEXT}/"
fi

echo "OUTPUT Location: ${OUTDIRP}"


get_best_tp 1400
exit 0

get_best_latency 1024 1
######################################################################
######################################################################


get_best_tp 1400
get_best_latency 1400 64
exit 0
######################################################################
######################################################################
######################################################################
######################################################################
