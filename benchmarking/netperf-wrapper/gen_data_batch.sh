#!/bin/bash

run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}"
plotpath="${3}/latency_${target_t}.png"
log="${3}/latency_${target_t}.log"
tmpfile="${3}/latency_${target_t}.tmp"
title="${target_t}_PKT_${PKTSIZE}_B_${BRUST}"
./netperf-wrapper -l 10 -P ${PKTSIZE} -b ${BRUST} -H asiago -C burrata -T ${target} udp_rr -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
./netperf-wrapper -i ${outfile} -p 'userbw2' -o ${plotpath}
}


run_bm_tp() {
    target=${1}
    target_t=${2}
    result="${3}/latency_${target_t}.png"
    log="${3}/latency_${target_t}.log"
    title="${target_t}_PKT_${PKTSIZE}_B_${BRUST}"

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

        title="${target_t}_PKT_${PKTSIZE}_B_${CBRUST}"

        ./netperf-wrapper -l 10 -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} udp_rr -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        outfile=`cat ${TMPFILE} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
        ./netperf-wrapper -i ${outfile} -p 'userbw2' -o ${TMPPLOT} | tee ${TMPRES}

        let CTP=NTP

        NBWD=`cat ${TMPRES} | grep "THROUGHPUT: " | cut -d'[' -f2 | cut -d']' -f1`
        NLATENCY=`cat ${TMPRES} | grep "RT_LATENCY: " | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPRES} | grep "TRANSACTION_RATE: " | cut -d'[' -f2 | cut -d'.' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${PKTSIZE}, ${CBRUST}, ${NTP}, ${NLATENCY}, ${NBWD}" >> ${SAMARRYFILE}
        echo "TP of $NTP for $CBRUST"

        cat ${TMPRES} >> ${FINALRESULT}
        rm -f ${TMPFILE}
        rm -f ${TMPRES}
        rm -f ${TMPPLOT}
        sleep 5
    done

    set -x
    set -e

        title="${target_t}_PKT_${PKTSIZE}_B_${LBRUST}_BEST"
        ./netperf-wrapper -l 10 -P ${PKTSIZE} -b ${LBRUST} -H asiago -C burrata -T ${target} udp_rr -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
    set +x
    set +e
    cat ${SAMARRYFILE} >> ${FINALRESULT}
    rm -f ${SAMARRYFILE}
    sleep 3
    echo "while [  $NTP -gt $CTP ]; do"
}




OUTDIR="../myplots/TPmax/"
INTEL_T="10.22.4.11"
INTEL_S_T="10.113.4.11"
SF_T="10.23.4.21"
SF_S_T="10.113.4.21"
DEFAULT_PLOT="userlatency2"
#PKTSIZE=1024
PKTSIZE=8000
BRUST=1

mkdir -p ${OUTDIR}

run_bm_tp ${INTEL_T} "INTEL_T" ${OUTDIR}
exit 0

# echo latency numbers
run_npf_w_rr ${INTEL_T} "INTEL_T" ${OUTDIR}
run_npf_w_rr ${SF_T} "SF_T" ${OUTDIR}

#run_npf_w_rr ${SF_S_T} "SF_S_T" ${OUTDIR}
#run_npf_w_rr ${INTEL_S_T} "INTEL_S_T" ${OUTDIR}

