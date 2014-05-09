#!/bin/bash

run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}_${ECHO_SERVER}"
plotpath="${result}.png"
log="${result}.log"
tmpfile="${result}.tmp"
title="${USE_TCP}${ECHO_SERVER}_${target_t}_SRV_${SERVERCORES}_C_${CONCURENCY}_${ONLOADTEXT}"

   ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} ${USE_TCP} \
       -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
       --servercores ${SERVERCORES} --clientcores 2 -T ${target} ${UDP_TEST_NAME} \
       --concurrency ${CONCURENCY} -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

#outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
#./netperf-wrapper -i ${outfile} # -p ${PLOTTYPE} -o ${plotpath}
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

        title="${USE_TCP}${ECHO_SERVER}_${target_t}_SRV_${SERVERCORES}_C_${CBRUST}_${ONLOADTEXT}"

        ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} ${USE_TCP} \
       -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
       --servercores ${SERVERCORES} --clientcores 2 -T ${target} ${UDP_TEST_NAME} \
       --concurrency ${CBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}

#        ./netperf-wrapper -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        #./netperf-wrapper -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER}   -P ${PKTSIZE} -b ${CBRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        #outfile=`cat ${TMPFILE} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
        #./netperf-wrapper -i ${outfile} | tee ${TMPRES}

        let CTP=NTP

        NVTPS=`cat ${TMPFILE} | grep "^Valid TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPFILE} | grep "^Valid TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        GETMISSES=`cat ${TMPFILE} | grep "^total get_misses: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${SERVERCORES}, ${CBRUST}, ${GETMISSES}, ${NVTPS}, ${NTP}" >> ${SAMARRYFILE}
        echo "#################################################"
        echo "################### TP of [$NTP:$CBRUST]"
        echo "#################################################"

        cat ${TMPFILE} >> ${FINALRESULT}
        rm -f ${TMPFILE}
    done

    set -x
    set -e

    title="${USE_TCP}${ECHO_SERVER}_${target_t}_SRV_${SERVERCORES}_C_${LBRUST}_${ONLOADTEXT}_BEST"

    ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} ${USE_TCP} \
        -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
        --servercores ${SERVERCORES} --clientcores 2 -T ${target} ${UDP_TEST_NAME} \
        --concurrency ${LBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}

    set +x
    set +e
    cat ${SAMARRYFILE} >> ${FINALRESULT}
    rm -f ${SAMARRYFILE}
    echo "done with while [  $NTP -gt $CTP ]; do"
    echo "#############################################################"
    echo "############ BEST TP for CORES:${SERVERCORES} [$NTP:$CBRUST]"
    echo "#############################################################"


    sleep 3
}


get_best_latency()
{
SERVERCORES=$1
CONCURENCY=$2
OUTDIR="${OUTDIRP}/LATENCY/"
mkdir -p ${OUTDIR}
# echo latency numbers
run_npf_w_rr ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
./cleanup.sh
}

get_best_tp()
{
SERVERCORES=$1
OUTDIR="${OUTDIRP}/TP_MAX/S_${SERVERCORES}/"
mkdir -p ${OUTDIR}
# for increasing TP on transaction based benchmark
run_bm_tp_rr  ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
}

get_scalability() {
    get_best_tp 1
    get_best_tp 2
    get_best_tp 4
    get_best_tp 8
    get_best_tp 16
}


#########################################
## Main starts here
#########################################
USE_TCP="--use-tcp"
USE_TCP=""

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
echo "Onload: [${ONLOADTYPE}, ${ONLOADTEXT}], USE_TCP: [${USE_TCP}] "

OUTDIRPP="../memcachedResults/results/${SELTARGET_T}/"

OUTDIRP="${OUTDIRPP}/${ECHO_SERVER}/${UDP_TEST_NAME}/"
mkdir -p ${OUTDIRP}
OUTDIR=${OUTDIRP}

if [ "${USE_TCP}" == "" ] ; then
    echo "Using UDP"
else
    echo "Using TCP"
    OUTDIRP="${OUTDIRP}/${USE_TCP}/"
fi

if [ "${ONLOADTYPE}" == "" ] ; then
    echo "Wthout onload"
else
    OUTDIRP="${OUTDIRP}/${ONLOADTEXT}/"
fi

echo "OUTPUT Location: ${OUTDIRP}"

get_scalability
echo "done with benchmarking"
exit 0

get_best_latency 1 1

######################################################################
######################################################################


get_best_tp 1400
get_best_latency 1400 64
exit 0
######################################################################
######################################################################
######################################################################
######################################################################
