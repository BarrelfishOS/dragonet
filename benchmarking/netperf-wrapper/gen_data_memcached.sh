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

   ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} -c ${ECHO_SERVER} ${USE_TCP} \
       -H asiago  -C ziger2 \
       --servercores ${SERVERCORES} --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} \
       --concurrency ${CONCURENCY} -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

#       -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
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

        ./netperf-wrapper -I 1 -l ${DURATION} -c ${ECHO_SERVER} ${USE_TCP} \
       -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
       --servercores ${SERVERCORES} --clientcores 2 -T ${target} ${UDP_TEST_NAME} \
       --concurrency ${CBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}

#        ./netperf-wrapper -l ${DURATION} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        #./netperf-wrapper -l ${DURATION} -c ${ECHO_SERVER}   -P ${PKTSIZE} -b ${CBRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
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

    ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} -c ${ECHO_SERVER} ${USE_TCP} \
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

get_per_queue_packets() {
    SUFFIX=$1
   ssh asiago "ethtool -S eth5 | tee bm_ethtool_log.${SUFFIX}" > "${OUTDIR}/bm_ethtool_log.${SUFFIX}"
}

get_best_latency()
{
    SERVERCORES=$1
    CONCURENCY=$2
    CLIENTCORES="1"
    OUTDIR="${OUTDIRP}/LATENCY/"
    mkdir -p ${OUTDIR}
    # echo latency numbers
    get_per_queue_packets "start"
    run_npf_w_rr ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
    get_per_queue_packets "end"
    ./cleanup.sh
    cat "${OUTDIR}/bm_ethtool_log.start" | grep rx | grep packets | grep -v ': 0$'   > "${OUTDIR}/bm_ethtool_pkts.start"
    cat "${OUTDIR}/bm_ethtool_log.end" | grep rx | grep packets | grep -v ': 0$'   > "${OUTDIR}/bm_ethtool_pkts.end"
    diff "${OUTDIR}/bm_ethtool_log.start" "${OUTDIR}/bm_ethtool_log.end" | tee  "${OUTDIR}/bm_ethtool_log.diff"
    diff "${OUTDIR}/bm_ethtool_pkts.start" "${OUTDIR}/bm_ethtool_pkts.end" | tee  "${OUTDIR}/bm_ethtool_pkts.diff"
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
    get_best_tp 10
    get_best_tp 12
    get_best_tp 14
    get_best_tp 16
    get_best_tp 18
}

setup_output_location() {
    OUTDIRPP="${MAIN_OUTPUT_DIR}/${SELTARGET_T}/"

    OUTDIRP="${OUTDIRPP}/${ECHO_SERVER}/${UDP_TEST_NAME}/"
    mkdir -p ${OUTDIRP}
    OUTDIR=${OUTDIRP}

    if [ "${USE_TCP}" == "" ] ; then
        echo "Using UDP"
    else
        echo "Using TCP"
        OUTDIRP="${OUTDIRP}/${USE_TCP}/"
    fi


    echo "OUTPUT Location: ${OUTDIRP}"
}


select_intel_nic() {
    # this is just for general idea, and is not fully tested yet
    sfi=ssh asigago "cat minfo/used_if.log | grep 'asiago-sf[12]-switch' | cut -d, -f1"
    inteli=ssh asigago "cat minfo/used_if.log | grep 'asiago-intel[12]-switch' | cut -d, -f1"
    ssh asigago sudo ifconfig $sfi down
    ssh asigago sudo ifconfig $inteli up
}

#########################################
## Main starts here
#########################################

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

UDP_TEST_NAME="memcached_rr"

ECHO_SERVER="memcached"

ITERATIONS=5
ITERATIONS=1

DURATION=10

USE_TCP="--use-tcp"
USE_TCP=""

MAIN_OUTPUT_DIR="../memcachedResults/pfs/${1}/"
ECHO_SERVER="memcached_poll"
ECHO_SERVER="memcached_onload"
ECHO_SERVER="memcached"

setup_output_location

#get_best_latency server_cores concurency
get_best_latency 2 1

exit 0

######################################################################
######################################################################

USE_TCP=""
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability

USE_TCP="--use-tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability

ECHO_SERVER="memcached_poll"

USE_TCP=""
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability


USE_TCP="--use-tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability

exit 0

ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability


ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_TCP: [${USE_TCP}] "
setup_output_location
get_scalability

exit 0

get_best_latency 1 1
echo "done with benchmarking"
get_scalability

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
