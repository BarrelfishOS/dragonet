#!/bin/bash

run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}_${ECHO_SERVER}"
plotpath="${result}.png"
log="${result}.log"
tmpfile="${result}.tmp"
title="${ECHO_SERVER},${target_t},${USE_PROTO},,SRV_${SERVERCORES},C_${CONCURENCY}"

   ./netperf-wrapper -l ${DURATION} -c ${ECHO_SERVER} --${USE_PROTO} \
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

        title="${ECHO_SERVER},${target_t},${USE_PROTO},,SRV_${SERVERCORES},C_${CBRUST}"

        ./netperf-wrapper -I 1 -l 10 -c ${ECHO_SERVER} --${USE_PROTO} \
       -H asiago -C ziger2 -C sbrinz2 -C gruyere -C ziger2 -C sbrinz2 -C gruyere \
       --servercores ${SERVERCORES} --clientcores 2 -T ${target} ${UDP_TEST_NAME} \
       --concurrency ${CBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}

#        ./netperf-wrapper -l ${DURATION} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        #./netperf-wrapper -l ${DURATION} -c ${ECHO_SERVER}   -P ${PKTSIZE} -b ${CBRUST} -H asiago -C ziger2 -C sbrinz2 -C gruyere -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        #outfile=`cat ${TMPFILE} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
        #./netperf-wrapper -i ${outfile} | tee ${TMPRES}

        let CTP=NTP

        NVTPS=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        GETMISSES=`cat ${TMPFILE} | grep "^total get_misses: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${SERVERCORES}, ${CBRUST}, ${GETMISSES}, ${NVTPS}, ${NTP}" >> ${SAMARRYFILE}
        echo "#################################################"
        echo "################### TP of [$NTP:$CBRUST]"
        echo "#################################################"

        cat ${TMPFILE} >> ${FINALRESULT}
        rm -f ${TMPFILE}
        ./cleanup.sh
    done

    set -x
    set -e

    title="${ECHO_SERVER},${target_t},${USE_PROTO},,SRV_${SERVERCORES},C_${LBRUST},BEST"

    ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION} -c ${ECHO_SERVER} --${USE_PROTO} \
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
    ./cleanup.sh
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

get_scalability_special() {
    get_best_tp 10
    get_best_tp 8
    get_best_tp 12
    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
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

    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

setup_output_location() {
    OUTDIRPP="${MAIN_OUTPUT_DIR}/${SELTARGET_T}/"

    OUTDIRP="${OUTDIRPP}/${ECHO_SERVER}/${USE_PROTO}/${UDP_TEST_NAME}/"
    mkdir -p ${OUTDIRP}
    OUTDIR=${OUTDIRP}
    echo "OUTPUT Location: ${OUTDIR}"
}


select_intel_nic() {
    # this is just for general idea, and is not fully tested yet
    sfi=`ssh asiago "cat minfo/used_if.log | grep 'asiago-sf[12]-switch' | cut -d, -f1"`
    inteli=`ssh asiago "cat minfo/used_if.log | grep 'asiago-intel[12]-switch' | cut -d, -f1"`
    ssh asiago "sudo ifconfig $sfi down"
    ssh asiago "sudo ifconfig $inteli up"
    rm asiago_details.mconf
    SELTARGET=${INTEL_S_T}
    SELTARGET_T="Intel_S"
}

select_sf_nic() {
    # this is just for general idea, and is not fully tested yet
    sfi=`ssh asiago "cat minfo/used_if.log | grep 'asiago-sf[12]-switch' | cut -d, -f1"`
    inteli=`ssh asiago "cat minfo/used_if.log | grep 'asiago-intel[12]-switch' | cut -d, -f1"`
    ssh asiago "sudo ifconfig $inteli down"
    ssh asiago "sudo ifconfig $sfi up"
    rm asiago_details.mconf
    rm *.mconf
    SELTARGET=${SF_S_T}
    SELTARGET_T="SF_S"
    # pinging ziger2's 10G ip address
    ssh asiago "sudo ping -c 10 10.113.4.57"
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
SELTARGET_T="SF_S"

SELTARGET=${SF_S_T}
SELTARGET_T="SF_S"

SELTARGET=${INTEL_S_T}
SELTARGET_T="Intel_S"

UDP_TEST_NAME="memcached_rr"

ECHO_SERVER="memcached"

ITERATIONS=5
ITERATIONS=3

DURATION=10

USE_PROTO="tcp"
USE_PROTO="udp"

MAIN_OUTPUT_DIR="../memcachedResults/sfDebug/${1}/"
ECHO_SERVER="memcached_onload"
ECHO_SERVER="memcached_poll"
ECHO_SERVER="memcached"

GRAPH_GEN_CMDS="${MAIN_OUTPUT_DIR}/graph_gen_cmds.sh"

setup_output_location

#get_scalability
#exit 0

#get_best_latency server_cores concurency
#get_best_latency 2 1
#exit 0

############################## getting SF data ######
select_sf_nic
starttime=`date`
ECHO_SERVER="memcached"

USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

ECHO_SERVER="memcached_onload"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

ECHO_SERVER="memcached"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability



############################## getting Intel data ######
select_intel_nic

ECHO_SERVER="memcached"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

ECHO_SERVER="memcached_poll"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

endtime=`date`
echo "BM start time  ${starttime}"
echo "BM   end time  ${endtime}"
exit 0


######################################################################
######################################################################

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

ECHO_SERVER="memcached_poll"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability

exit 0

ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability


ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
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
