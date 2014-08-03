#!/bin/bash

# Retries a command on failure.
# $1 - the max number of attempts
# $2... - the command to run
retry() {
    local -r -i max_attempts="$1"; shift
    local -r cmd="$@"
    local -i attempt_num=1

    echo $cmd

    until $cmd
    do
        if (( attempt_num == max_attempts ))
        then
            echo "Attempt $attempt_num failed and there are no more attempts left!"
            exit 1
            return 1
        else
            echo "Attempt $attempt_num failed! Trying again in $attempt_num seconds..."
            ./cleanup.sh 2> /dev/null
            sleep 5
            sleep $(( attempt_num++ ))
        fi
    done
}

get_per_queue_packets() {
    SUFFIX=$1
    ifname=`ssh ${srvName} "cat minfo/used_if.log | grep ${SELTARGET} | cut -d, -f1"`
    ssh ${srvName} "ethtool -S ${ifname} | tee bm_ethtool_log.${SUFFIX}" > "${OUTDIR}/bm_ethtool_log.${SUFFIX}"
}


run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}_${ECHO_SERVER}"
plotpath="${result}.png"
log="${result}.log"
tmpfile="${result}.tmp"
title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},,SRV_${SERVERCORES},C_${CONCURENCY}"

   retry 1 ./netperf-wrapper -d ${DELAY} -l ${DURATION} -c ${ECHO_SERVER} --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
       --servercores ${SERVERCORES} --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
       --concurrency ${CONCURENCY} -t "${title}" -o "${result}" -L "${log}"

#outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
#./netperf-wrapper -i ${outfile} # -p ${PLOTTYPE} -o ${plotpath}
}


run_bm_tp_rr() {
    target=${1}
    target_t=${2}
    resultLocation=${3}
    result="${3}/latency_${target_t}.png"
    log="${3}/latency_${target_t}.log"
    ONLYBEST=${4}
    ALLRESULTS="${3}/maxTP_${target_t}_${PKTSIZE}/"
    mkdir -p ${ALLRESULTS}

    MYTMPDIR=${ALLRESULTS}
    TMPFILE="${MYTMPDIR}/f1.txt"
    TMPRES="${MYTMPDIR}/f2.txt"
    TMPLOG="${MYTMPDIR}/f1.log"
    FINALRESULT="${ALLRESULTS}/finalResult.result"
    SAMARRYFILE=`mktemp`
    rm -f ${SAMARRYFILE}
    SHORTRUN=5

#    set -x
#    set -e
    CTP=0
    NTP=1
    LBRUST=16
    CBRUST=16
    NBRUST=16


    if [ ${ONLYBEST} -ne 1 ] ; then
        let LBRUST=ONLYBEST
        let CBRUST=ONLYBEST
        let NBRUST=CBRUST+CBRUST
    else

    # Initial reset of stack
    reset_and_warmup_stack ${target} ${target_t} ${resultLocation}

    while [  "$NTP" -gt "$CTP" ]; do
#        set -x
#        set -e
        let LBRUST=CBRUST
        let CBRUST=NBRUST
        let NBRUST=CBRUST+CBRUST

        title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${CBRUST}"
        echo "#################################################"
        echo "running for ${title}"

        retry 2 ./netperf-wrapper -d ${DELAY} -I 1 -l ${SHORTRUN} -c noServer --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${CBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" 2>&1 | tee ${TMPFILE}

        let CTP=NTP

        NVTPS=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`

        if [ ${NBRUST} -gt 2048 ] ; then
            echo "Stopping at ${LBRUST} as clients tend run out of memory beyond this point for memaslap"
            break
        fi

        if [ -z ${NTP} ] ; then
            echo "System si not running atall for given configuration. So, assuming that last was the best run: ${LBRUST}"
            cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
            cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.error
            ./cleanup.sh 2> /dev/null
            break
        fi

        #GETMISSES=`cat ${TMPFILE} | grep "^total get_misses: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${SERVERCORES}, ${CBRUST}, ${NVTPS}, ${NTP}" >> ${SAMARRYFILE}

        echo "#################################################"
        echo "################### TP of [$NTP:$CBRUST]"
        echo "#################################################"

        cat ${TMPFILE} >> ${FINALRESULT}
        cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
        rm -f ${TMPFILE}
        ./cleanup.sh 2> /dev/null
        #sleep 3
    done
    fi

    reset_and_warmup_stack ${target} ${target_t} ${resultLocation}
    set -x
    set -e

#    sleep 5
    title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${LBRUST},BEST"
    echo "running for ${title}"

    retry 4 ./netperf-wrapper -d ${DELAY} -I ${ITERATIONS} -l ${DURATION} -c noServer  --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${LBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" 2>&1 | tee ${TMPFILE}

    set +x
    set +e
    #cat ${SAMARRYFILE} >> ${FINALRESULT}
    cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
    rm -f ${SAMARRYFILE}
    ./cleanup.sh 2> /dev/null
}

reset_and_warmup_stack()
{

    target=${1}
    target_t=${2}
    ALLRESULTS="${3}/maxTP_${target_t}_${PKTSIZE}/"

    brust=2
    SHORTRUN=5

    NTP=1
    iterations=0

    while [ 1 ]; do

    let iterations=iterations+1
    ./cleanupServer.sh

    echo "###################################################################"
    echo "#### Resetting the stack ${ECHO_SERVER} Iteration: ${iterations} ##"
    echo "###################################################################"

    mkdir -p ${ALLRESULTS}

    MYTMPDIR=${ALLRESULTS}
    TMPFILE="${MYTMPDIR}/warmup.txt"

    title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${brust},warmup"
    echo "running for ${title}"

    retry 2 ./netperf-wrapper -d 2 -l ${SHORTRUN} -c ${ECHO_SERVER} --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${brust} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" 2>&1 | tee ${TMPFILE}


    # FIXME: make sure that stack is working properly and all clients are seeing reasonable transacations
    TOTALTPS=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
    NTP=`cat ${TMPFILE} | grep "^MIN TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
    if [[ ! -z "${NTP}" ]] && [[ "${NTP}" -ge 100 ]] ; then
        echo "Stack is running and working properly (reported min TPS ${NTP})"
        break
    fi
    if [[ ! -z "${TOTALTPS}" ]] && [[ "${TOTALTPS}" -ge 100 ]] ; then
    echo "Seems that only few clients are stuck, so retyring quickly without restarting whole stack"

        retry 2 ./netperf-wrapper -d 2 -l ${SHORTRUN} -c noServer --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${brust} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" 2>&1 | tee ${TMPFILE}

        NTP11=`cat ${TMPFILE} | grep "^MIN TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        if [[ ! -z "${NTP11}" ]] && [[ "${NTP11}" -ge 100 ]] ; then
            echo "Stack is running and working properly (reported min TPS        ${NTP11})"
            break
        fi
    fi

    if [ ${iterations} -gt 3 ] ; then
        echo "We have already tried to reset the stack ${iterations} times, so giving up"
        exit 1
    fi
    sleep 3

    done
    cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
    ./cleanup.sh 2> /dev/null
}


get_best_latency()
{
    SERVERINSTANCES=$1
    SERVERCORES=$2
    CONCURENCY=$3
    CLIENTCORES="1"
    OUTDIR="${OUTDIRP}/LATENCY/"
    mkdir -p ${OUTDIR}
    # echo latency numbers
    get_per_queue_packets "start"
    run_npf_w_rr ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
    get_per_queue_packets "end"
    #./cleanup.sh
    cat "${OUTDIR}/bm_ethtool_log.start" | grep rx | grep packets | grep -v ': 0$'   > "${OUTDIR}/bm_ethtool_pkts.start"
    cat "${OUTDIR}/bm_ethtool_log.end" | grep rx | grep packets | grep -v ': 0$'   > "${OUTDIR}/bm_ethtool_pkts.end"
    diff "${OUTDIR}/bm_ethtool_log.start" "${OUTDIR}/bm_ethtool_log.end" | tee  "${OUTDIR}/bm_ethtool_log.diff"
    echo "Only packet difference"

    diff "${OUTDIR}/bm_ethtool_pkts.start" "${OUTDIR}/bm_ethtool_pkts.end" | tee  "${OUTDIR}/bm_ethtool_pkts.diff"
}




find_best_tp()
{
    SERVERINSTANCES=$1
    SERVERCORES=$2

    if [  "$SERVERINSTANCES" -gt "1" ]; then
        cliName6=${cliName6Short}
    else
        cliName6=${cliName6Long}
    fi
    echo "using client list ${cliName6}"
    #CLIENTCORES="2"
    CLIENTCORES="1"
    OUTDIR="${OUTDIRP}/TP_MAX/P_${SERVERCORES}/T_${SERVERCORES}/"
    mkdir -p ${OUTDIR}
    # for increasing TP on transaction based benchmark
    run_bm_tp_rr  ${SELTARGET} ${SELTARGET_T} ${OUTDIR} ${FOR_BEST_RUN}
}

get_scalability_linux_rss() {

    HWQUEUES=1
    setup_output_location
    find_best_tp 1 2
}



get_scalability_linux_sp() {

    HWQUEUES=1
    setup_output_location

    find_best_tp 8 1
    find_best_tp 16 1
    find_best_tp 10 1
    find_best_tp 12 1
    find_best_tp 14 1
}


get_scalability_only_one_para() {
    threads=${1}
    find_best_tp 1 ${threads}
}



get_scalability_only_one() {
    find_best_tp 1 4
}

get_scalability_all() {

    find_best_tp 1 6

#    setup_output_location
    find_best_tp 1 1
    find_best_tp 1 2
    find_best_tp 1 4
    find_best_tp 1 6
    find_best_tp 1 8
    find_best_tp 1 10
}

get_scalability_all_v2() {
    find_best_tp 1 10
    find_best_tp 1 12
    find_best_tp 1 14
    find_best_tp 1 16
    find_best_tp 1 18
}



get_scalability_linux() {

    HWQUEUES=1
    setup_output_location
    find_best_tp 1 1
    find_best_tp 1 2
    find_best_tp 1 4
    find_best_tp 1 6
    find_best_tp 1 8
    find_best_tp 1 16
    find_best_tp 1 10
    find_best_tp 1 12
    find_best_tp 1 14
    find_best_tp 1 18
}

get_scalability_linux_multiport() {
    find_best_tp 2 1
    find_best_tp 4 1
    find_best_tp 6 1
    find_best_tp 8 1
    find_best_tp 16 1
}

get_scalability_remaining() {

    HWQUEUES=1
    setup_output_location

    find_best_tp 1 6
    find_best_tp 1 10
    find_best_tp 1 12
    find_best_tp 1 14
    find_best_tp 1 18

    find_best_tp 6 1
    find_best_tp 10 1
    find_best_tp 12 1
    find_best_tp 14 1
    find_best_tp 18 1

    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

gen_graph() {
    pdir=${1}
    fname="scalability-TPS-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    fnameNet="scalability-BW-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    graphGenFile="${pdir}/graphGen.sh"
    ./netperf-wrapper -p bbox -o ${pdir}/${fname} -i `find ${pdir} -name '*BEST.json*' | sort`
    ./netperf-wrapper -p bboxNet -o ${pdir}/${fnameNet} -i `find ${pdir} -name '*BEST.json*' | sort`

    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" > ${graphGenFile}
    echo "./netperf-wrapper -p bboxNet -o ${OUTDIRP}/TP_MAX/${fnameNet} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${graphGenFile}
}

get_scalability_dn() {

    HWQUEUES=1
    setup_output_location
    find_best_tp 1 1
    find_best_tp 1 2
    find_best_tp 1 4
    find_best_tp 2 1
    find_best_tp 4 1
    HWQUEUES=2
    setup_output_location
    find_best_tp 1 2
    find_best_tp 1 4
    find_best_tp 2 1
    find_best_tp 4 1
    HWQUEUES=4
    setup_output_location
    find_best_tp 1 4
    find_best_tp 4 1

#    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
#    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
#    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}



get_scalability_special() {
#    find_best_tp 1 4
    find_best_tp 4 1
    #find_best_tp 1 1
    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}


get_scalability_instances() {
    find_best_tp 1 1
    find_best_tp 2 1
    find_best_tp 4 1
    find_best_tp 8 1
#    find_best_tp 10 1
#    find_best_tp 12 1
#    find_best_tp 14 1
    find_best_tp 16 1
#    find_best_tp 18 1

    fname="scalability-instances-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

get_scalability_threads() {
    find_best_tp 1 1
#    find_best_tp 1 2
    find_best_tp 1 4
    find_best_tp 1 8
#    find_best_tp 1 10
#    find_best_tp 1 12
#    find_best_tp 1 14
#    find_best_tp 1 16
#    find_best_tp 1 18

    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

setup_output_location() {
    OUTDIRPP="${MAIN_OUTPUT_DIR}/${SELTARGET_T}/"
    OUTDIRP="${OUTDIRPP}/${UDP_TEST_NAME}/${ECHO_SERVER}/${USE_PROTO}/P_${PACKET_SIZE}/HWQ_${HWQUEUES}/${CORESHIFT}/"
    mkdir -p ${OUTDIRP}
    OUTDIR=${OUTDIRP}
    echo "OUTPUT Location: ${OUTDIR}"
}


select_intel_nic() {
    # this is just for general idea, and is not fully tested yet
    sfi=`ssh ${srvName} "cat minfo/used_if.log | grep '${srvName}-sf[12]-switch' | cut -d, -f1"`
    inteli=`ssh ${srvName} "cat minfo/used_if.log | grep '${srvName}-intel[12]-switch' | cut -d, -f1"`
    ssh ${srvName} "sudo ifconfig $sfi down"
    ssh ${srvName} "sudo ifconfig $inteli up"
    rm asiago_details.mconf
    SELTARGET=${INTEL_S_T}
    SELTARGET_T="Intel_S"
}

select_sf_nic() {
    # this is just for general idea, and is not fully tested yet
    sfi=`ssh ${srvName} "cat minfo/used_if.log | grep '${srvName}-sf[12]-switch' | cut -d, -f1"`
    inteli=`ssh ${srvName} "cat minfo/used_if.log | grep '${srvName}-intel[12]-switch' | cut -d, -f1"`
    ssh ${srvName} "sudo ifconfig $inteli down"
    ssh ${srvName} "sudo ifconfig $sfi up"
    rm asiago_details.mconf
    rm *.mconf
    SELTARGET=${SF_S_T}
    SELTARGET_T="SF_S"
    # pinging ziger2's 10G ip address
    ssh ${srvName} "sudo ping -c 10 10.113.4.57"
}

use_burrata_server_intel_switched() {
    srvName="burrata"
    INTEL_S_T="10.113.4.96" # for burrata
    cliName6="-C ziger2 -C sbrinz2 -C ziger2 -C sbrinz2"
    cliName1="-C ziger2"
    SELTARGET=${INTEL_S_T}
    SELTARGET_T="Intel_S"
}

use_sbrinz2_server_intel_switched() {
    srvName="sbrinz2"
    INTEL_S_T="10.113.4.29"
    cliName6="-C ziger2 -C ziger2 "
    cliName1="-C ziger2"
}

use_asiago_server() {

    rm asiago_details.mconf
    srvName="asiago"

    INTEL_T="10.22.4.95"
    INTEL_S_T="10.113.4.95" # for asiago

    SF_T="10.23.4.195"
    SF_S_T="10.113.4.195"

    cliName6Long="-C ziger2 -C sbrinz2 -C gruyere -C burrata -C ziger2 -C sbrinz2 -C gruyere -C burrata -C ziger2 -C sbrinz2 -C gruyere  -C burrata -C ziger2 -C sbrinz2 -C gruyere -C burrata"
    cliName6Long="-C burrata -C gruyere -C  ziger2 -C sbrinz2 -C burrata -C gruyere -C  ziger2 -C sbrinz2  -C burrata -C gruyere -C  ziger2 -C sbrinz2 -C burrata -C gruyere -C  ziger2 -C sbrinz2"
    #cliName6Long="-C ziger2 -C sbrinz2 -C gruyere -C burrata"
    cliName6Short="-C ziger2 -C sbrinz2 -C gruyere -C burrata"
#    cliName6Long="-C ziger2 -C sbrinz2 -C gruyere -C burrata -C ziger2 -C sbrinz2 -C gruyere -C burrata"
    cliName6="-C ziger2 -C sbrinz2 -C gruyere -C burrata"
    cliName1="-C ziger2"
    cliName1="-C sbrinz2 -C sbrinz2"
    cliName6=${cliName6Short}
}

use_asiago_server_sf_switched() {
    use_asiago_server
    SELTARGET=${SF_S_T}
    SELTARGET_T="SF_S"
}

use_asiago_server_intel_switched() {
    use_asiago_server
    SELTARGET=${INTEL_S_T}
    SELTARGET_T="Intel_S"
}



#########################################
## Main starts here
#########################################

PLOTTYPE="bbest"
PLOTTYPE="lbest"
UDP_TEST_NAME="memcached_rr"


ECHO_SERVER="memcached"

ITERATIONS=1
ITERATIONS=5
ITERATIONS=3

DURATION=10
CORESHIFT=0
SERVERINSTANCES=1

DELAY=5
DELAY=2
HWQUEUES=1
HWQUEUES=4
USE_PROTO="tcp"
USE_PROTO="udp"
PACKET_SIZE=1024

ECHO_SERVER="memcached_poll"
ECHO_SERVER="memcached_dragonet"
ECHO_SERVER="memcached_onload"
ECHO_SERVER="memcached"

MAIN_OUTPUT_DIR="../netperfScaleResults/intelTest/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/deleteme_smartOracle/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/smartOracle/r1/${1}/"
GRAPH_GEN_CMDS="${MAIN_OUTPUT_DIR}/graph_gen_cmds.sh"


./cleanup.sh 2> /dev/null

#use_burrata_server_intel_switched
#use_asiago_server_intel_switched
#use_asiago_server_sf_switched

UDP_TEST_NAME="memcached_rr"


#use_asiago_server_intel_switched
#ECHO_SERVER="memcached"
#ECHO_SERVER="memcached_poll"
#ECHO_SERVER="llvmE10k"
#use_asiago_server_intel_switched

#use_asiago_server_sf_switched
#ECHO_SERVER="memcached"
#ECHO_SERVER="memcached_onload"
#ECHO_SERVER="llvmSF"

PACKET_SIZE=1024
HWQUEUES=1
UDP_TEST_NAME="udp_rr"
UDP_TEST_NAME="memcached_rr"

#use_asiago_server_sf_switched
#ECHO_SERVER="memcached"
#ECHO_SERVER="memcached_onload"
ECHO_SERVER="llvmSF"
MAIN_OUTPUT_DIR="../memcachedResults_v2/scalability/P1024/Dragonet_NRT_SF_Q1/"


FOR_BEST_RUN=1
HWQUEUES=4
UDP_TEST_NAME="memcached_rr"
#ECHO_SERVER="noServer"
#ECHO_SERVER="llvmSF"
#use_asiago_server_sf_switched
ECHO_SERVER="llvmE10k"
use_asiago_server_intel_switched
#MAIN_OUTPUT_DIR="../memcachedResults_v2/scalability/P1024/Dragonet_NRT_SF/"
MAIN_OUTPUT_DIR="../memcachedResults_v2/scalability/P1024/Dragonet_NRT_Intel_N/"
cliName6Long="-C burrata -C gruyere -C  ziger2 -C burrata -C gruyere -C  ziger2 -C sbrinz2 -C burrata -C gruyere -C  ziger2 -C sbrinz2"

setup_output_location
get_scalability_only_one_para 1
get_scalability_only_one_para 2
get_scalability_only_one_para 4
get_scalability_only_one_para 6
get_scalability_only_one_para 8
get_scalability_only_one_para 12
get_scalability_only_one_para 14
get_scalability_only_one_para 16
get_scalability_only_one_para 18
exit 0
get_scalability_only_one_para 10

#get_scalability_only_one_para 8

#use_asiago_server_intel_switched
#MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_NRT_FLOWS_E10k_Q4/"


#MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_NRT_FLOWS_SF_Q4/"

FOR_BEST_RUN=1
MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_NRT_FLOWS_SF_Q4_D/"

#FOR_BEST_RUN=64
#MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_NRT_FLOWS_E10k_Q4_D/"

cliName6Long="-C burrata -C gruyere -C  ziger2 -C sbrinz2 -C burrata -C gruyere -C  ziger2 -C sbrinz2 -C burrata -C gruyere -C  ziger2 -C sbrinz2  -C burrata -C gruyere -C  ziger2 -C sbrinz2"




UDP_TEST_NAME="udp_rr"
HWQUEUES=4
#ECHO_SERVER="llvmE10k"
ECHO_SERVER="noServer"
use_asiago_server_intel_switched
MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_NRT_FLOWS_E10k_Q4/"
setup_output_location
get_scalability_only_one
exit 0


use_asiago_server_intel_switched
ECHO_SERVER="llvmE10k"

MAIN_OUTPUT_DIR="../memcachedResults_v2/scalability/P1024/Dragonet_Intel_Q1/"
setup_output_location
get_scalability_only_one
exit 0

get_scalability_all

get_scalability_only_one
get_scalability_only_one

#get_scalability_only_one

UDP_TEST_NAME="udp_rr"

PACKET_SIZE=64

#ECHO_SERVER="netserver"
ECHO_SERVER="llvmE10k"
ECHO_SERVER="fancyEchoOnload"
ECHO_SERVER="fancyEchoOnload"
ECHO_SERVER="fancyEchoOnload"
ECHO_SERVER="fancyEchoLinuxPoll"



#ECHO_SERVER="llvmE10k"
#use_asiago_server_intel_switched
ECHO_SERVER="llvmSF"
use_asiago_server_sf_switched

PACKET_SIZE=1024
MAIN_OUTPUT_DIR="../netperfScaleResults/Scalability/P1024/dragonet_SF_Q2/${1}/"
HWQUEUES=2
setup_output_location
get_scalability_all
exit 0


##############################

get_scalability_all
get_scalability_linux_rss
./graphGen.sh ${MAIN_OUTPUT_DIR}
get_scalability_linux_sp
exit 0

##############################

#get_scalability_linux
get_scalability_dn

MAIN_OUTPUT_DIR="../netperfScaleResults/dnIntelSp/${1}/"
setup_output_location

get_scalability_special22
./graphGen.sh ${MAIN_OUTPUT_DIR}

exit 0
##############################


MAIN_OUTPUT_DIR="../netperfScaleResults/smartOracle/rAll/${1}/"
PACKET_SIZE=1024
setup_output_location
get_scalability_special22
PACKET_SIZE=64
setup_output_location
get_scalability_special22
PACKET_SIZE=512
setup_output_location
get_scalability_special22
PACKET_SIZE=128
setup_output_location
get_scalability_special22
exit 0

get_scalability_special
##############################

ECHO_SERVER="memcached"
setup_output_location
get_scalability_instances
exit 0
##############################
get_scalability_threads
get_best_latency 1 1 1


get_scalability_instances

get_scalability_special

get_scalability_threads
./cleanup.sh 2> /dev/null
sleep 10

CORESHIFT=6
setup_output_location
get_scalability_threads

exit 0
##############################

get_scalability_special

get_best_latency 1 1 1
get_scalability_special
find_best_tp 1

#exit 0

#get_best_latency server-instances server-cores concurency
#get_best_latency 2 1 1
#exit 0

############################## getting SF data ######
#select_sf_nic
starttime=`date`
ECHO_SERVER="memcached"

USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

ECHO_SERVER="memcached_onload"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

ECHO_SERVER="memcached"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads



############################## getting Intel data ######
#select_intel_nic

ECHO_SERVER="memcached"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

ECHO_SERVER="memcached_poll"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

endtime=`date`
echo "BM start time  ${starttime}"
echo "BM   end time  ${endtime}"
exit 0


######################################################################
######################################################################

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

ECHO_SERVER="memcached_poll"

USE_PROTO="udp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads


USE_PROTO="tcp"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

exit 0

ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads


ECHO_SERVER="memcached_onload"
echo "BM : ${ECHO_SERVER}, Target: ${SELTARGET}, USE_PROTO: [${USE_PROTO}] "
setup_output_location
get_scalability_threads

exit 0

get_best_latency 1 1 1
echo "done with benchmarking"
get_scalability_threads

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
