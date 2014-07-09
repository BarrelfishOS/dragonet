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
    while [  "$NTP" -gt "$CTP" ]; do
#        set -x
#        set -e
        let LBRUST=CBRUST
        let CBRUST=NBRUST
        let NBRUST=CBRUST+CBRUST

        title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${CBRUST}"
        echo "#################################################"
        echo "running for ${title}"

        retry 2 ./netperf-wrapper -d ${DELAY} -I 1 -l 10 -c ${ECHO_SERVER} --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${CBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" | tee ${TMPFILE}

        let CTP=NTP

        NVTPS=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPFILE} | grep "^total TPS: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        #GETMISSES=`cat ${TMPFILE} | grep "^total get_misses: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${SERVERCORES}, ${CBRUST}, ${NVTPS}, ${NTP}" >> ${SAMARRYFILE}
        echo "#################################################"
        echo "################### TP of [$NTP:$CBRUST]"
        echo "#################################################"

        cat ${TMPFILE} >> ${FINALRESULT}
        cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
        rm -f ${TMPFILE}
        ./cleanup.sh 2> /dev/null
        sleep 3
    done

    set -x
    set -e

    sleep 5
    title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${LBRUST},BEST"
    echo "running for ${title}"

    retry 2 ./netperf-wrapper -d ${DELAY} -I ${ITERATIONS} -l ${DURATION} -c ${ECHO_SERVER} --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${cliName6} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${LBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" | tee ${TMPFILE}

    set +x
    set +e
    cat ${SAMARRYFILE} >> ${FINALRESULT}
    cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
    rm -f ${SAMARRYFILE}
    echo "done with while [  $NTP -gt $CTP ]; do"
    echo "#############################################################"
    echo "############ BEST TP for instances:${SERVERINSTANCES} CORES:${SERVERCORES} [$NTP:$CBRUST]"
    echo "#############################################################"
    ./cleanup.sh 2> /dev/null
    sleep 3
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


get_best_tp()
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
    OUTDIR="${OUTDIRP}/TP_MAX/S_${SERVERCORES}/"
    mkdir -p ${OUTDIR}
    # for increasing TP on transaction based benchmark
    run_bm_tp_rr  ${SELTARGET} ${SELTARGET_T} ${OUTDIR}
}

get_scalability_linux() {

    HWQUEUES=1

    setup_output_location
    get_best_tp 2 1
    get_best_tp 4 1
    get_best_tp 4 1
    get_best_tp 6 1
    get_best_tp 8 1

    get_best_tp 1 1
    get_best_tp 1 2
    get_best_tp 1 4
    get_best_tp 1 6
    get_best_tp 1 8
    get_best_tp 1 10
    get_best_tp 1 12
    get_best_tp 1 14
    get_best_tp 1 16
    get_best_tp 1 18
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

get_scalability_special22() {

    HWQUEUES=1
    setup_output_location
    get_best_tp 1 1
    get_best_tp 1 2
    get_best_tp 1 4
    get_best_tp 2 1
    get_best_tp 4 1
    HWQUEUES=2
    setup_output_location
    get_best_tp 2 1
    get_best_tp 4 1
    get_best_tp 1 2
    get_best_tp 1 4
    HWQUEUES=4
    setup_output_location
    get_best_tp 1 4
    get_best_tp 4 1

    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}



get_scalability_special() {
    get_best_tp 4 1
    #get_best_tp 1 4
    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}


get_scalability_instances() {
    get_best_tp 1 1
    get_best_tp 2 1
    get_best_tp 4 1
    get_best_tp 8 1
#    get_best_tp 10 1
#    get_best_tp 12 1
#    get_best_tp 14 1
    get_best_tp 16 1
#    get_best_tp 18 1

    fname="scalability-instances-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

get_scalability_threads() {
    get_best_tp 1 1
#    get_best_tp 1 2
    get_best_tp 1 4
    get_best_tp 1 8
#    get_best_tp 1 10
#    get_best_tp 1 12
#    get_best_tp 1 14
#    get_best_tp 1 16
#    get_best_tp 1 18

    fname="scalability-${SELTARGET_T}-${ECHO_SERVER}-${USE_PROTO}.png"
    ./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i `find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
    echo "./netperf-wrapper -p bbox -o ${OUTDIRP}/TP_MAX/${fname} -i \`find ${OUTDIRP}/TP_MAX/ -name '*.json*' | grep -i 'best' | sort\`" >> ${GRAPH_GEN_CMDS}
}

setup_output_location() {
    OUTDIRPP="${MAIN_OUTPUT_DIR}/${SELTARGET_T}/"

    OUTDIRP="${OUTDIRPP}/${ECHO_SERVER}/${USE_PROTO}/P_${PACKET_SIZE}/HWQ_${HWQUEUES}/${CORESHIFT}/${UDP_TEST_NAME}/"
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
    srvName="asiago"

    INTEL_T="10.22.4.95"
    INTEL_S_T="10.113.4.95" # for asiago

    SF_T="10.23.4.195"
    SF_S_T="10.113.4.195"

    cliName6Long="-C ziger2 -C sbrinz2 -C gruyere -C burrata -C ziger2 -C sbrinz2 -C gruyere -C burrata -C ziger2 -C sbrinz2 -C gruyere  -C burrata -C ziger2 -C sbrinz2 -C gruyere -C burrata"
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
CORESHIFT=2
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
MAIN_OUTPUT_DIR="../memcachedResults/sf_scale_test/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/intelTest/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/deleteme_smartOracle/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/smartOracle/r1/${1}/"
GRAPH_GEN_CMDS="${MAIN_OUTPUT_DIR}/graph_gen_cmds.sh"

./cleanup.sh 2> /dev/null

#use_burrata_server_intel_switched
#use_asiago_server_intel_switched
#use_asiago_server_sf_switched
#use_asiago_server_intel_switched
use_asiago_server_sf_switched

UDP_TEST_NAME="udp_rr"
#ECHO_SERVER="netserver"
ECHO_SERVER="llvmE10k"
ECHO_SERVER="fancyEchoOnload"
PACKET_SIZE=1024
MAIN_OUTPUT_DIR="../netperfScaleResults/Linux/${1}/"
setup_output_location
get_scalability_linux
gen_graph ${MAIN_OUTPUT_DIR}
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
get_best_tp 1

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
