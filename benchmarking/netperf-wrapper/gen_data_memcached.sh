#!/bin/bash

get_x_entries() {
    local total_entries=${1}
    local clist=${2}
    local done=1

    local -i attempt_num=1

while [ ${done} == 1 ] ;
do
    for x in `echo "${clist}" | tr ' ' '\n'` ; do
        selectedList="${selectedList} ${x}"
        if [ ${attempt_num} == ${total_entries} ] ; then
            done=0
            break
        fi
        ((attempt_num++))
    done
done

echo ${selectedList}
}



clean_machine() {
    local mname=$1
    local commands_to_kill=""
    commands_to_kill="${commands_to_kill} memaslap"
    commands_to_kill="${commands_to_kill} netperf"
    commands_to_kill="${commands_to_kill} netserver"
    commands_to_kill="${commands_to_kill} memcached"
    commands_to_kill="${commands_to_kill} bench-fancyecho"
    commands_to_kill="${commands_to_kill} stack-sf"
    commands_to_kill="${commands_to_kill} stack-tap"
    commands_to_kill="${commands_to_kill} stack-e10k"
    commands_to_kill="${commands_to_kill} fancyEchoLinux"
    echo "ssh ${mname} sudo killall ${commands_to_kill}"
    ssh ${mname} "sudo killall ${commands_to_kill}"
    ssh ${mname} "sudo rm -rf tempResult*"
}

cleanup_machines() {
    local allClients=$1

    for x in `echo "${allClients}" | tr ' ' '\n'` ; do
        echo "cleaning machine $x"
        clean_machine ${x} 2> /dev/null
    done
}

convert_clients_to_cmdline() {
    local allClients=$1
    local answer=""
    for x in `echo "${allClients}" | tr ' ' '\n'` ; do
        answer="${answer} -C ${x}"
    done

    echo "${answer} "
}


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
            cleanup_machines "${ClientListUnique}"
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
    local clientCmdline=$(convert_clients_to_cmdline  "${ClientList}")


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
        -H ${srvName} ${clientCmdline} \
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
            cleanup_machines "${ClientListUnique}"
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
        cleanup_machines "${ClientListUnique}"
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
        -H ${srvName} ${clientCmdline} \
        --servercores ${SERVERCORES} --serverInstances ${SERVERINSTANCES} --hwqueues ${HWQUEUES} \
        --clientcores ${CLIENTCORES} -T ${target} ${UDP_TEST_NAME} --packet ${PACKET_SIZE} \
        --concurrency ${LBRUST} -t "${title}" -o "${ALLRESULTS}" -L "${MYTMPDIR}/${title}.log" 2>&1 | tee ${TMPFILE}

    set +x
    set +e
    #cat ${SAMARRYFILE} >> ${FINALRESULT}
    cat ${TMPFILE} >>  ${MYTMPDIR}/${title}.log
    rm -f ${SAMARRYFILE}
    cleanup_machines "${ClientListUnique}"
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
    cleanup_machines "${ClientListUnique}"
    cleanup_machines "${srvName}"

    local clientCmdline=$(convert_clients_to_cmdline  "${ClientList}")

    echo "###################################################################"
    echo "#### Resetting the stack ${ECHO_SERVER} Iteration: ${iterations} ##"
    echo "###################################################################"

    mkdir -p ${ALLRESULTS}

    MYTMPDIR=${ALLRESULTS}
    TMPFILE="${MYTMPDIR}/warmup.txt"

    title="${ECHO_SERVER},${target_t},${USE_PROTO},${CORESHIFT},Q_${HWQUEUES},P_${PACKET_SIZE},,SRVI_${SERVERINSTANCES},SRV_${SERVERCORES},C_${brust},warmup"
    echo "running for ${title}"

    retry 2 ./netperf-wrapper -d 2 -l ${SHORTRUN} -c ${ECHO_SERVER} --${USE_PROTO}  --serverCoreShift ${CORESHIFT} \
        -H ${srvName} ${clientCmdline} \
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
        -H ${srvName} ${clientCmdline} \
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
    cleanup_machines "${ClientListUnique}"
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

#    if [  "$SERVERINSTANCES" -gt "1" ]; then
#        ClientList=${cliName6Short}
#    else
#        ClientList=${cliName6Long}
#    fi

    echo "using client list ${ClientList}"
    #CLIENTCORES="2"
    CLIENTCORES="1"
    OUTDIR="${OUTDIRP}/TP_MAX/P_${SERVERCORES}/T_${SERVERCORES}/"
    mkdir -p ${OUTDIR}
    # for increasing TP on transaction based benchmark
    run_bm_tp_rr  ${SELTARGET} ${SELTARGET_T} ${OUTDIR} ${FOR_BEST_RUN}
}

################################################################
#                  diff order of calling benchmark
################################################################

get_scalability_linux_rss() {

    HWQUEUES=1
    setup_output_location
    find_best_tp 1 2
}

get_main_scalability_dragonet() {

ClientList=$(get_x_entries 4 "${ClientListUnique}")
get_scalability_only_one_para 1

ClientList=$(get_x_entries 8 "${ClientListUnique}")
get_scalability_only_one_para 4
get_scalability_only_one_para 8

ClientList=$(get_x_entries 10 "${ClientListUnique}")
get_scalability_only_one_para 10

ClientList=$(get_x_entries 16 "${ClientListUnique}")
get_scalability_only_one_para 16
}

get_all_scalability_dragonet() {

ClientList=$(get_x_entries 4 "${ClientListUnique}")
get_scalability_only_one_para 1
get_scalability_only_one_para 2

ClientList=$(get_x_entries 8 "${ClientListUnique}")
get_scalability_only_one_para 4
get_scalability_only_one_para 6
get_scalability_only_one_para 8

ClientList=$(get_x_entries 10 "${ClientListUnique}")
get_scalability_only_one_para 10

ClientList=$(get_x_entries 12 "${ClientListUnique}")
get_scalability_only_one_para 12

ClientList=$(get_x_entries 16 "${ClientListUnique}")
get_scalability_only_one_para 16
}

get_all_loadbalancing_dragonet() {

MAIN_OUTPUT_DIR_CP="${MAIN_OUTPUT_DIR}"
MAIN_OUTPUT_DIR="${MAIN_OUTPUT_DIR_CP}/F_10/"
setup_output_location
ClientList=$(get_x_entries 10 "${ClientListUnique}")
get_scalability_only_one_para 10


MAIN_OUTPUT_DIR="${MAIN_OUTPUT_DIR_CP}/F_20/"
setup_output_location
ClientList=$(get_x_entries 20 "${ClientListUnique}")
get_scalability_only_one_para 10

MAIN_OUTPUT_DIR="${MAIN_OUTPUT_DIR_CP}/F_30/"
setup_output_location
ClientList=$(get_x_entries 30 "${ClientListUnique}")
get_scalability_only_one_para 10

MAIN_OUTPUT_DIR="${MAIN_OUTPUT_DIR_CP}/F_40/"
setup_output_location
ClientList=$(get_x_entries 40 "${ClientListUnique}")
get_scalability_only_one_para 10
}



get_scalability_only_one_para() {
    threads=${1}
    find_best_tp 1 ${threads}
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

}

################################################################
################################################################



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

    #ClientList="ziger2 sbrinz2 ziger2 sbrinz2"
    ClientList="ziger1 ziger2 sbrinz2 appenzeller-e1000"
    ClientListUnique="ziger1 ziger2 sbrinz2 appenzeller-e1000"
    SELTARGET=${INTEL_S_T}
    SELTARGET_T="Intel_S"
}

use_sbrinz2_server_intel_switched() {
    srvName="sbrinz2"
    INTEL_S_T="10.113.4.29"
    ClientList="ziger2 ziger2"
}

use_asiago_server() {

    rm asiago_details.mconf
    srvName="asiago"

    INTEL_T="10.22.4.95"
    INTEL_S_T="10.113.4.95" # for asiago

    SF_T="10.23.4.195"
    SF_S_T="10.113.4.195"

    ClientList="ziger1 ziger2 sbrinz2 appenzeller-e1000"
    ClientListUnique="ziger1 ziger2 sbrinz2 appenzeller-e1000"
    ClientList=${cliName6Short}
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

ECHO_SERVER="memcached"

ITERATIONS=3

DURATION=10
CORESHIFT=0
SERVERINSTANCES=1
FOR_BEST_RUN=1
DELAY=2

HWQUEUES=10
USE_PROTO="tcp"
USE_PROTO="udp"
PACKET_SIZE=64
PACKET_SIZE=1024

MAIN_OUTPUT_DIR="../netperfScaleResults/intelTest/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/deleteme_smartOracle/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/smartOracle/r1/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/deletme_test/${1}/"
MAIN_OUTPUT_DIR="../netperfScaleResults/deletme_test/${1}/"

MAIN_OUTPUT_DIR="../echoServerResults/EchoP1024DP/dragonet_Intel_Q10/${1}/"
MAIN_OUTPUT_DIR="../echoServerResults/EchoP64DP/dragonet_Intel_Q10/${1}/"
MAIN_OUTPUT_DIR="../echoServerResults/loadBalacingP64/Dragonet_Intel_Q10/${1}/"
MAIN_OUTPUT_DIR="../echoServerResults/MemcachedP1024DP/Dragonet_Intel_Q10/${1}/"

MAIN_OUTPUT_DIR="../echoServerResults/EchoP1024DP/dragonet_SF_Q10/${1}/"

#use_burrata_server_intel_switched
#use_asiago_server_intel_switched
use_asiago_server_sf_switched
UDP_TEST_NAME="memcached_rr"
UDP_TEST_NAME="udp_rr"
ECHO_SERVER="llvmE10k"
#ECHO_SERVER="noServer"
ECHO_SERVER="llvmSF"

cleanup_machines "${ClientListUnique}"
setup_output_location
get_main_scalability_dragonet
exit 0
get_all_scalability_dragonet

ClientList="ziger1 ziger2 sbrinz2 appenzeller-e1000"
ClientList=$(get_x_entries 12 "${ClientListUnique}")
get_scalability_only_one_para 12
exit 0


get_all_scalability_dragonet

get_all_loadbalancing_dragonet
exit 0


get_all_scalability_dragonet
ClientList=$(get_x_entries 4 "${ClientListUnique}")
get_scalability_only_one_para 4
exit 0


ECHO_SERVER="llvmE10k"
use_asiago_server_intel_switched
MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_Intel_N/"
setup_output_location
get_all_scalability_dragonet_special

ECHO_SERVER="llvmSF"
use_asiago_server_sf_switched
MAIN_OUTPUT_DIR="../netperfScaleNRTS/scalability/P1024/Dragonet_SF_N/"
setup_output_location
get_all_scalability_dragonet_special
#get_all_scalability_dragonet

exit 0

#####################################################################
