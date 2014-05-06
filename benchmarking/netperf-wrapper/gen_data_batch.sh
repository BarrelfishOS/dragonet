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

   ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION}${ONLOADTYPE} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${BRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
./netperf-wrapper -i ${outfile} -p ${PLOTTYPE} -o ${plotpath}
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

        ./netperf-wrapper -l ${DURATION} ${ONLOADTYPE} -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${CBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
        outfile=`cat ${TMPFILE} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
        ./netperf-wrapper -i ${outfile} -p ${PLOTTYPE} -o ${TMPPLOT} | tee ${TMPRES}

        let CTP=NTP

        NBWD=`cat ${TMPRES} | grep "^THROUGHPUT: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        NLATENCY=`cat ${TMPRES} | grep "^RT_LATENCY: " | head -n1 |cut -d'[' -f2 | cut -d']' -f1`
        NTP=`cat ${TMPRES} | grep "^TRANSACTION_RATE: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1 | cut -d'.' -f1`
        SCPU=`cat ${TMPRES} | grep "^Server CPU: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        CCPU=`cat ${TMPRES} | grep "^Client CPU: " | head -n1 | cut -d'[' -f2 | cut -d']' -f1`
        echo "TPITERATOR: ${title}, ${target}, ${PKTSIZE}, ${CBRUST}, ${SCPU}, ${CCPU}, ${NTP}, ${NLATENCY}, ${NBWD}" >> ${SAMARRYFILE}
        echo "TP of $NTP for $CBRUST"

        cat ${TMPRES} >> ${FINALRESULT}
        rm -f ${TMPFILE}
        rm -f ${TMPRES}
        rm -f ${TMPPLOT}
    done

    set -x
    set -e

        title="${ECHO_SERVER}_${target_t}_PKT_${PKTSIZE}_B_${LBRUST}${ONLOADTEXT}_BEST"
        ./netperf-wrapper -I ${ITERATIONS} -l ${DURATION}${ONLOADTYPE}  -c ${ECHO_SERVER} -P ${PKTSIZE} -b ${LBRUST} -H asiago -C burrata -T ${target} ${UDP_TEST_NAME} -t "${title}" -o "${ALLRESULTS}" -L "${TMPLOG}" | tee ${TMPFILE}
    set +x
    set +e
    cat ${SAMARRYFILE} >> ${FINALRESULT}
    rm -f ${SAMARRYFILE}
    sleep 3
    echo "while [  $NTP -gt $CTP ]; do"
}




run_npf_w_stream()
{
target=${1}
target_t=${2}
result="${3}/stream_tp_${target_t}"
plotpath="${3}/stream_tp_${target_t}.png"
FINALRESULT="${3}/${target_t}finalResult.result"
log="${3}/stream_tp_${target_t}.log"
tmpfile="${3}/stream_tp_${target_t}.tmp"
title="${target_t}_PKT_${PKTSIZE}"
./netperf-wrapper -l ${DURATION}${ONLOADTYPE}  -c ${ECHO_SERVER} -P ${PKTSIZE} -H asiago -C burrata -T ${target} udp_localhost_stream -t "${title}" -o "${result}" -L "${log}" | tee ${tmpfile}

outfile=`cat ${tmpfile} | grep  'Test data is in ' | cut -d'[' -f2 | cut -d']' -f1`
./netperf-wrapper -i ${outfile} -p ${PLOTTYPE} -o ${plotpath} | tee -a ${FINALRESULT}

}

run_udp_stream_increasing_pkt_size()
{
    SELECTED_T=${1}
    SELECTED_TT=${2}
    OUTDIR=${3}

mkdir -p ${OUTDIR}

PKTSIZE=1024
run_npf_w_stream ${SELECTED_T} ${SELECTED_TT} ${OUTDIR}
sleep 5

PKTSIZE=8000
run_npf_w_stream ${SELECTED_T} ${SELECTED_TT} ${OUTDIR}
sleep 5


PKTSIZE=16000
run_npf_w_stream ${SELECTED_T} ${SELECTED_TT} ${OUTDIR}
sleep 5

PKTSIZE=32000
run_npf_w_stream ${SELECTED_T} ${SELECTED_TT} ${OUTDIR}
sleep 5

PKTSIZE=64000
run_npf_w_stream ${SELECTED_T} ${SELECTED_TT} ${OUTDIR}
sleep 5

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


INTEL_T="10.22.4.11"
INTEL_S_T="10.113.4.11"
SF_T="10.23.4.21"
SF_S_T="10.113.4.21"
PLOTTYPE="bbest"
PLOTTYPE="lbest"

SELTARGET=${INTEL_T}
SELTARGET_T="Intel"

SELTARGET=${SF_T}
SELTARGET_T="SF"

UDP_TEST_NAME="udp_rr1"
UDP_TEST_NAME="udp_latency"
UDP_TEST_NAME="udp_rr"

ONLOADTYPE="--onloadLatency"
ONLOADTEXT="onloadLatency"

ONLOADTYPE=""
ONLOADTEXT=""

ECHO_SERVER="HImplDpdk"
ECHO_SERVER="HImplOnload"
ECHO_SERVER="CImplOnload"
ECHO_SERVER="netserver"
ECHO_SERVER="netcat"
ECHO_SERVER="socat"
ECHO_SERVER="netcat_poll"
ECHO_SERVER="llvmDpdk"
ECHO_SERVER="CImplDpdk"
ECHO_SERVER="CImplOnload"
ECHO_SERVER="llvmE10k"
ECHO_SERVER="socat_pollL"
ECHO_SERVER="socat_poll"
ECHO_SERVER="socat_onload"
ECHO_SERVER="socat"
DURATION=10
ITERATIONS=5

JUMBO="JUMBO"
JUMBO=""

echo "Running BM for Echo server: ${ECHO_SERVER}, Target: [${SELTARGET} , ${SELTARGET_T}]"
echo "Onload: [${ONLOADTYPE}, ${ONLOADTEXT}], Jumbo: [${JUMBO}] "

OUTDIRPP="../myplots/resultsI/${SELTARGET_T}/"

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


get_best_tp 1024
exit 0

get_best_latency 64 1

######################################################################
######################################################################

get_best_latency 1400 1

#get_best_latency 64 1
#get_best_latency 1400 64
#exit 0

get_best_tp 1400
get_best_latency 1400 64
get_best_tp 8000
get_best_tp 32000
#get_best_tp 16000
#get_best_tp 1400
#get_best_tp 1024
#get_best_tp 64
exit 0

get_best_latency 64 1
get_best_latency 1024 1
get_best_latency 1400 1

exit 0

echo "Following is to test if everything works"
get_best_latency 8000 1
#get_best_latency 16000 1
#get_best_latency 32000 1
#get_best_latency 64000 1
exit 0

######################################################################
######################################################################
######################################################################
######################################################################

#get_best_latency 8000 1
#get_best_latency 1400 1

OUTDIR="${OUTDIR}/TP_MAX/"

PKTSIZE=8000
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}

exit 0

PKTSIZE=32000
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}


PKTSIZE=63000
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}


PKTSIZE=64000
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}

mkdir -p ${OUTDIR}
PKTSIZE=64
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}

sleep 5
PKTSIZE=1024
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}
sleep 5

PKTSIZE=1400
# for increasing TP on transaction based benchmark
run_bm_tp_rr ${SF_T} "SF_T" ${OUTDIR}

exit 0

echo ${OUTDIRP}
get_best_latency 64 1
get_best_latency 1024 1
get_best_latency 1400 1
get_best_latency 8000 1
get_best_latency 16000 1
get_best_latency 32000 1
get_best_latency 64000 1


#run_npf_w_rr ${SF_S_T} "SF_S_T" ${OUTDIR}
#run_npf_w_rr ${INTEL_S_T} "INTEL_S_T" ${OUTDIR}

OUTDIR="${OUTDIRP}/UDP_STREAM_INCREASE/"
mkdir -p ${OUTDIR}

run_udp_stream_increasing_pkt_size ${SF_T} "SF_T" "${OUTDIR}"



