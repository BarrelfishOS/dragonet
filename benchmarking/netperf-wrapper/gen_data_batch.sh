#!/bin/bash

set -x
set -e

run_npf_w_rr()
{
target=${1}
target_t=${2}
result="${3}/latency_${target_t}.png"
log="${3}/latency_${target_t}.log"

./netperf-wrapper -l 1 -H asiago -C burrata -T ${target} udp_rr -t "${target_t}" -P 64 -b 1 -p  ${DEFAULT_PLOT} -o "${result}" -L "${log}"
}

OUTDIR="../myplots/r_latency2/"
INTEL_T="10.22.4.11"
INTEL_S_T="10.113.4.11"
SF_T="10.23.4.21"
SF_S_T="10.113.4.21"
DEFAULT_PLOT="userlatency2"

mkdir -p ${OUTDIR}

# echo latency numbers
run_npf_w_rr ${INTEL_T} "INTEL_T" ${OUTDIR}
run_npf_w_rr ${SF_T} "SF_T" ${OUTDIR}
#run_npf_w_rr ${SF_S_T} "SF_S_T" ${OUTDIR}
#run_npf_w_rr ${INTEL_S_T} "INTEL_S_T" ${OUTDIR}

