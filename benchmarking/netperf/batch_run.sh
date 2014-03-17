#!/bin/bash

run_bm() {
    THOST=${1}
    MSG=${2}
    set -x
    set -e
    ./run_echo_bm.sh -g -M ${MSG} -m ${THOST} -t ${RTIME} -p ${PKTSIZE} -b ${BRUST} | tee -a ${OUTFILE}
    set +x
    set +e
    sleep 3
}

run_haskell()
{
#    bash -c "cd ../../Dragonet/ ; sudo ./tuntap" &
    run_bm "192.168.123.1" "HImpl"
#    sudo killall tuntap
}

run_others()
{
#    bash -c "cd ../../Dragonet/c_impl ; sudo ./tuntap" &
    run_bm "192.168.123.1" "CImpl"
#    sudo killall tuntap
    run_bm "localhost" "LinuxLO"
    run_bm "base-station.ethz.ch" "LinuxLAN"
}

# latency run
PKTSIZE=10
BRUST=1

# Throughput run
PKTSIZE=1400
BRUST=50

RTIME=5
OUTFILE="output.txt"
RESULTFILE="result.txt"
echo > ${OUTFILE}

#run_haskell
run_others

cat ${OUTFILE}  | grep -v "MIGRATED"  | tr , \| | sed 's\$\|\' | sed 's\^\|\'  | grep "Result Tag" | head -n1 >> ${RESULTFILE}
cat ${OUTFILE}  | grep -v "MIGRATED"  | tr , \| | sed 's\$\|\' | sed 's\^\|\'  | grep -v "Result Tag" >> ${RESULTFILE}
cat ${RESULTFILE}

