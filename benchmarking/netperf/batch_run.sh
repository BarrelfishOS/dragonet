#!/bin/bash

run_bm_tp() {
    THOST=${1}
    MYMSG=${2}
    if [ ${MYMSG} == "" ] ; then
        MYMSG="${THOST}-${BRUST}"
    fi
    TMPFILE="./tmpout.txt"
    SAMARRYFILE="./tmpsammary.txt"
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
        ./run_echo_bm.sh -g -M ${MYMSG} -m ${THOST} -t ${RTIME} -p ${PKTSIZE} -b ${CBRUST} -o k | tee ${TMPFILE}

        let CTP=NTP

        NBWD=`cat ${TMPFILE} | grep "THROUGHPUT=" | cut -d= -f2` # | cut -d. -f1`
        NLATENCY=`cat ${TMPFILE} | grep "RT_LATENCY=" | cut -d= -f2` # | cut -d. -f1`
        NTP=`cat ${TMPFILE} | grep "TRANSACTION_RATE=" | cut -d= -f2 | cut -d. -f1`
        echo "TPITERATOR: ${MYMSG}, ${THOST}, ${PKTSIZE}, ${CBRUST}, ${NTP}, ${NLATENCY}, ${NBWD}" >> ${SAMARRYFILE}
        echo "TP of $NTP for $CBRUST"

        cat ${TMPFILE} >> ${OUTFILE}
        rm -f ${TMPFILE}
        sleep 5
    done

    set -x
    set -e
    ./run_echo_bm.sh -g -M ${MYMSG} -m ${THOST} -t ${RTIME} -p ${PKTSIZE} -b ${LBRUST} | tee -a ${OUTFILE}
    set +x
    set +e
    cat ${SAMARRYFILE} >> ${OUTFILE}
    rm -f ${SAMARRYFILE}
    sleep 3
    echo "while [  $NTP -gt $CTP ]; do"
}



run_bm() {
    THOST=${1}
    MYMSG=${2}
    if [ ${MYMSG} == "" ] ; then
        MYMSG="${THOST}-${BRUST}"
    fi
    set -x
    set -e
    ./run_echo_bm.sh -g -M ${MYMSG} -m ${THOST} -t ${RTIME} -p ${PKTSIZE} -b ${BRUST} | tee -a ${OUTFILE}
    set +x
    set +e
    sleep 3
}

run_haskell_SF()
{
    run_bm "10.113.4.71" "HImpl-SF"
}

run_cimpl_SF()
{
    run_bm "10.113.4.71" "CImpl-SF"
}



run_haskell()
{
#    bash -c "cd ../../Dragonet/ ; sudo ./tuntap" &
    run_bm "192.168.123.1" "HImpl-TT"
    run_bm "10.113.4.71" "CImpl-SF"
#    sudo killall tuntap
}

run_others()
{
#    bash -c "cd ../../Dragonet/c_impl ; sudo ./tuntap" &
    run_bm "192.168.123.1" "CImpl-TT"
    run_bm "10.113.4.71" "CImpl-SF"
#    sudo killall tuntap
    run_bm "localhost" "LinuxLO"
    run_bm "base-station.ethz.ch" "LinuxLAN"
}

process_results () {

NTMPFILE="./tmpfile2"
cat ${OUTFILE} | grep -v "MIGRATED" | grep "TRANSACTION_RATE"  | grep "RT_LATENCY" | tr , \| | sed 's\$\|\' | sed 's\^\|\' > ${NTMPFILE}
if [ "${SILENTHEADER}" == "no" ] ; then
    cat ${NTMPFILE} | grep "Result Tag" | head -n1 >> ${RESULTFILE}
fi
cat ${NTMPFILE} | grep -v "Result Tag" | head -n1 >> ${RESULTFILE}

echo "Incremental Throughput measurement"
cat ${OUTFILE} | grep "TPITERATOR" | cut -d: -f2  | tr , \| | sed 's\$\|\' | sed 's\^\|\' >> ${RESULTFILE}

}


# latency run
BMTYPE="LP"
PKTSIZE=10
BRUST=1

# Throughput run
BMTYPE="TP"
PKTSIZE=1400
BRUST=10

RTIME=5
OUTFILE="output.txt"
RESULTFILE="result.txt"
SILENTHEADER="no"

LOOPBACK="no"
TUNTAP="no"
SF="no"
SFDirect="no"
DPDK="no"
LAN="no"
MSG=""

NETSTACKTYPE="Cimpl"


show_usage() {
    echo "Usage: ${0} -T -L -S -d -D -I (tuntap, loopback, sf, sf-direct, dpdk, lan)"
    echo "      -n <Net-stack-Type> -->  Type of network stack (Cimpl, Haskell,)"
    echo "      -M -->  MSG to prepend to test name (eg: R2)"
    echo "      -r <filename> -->  results file, where results will be added"
    echo "      -s  -->  Do not add headers in results file"
    echo "      -l  -->  Do a latency benchmark"
    echo "      -t  -->  Do a Throughput benchmark"
    echo "Examples: ${0} -t -T -L -n 'CImpl' -r ./runh.results"
    echo "Examples: ${0} -t -T -n 'HImpl' -s -r ./runh.results"
    echo "Examples: ${0} -t -S -n 'CImpl' -r ./runc.results"
    echo "Examples: ${0} -t -S -n 'CImpl' -s -r  ./runc.results"
    exit 1
}

while getopts ":M:r:n:sltLSDITd" opt; do
  case $opt in
    M)
        MSG="$OPTARG"
      ;;
    r)
        PROCESSRESULT="yes"
        RESULTFILE="$OPTARG"
      ;;
    n)
        NETSTACKTYPE="$OPTARG"
      ;;
    s)
        SILENTHEADER="no"
      ;;
    l)
        BMTYPE="LP"
        PKTSIZE=10
        BRUST=1
      ;;
    t)
        BMTYPE="TP"
        PKTSIZE=1400
        BRUST=10
      ;;
    L)
        LOOPBACK="yes"
      ;;

    T)
        TUNTAP="yes"
      ;;
    S)
        SF="yes"
      ;;
    d)
        SFDirect="yes"
      ;;
    D)
        DPDK="yes"
      ;;
    I)
        LAN="yes"
      ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        show_usage
        exit 1
        ;;
    :)
        echo "Option -$OPTARG requires an argument." >&2
        show_usage
        exit 1
        ;;
  esac
done


# Resetting putput.
echo > ${OUTFILE}

if [ "${TUNTAP}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-Tuntap"
    run_bm "192.168.123.1" "${NMSG}"
fi


if [ "${LOOPBACK}" == "yes" ] ; then
    NMSG="${MSG}-Linux-${BMTYPE}-LO"
    run_bm "localhost" "${NMSG}"
fi

if [ "${SF}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-SF"
    run_bm "10.113.4.71" "${NMSG}"
fi

if [ "${SFDirect}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-SFD"
    if [ "${BMTYPE}" == "TP" ] ; then
        run_bm_tp "10.22.4.38" "${NMSG}"
    else
        run_bm "10.22.4.38" "${NMSG}"
    fi
fi


if [ "${LAN}" == "yes" ] ; then
    NMSG="${MSG}-Linux-${BMTYPE}-LAN"
    run_bm "base-station.ethz.ch" "${NMSG}"
fi

if [ "${DPDK}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-DPDK"
    echo "NOT YET SUPPORTED"
    exit 1
#    run_bm "10.113.4.71" "CImpl-SF"
fi

if [ "${PROCESSRESULT}" == "yes" ] ; then
    process_results
fi

cat ${RESULTFILE}

