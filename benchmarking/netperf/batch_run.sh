#!/bin/bash

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

if [ "${SILENTHEADER}" == "no" ] ; then
    cat ${OUTFILE}  | grep -v "MIGRATED"  | tr , \| | sed 's\$\|\' | sed 's\^\|\'  | grep "Result Tag" | head -n1 >> ${RESULTFILE}
fi
    cat ${OUTFILE}  | grep -v "MIGRATED"  | tr , \| | sed 's\$\|\' | sed 's\^\|\'  | grep -v "Result Tag" >> ${RESULTFILE}
}


# latency run
BMTYPE="LP"
PKTSIZE=10
BRUST=1

# Throughput run
BMTYPE="TP"
PKTSIZE=1400
BRUST=50

RTIME=5
OUTFILE="output.txt"
RESULTFILE="result.txt"
SILENTHEADER="no"

LOOPBACK="no"
TUNTAP="no"
SF="no"
DPDK="no"
LAN="no"
MSG=""

NETSTACKTYPE="Cimpl"


show_usage() {
    echo "Usage: ${0} -T -L -S -D -I (tuntap, loopback, sf, dpdk, lan)"
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

while getopts ":M:r:n:sltLSDIT" opt; do
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
        BRUST=50
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

