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

show_usage() {
        echo "Usage: ${0} -i <interfaceName> --> tuntap, sf, dpdk, lo, lan"
        echo "           -M -->  Message to add into result"
        echo "           -p <filename> -->  process and write results in file"
        echo "Examples: ${0} -g -m 127.0.0.1 -t 5"
        echo "Examples: ${0} -g -m 192.168.123.1 -t 5"
        echo "Examples: ${0} -s"
        exit 1
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
SF="no"
DPDK="no"
LAN="no"
MSG=""


while getopts ":M:r:sltLSDI" opt; do
  case $opt in
    M)
        MSG="$OPTARG"
      ;;
    r)
        PROCESSRESULT="yes"
        RESULTFILE="$OPTARG"
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


if [ "${LOOPBACK}" == "yes" ] ; then
    NMSG="${MSG}-${BMTYPE}-LinuxLO"
    run_bm "localhost" "${NMSG}"
fi

if [ "${SF}" == "yes" ] ; then
    NMSG="${MSG}-${BMTYPE}-SF"
    run_bm "10.113.4.71" "${NMSG}"
fi

if [ "${LAN}" == "yes" ] ; then
    NMSG="${MSG}-${BMTYPE}-LAN"
    run_bm "base-station.ethz.ch" "${NMSG}"
fi

if [ "${DPDK}" == "yes" ] ; then
    NMSG="${MSG}-${BMTYPE}-DPDK"
    echo "NOT YET SUPPORTED"
    exit 1
#    run_bm "10.113.4.71" "CImpl-SF"
fi


if [ "${PROCESSRESULT}" == "yes" ] ; then
    process_results
fi

cat ${RESULTFILE}

