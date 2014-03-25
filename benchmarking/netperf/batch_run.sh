#!/bin/bash

run_bm_tp() {
    THOST=${1}
    MYMSG=${2}
    if [ ${MYMSG} == "" ] ; then
        MYMSG="${THOST}-${BRUST}"
    fi
    TMPFILE=`mktemp`
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


run_bm_ping() {
    THOST=${1}
    MYMSG=${2}
    if [ ${MYMSG} == "" ] ; then
        MYMSG="${THOST}-${BRUST}"
    fi
    set -x
    set -e
    ping -c 10 ${THOST} | tee -a ${OUTFILE}
    set +x
    set +e
    sleep 3
}

run_bm_echo() {
    THOST=${1}
    MYMSG=${2}
    if [ ${MYMSG} == "" ] ; then
        MYMSG="${THOST}-${BRUST}"
    fi
    TMPOUTFILE=`mktemp`
    set -x
    set -e
    nc.traditional -n -vv -i 1 -q 1 -u ${THOST} ${SERVERPORT} < ${INFILE} &>  ${TMPOUTFILE}
    cat ${TMPOUTFILE} | tee -a ${OUTFILE}
    diff ${INFILE} ${TMPOUTFILE} | tee -a ${OUTFILE}
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

NTMPFILE=`mktemp`
#cat ${OUTFILE} | grep -v "MIGRATED" | grep "TRANSACTION_RATE"  | grep "RT_LATENCY" | tr , \| | sed 's\$\|\' | sed 's\^\|\' > ${NTMPFILE}
cat ${OUTFILE} | grep -v "MIGRATED" |  grep -v "=" | grep -v "TPITERATOR" | tr , \| | sed 's\$\|\' | sed 's\^\|\' > ${NTMPFILE}
if [ "${SILENTHEADER}" == "no" ] ; then
    cat ${NTMPFILE} | grep "Result Tag" | head -n1 >> ${RESULTFILE}
fi
cat ${NTMPFILE} | grep -v "Result Tag" >> ${RESULTFILE}


    if [ "${BMTYPE}" == "TP" ] ; then
        echo "Incremental Throughput measurement"
        cat ${OUTFILE} | grep "TPITERATOR" | cut -d: -f2  | tr , \| | sed 's\$\|\' | sed 's\^\|\' >> ${RESULTFILE}
    fi

}


run_bm_generic() {
    if [ "${BMTYPE}" == "TPLONG" ] ; then
        run_bm ${1} ${2}
    elif [ "${BMTYPE}" == "TP" ] ; then
        run_bm_tp ${1} ${2}
    elif [ "${BMTYPE}" == "LP" ] ; then
        run_bm ${1} ${2}
    elif [ "${BMTYPE}" == "PING" ] ; then
        run_bm_ping ${1} ${2}
    elif [ "${BMTYPE}" == "ECHO" ] ; then
        run_bm_echo ${1} ${2}
    fi
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
OUTFILE=`mktemp --tmpdir=./ outputXXXXXX`
RESULTFILE=`mktemp --tmpdir=./ resultXXXXXX`
SILENTHEADER="no"

LOOPBACK="no"
TUNTAP="no"
SF="no"
SFDirect="no"
DPDK="no"
LAN="no"
MSG=""

NETSTACKTYPE="Cimpl"
SERVERPORT=7




show_usage() {
    echo "Usage: ${0} -T -L -S -d -D -I (tuntap, loopback, sf, sf-direct, dpdk, lan)"
    echo "      -n <Net-stack-Type> -->  Type of network stack (Cimpl, Haskell,)"
    echo "      -M -->  MSG to prepend to test name (eg: R2)"
    echo "      -r <filename> -->  results file, where results will be added"
    echo "      -s  -->  Do not add headers in results file"
    echo "      -l  -->  Do a latency benchmark"
    echo "      -t  -->  Do a incremental Throughput benchmark"
    echo "      -z  -->  Do a fixed and long Throughput benchmark"
    echo "      -p  -->  Do a ping benchmark"
    echo "      -e  -->  Do a echo benchmark"
    echo "Examples: ${0} -t -T -L -n 'CImpl' -r ./runh.results"
    echo "Examples: ${0} -t -T -n 'HImpl' -s -r ./runh.results"
    echo "Examples: ${0} -t -S -n 'CImpl' -r ./runc.results"
    echo "Examples: ${0} -t -S -n 'CImpl' -s -r  ./runc.results"
    echo "Examples: ${0} -p -D"
    echo "Examples: ${0} -e -D"
    exit 1
}

# for echo benchmark
INFILE="./inputfile.txt"
echo -e "hello\nworld\n1\n2\n3\n" > ${INFILE}

while getopts ":M:r:n:sltLSDITdpez" opt; do
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
    p)
        BMTYPE="PING"
      ;;
    z)
        PKTSIZE=1024
        RTIME=20
        BRUST=64
        BMTYPE="TPLONG"
      ;;
    e)
        BMTYPE="ECHO"
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

HOSTN=`hostname`


# Resetting putput.
echo > ${OUTFILE}

if [ "${TUNTAP}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-Tuntap"
    run_bm_generic "192.168.123.1" "${NMSG}"
fi


if [ "${LOOPBACK}" == "yes" ] ; then
    NMSG="${MSG}-Linux-${BMTYPE}-LO"
    run_bm_generic "localhost" "${NMSG}"
fi

if [ "${SF}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-SF"
    if [ "${HOSTN}" == "appenzeller" ] ; then
        DESTIPSF="10.110.4.38"
    elif [ "${HOSTN}" == "ziger1" ] ; then
        DESTIPSF="10.113.4.71"
    else
        echo "Running on unknown host, you may want to provide destination IP explicitly"
        exit 1
    fi
    run_bm_generic "${DESTIPSF}" "${NMSG}"
fi

if [ "${SFDirect}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-SFD"
    if [ "${HOSTN}" == "appenzeller" ] ; then
        DESTIPSF="10.22.4.37"
    elif [ "${HOSTN}" == "ziger1" ] ; then
        DESTIPSF="10.22.4.38"
    else
        echo "Running on unknown host, you may want to provide destination IP explicitly"
        exit 1
    fi
    run_bm_generic "${DESTIPSF}" "${NMSG}"
fi


if [ "${LAN}" == "yes" ] ; then
    NMSG="${MSG}-Linux-${BMTYPE}-LAN"
    run_bm_generic "base-station.ethz.ch" "${NMSG}"
fi

if [ "${DPDK}" == "yes" ] ; then
    NMSG="${MSG}-${NETSTACKTYPE}-${BMTYPE}-DPDK"
    if [ "${HOSTN}" == "appenzeller" ] ; then
        DESTIPSF="10.22.4.37"
    elif [ "${HOSTN}" == "ziger1" ] ; then
        DESTIPSF="10.22.4.38"
    else
        echo "Running on unknown host, you may want to provide destination IP explicitly"
        exit 1
    fi
    run_bm_generic "${DESTIPSF}" "${NMSG}"
fi

if [ "${PROCESSRESULT}" == "yes" ] ; then
    process_results
    cat ${RESULTFILE}
fi

echo "OUTPUT FILE --> ${OUTFILE}"
echo "Result FILE --> ${RESULTFILE}"
