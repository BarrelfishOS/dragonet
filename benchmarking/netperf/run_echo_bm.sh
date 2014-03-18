#!/bin/bash

run_server() {
    echo "Running sink on port ${ATTACKPORT}"
    # Note: I normally execute this as root, and not with sudo
    #sudo onload --profile=latency --preload=/usr/lib64/libonload.so socat PIPE UDP-LISTEN:7,fork
    sudo socat PIPE UDP-LISTEN:7,fork
    #sudo nc.traditional -vvlup 7 -e /bin/cat
}

run_netperf() {
#    echo "Running netperf for machine [${MACHINE}:${ATTACKPORT}] for ${TIME} seconds"
    #OUTFORMAT1="LOCAL_BYTES_SENT,LOCAL_BYTES_RECVD"
    OUTFORMAT="RESULT_BRAND,DEST_ADDR,REQUEST_SIZE,BURST_SIZE,TRANSACTION_RATE,RT_LATENCY,THROUGHPUT,THROUGHPUT_UNITS"
    netperf -B ${MSG} -P 1 -N -H ${MACHINE} -4 -t UDP_RR  -l ${TIME} -v 2 -f m -- -r ${PSIZE} -b ${BRUST} "-${OUTPUTTYPE}" "${OUTFORMAT}"
}

show_usage() {
        echo "Please specify the machine name"
        echo "Usage: ${0} -m <machineName> --> ip of server to test"
        echo "           -s/-g -->  run as server/generator"
        #echo "           -a -->  attack port"
        echo "           -t -->  no. seconds to run the test"
        echo "           -M -->  Message to prepend in output"
        echo "           -f -->  output unit k/m/g"
        echo "           -o -->  output format (k/o/O)"
        echo "Examples: ${0} -g -m 127.0.0.1 -t 5"
        echo "Examples: ${0} -g -m 192.168.123.1 -t 5"
        echo "Examples: ${0} -g -m 10.113.4.71 -M CImpl-SF -O"
        echo "Examples: ${0} -s"
        exit 1
}

MACHINE="192.168.123.1"
MACHINE="127.0.0.1"
ATTACKPORT=7
TIME=5
BRUST=50
PSIZE=1400
MSG="RUN"
OUTPUTTYPE="o"
FTYPE="m"

while getopts ":m:M:t:b:p:f:sgo:" opt; do
  case $opt in
    m)
        MACHINE="$OPTARG"
      ;;
    M)
        MSG="$OPTARG"
      ;;
    t)
        TIME="$OPTARG"
      ;;
    b)
        BRUST="$OPTARG"
      ;;
    p)
        PSIZE="$OPTARG"
      ;;
    s)
        SERVER="yes"
      ;;
    o)
        OUTPUTTYPE="$OPTARG"
      ;;
    g)
        GENERTOR="yes"
      ;;
    f)
        FTYPE="$OPTARG"
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

if [ "${SERVER}" == "yes" ] ; then
    run_server
elif [ "${GENERTOR}" == "yes" ] ; then
    run_netperf
fi

