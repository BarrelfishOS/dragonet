#!/bin/bash

run_server() {
    echo "Running sink on port ${ATTACKPORT}"
    sudo socat PIPE UDP-LISTEN:7,fork
    #sudo nc.traditional -vvlup 7 -e /bin/cat
}

run_netperf() {
    echo "Running netperf for machine [${MACHINE}:${ATTACKPORT}] for ${TIME} seconds"
    netperf -N -H ${MACHINE} -4 -t UDP_RR  -l ${TIME} -- -r ${PSIZE} -b ${BRUST}
}

show_usage() {
        echo "Please specify the machine name"
        echo "Usage: ${0} -m <machineName> --> ip of server to test"
        echo "           -s/-g -->  run as server/generator"
        #echo "           -a -->  attack port"
        echo "           -t -->  no. seconds to run the test"

        echo "Examples: ${0} -g -m 127.0.0.1 -t 5"
        echo "Examples: ${0} -g -m 192.168.123.1 -t 5"
        echo "Examples: ${0} -s"
        exit 1
}

MACHINE="192.168.123.1"
MACHINE="127.0.0.1"
ATTACKPORT=7
TIME=5
BRUST=50
PSIZE=1400

while getopts ":m:t:b:p:sg" opt; do
  case $opt in
    m)
        MACHINE="$OPTARG"
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
    g)
        GENERTOR="yes"
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
