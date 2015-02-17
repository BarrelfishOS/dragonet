#!/bin/bash

show_usage() {
    echo "USAGE: $0 <runtype> <stackname> <HWqueues> <cost-fn> <oracle> <concurrency> <clients>"
    echo "EXAMPLE: $0 fg tap 1 balanced greedy 0 0"
    echo "EXAMPLE: $0 bg sf 4 priority greedy 0 0"
    echo "EXAMPLE: $0 strace e10k 10 priority greedy 0 0"
    echo "EXAMPLE: $0 gdb null 5 balanced greedy 0 0"
    echo "EXAMPLE: $0 fg dpdk 10 priority greedy 0 0"
    echo "EXAMPLE: $0 bg dpdk 10 priority hardcoded 4 40"
}

if [ -z $1 ] ; then
    echo "ERROR: provide the runtype"
    show_usage
    exit 1
fi

RUNTYPE="$1"

shift

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    show_usage
    exit 1
fi

STACKNAME=stack-"${1}"

shift





if [ "$1" == "" -o "$2" == "" ]; then
    echo "ERROR: provide number of HW queues and cost function"
    show_usage
    exit 1
fi
HWQUEUES=${1}
COSTFN=${2}


# check for arguments related to hardcoded oracle
if [ -z $3 ] ; then
    # FIXME: for backward comptability with older deployment code in netperf-wrapper.  Should be removed
    #       assuming greedy when nothing is specified
    echo "WARNING: No oracle type given, assuming greedy"
    ORACLE="greedy"
    CONC=0
    CLIENTS=0
else

    ORACLE=${3}

    # Make sure you have all the needed arguments when oracle type is provided
    if [ "$4" == "" -o "$5" == "" ]; then
        echo "ERROR: incorrect no. of arguments for hardcoded oracle"
        show_usage
        exit 1
    else
    CONC=${4}
    CLIENTS=${5}
    fi
fi


SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

EXTRAENV=""
INITCMD=""
if [ ${STACKNAME} == "stack-dpdk" ] ; then
echo "Using dpdk library"
#EXTRAENV="LD_PRELOAD=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/libintel_dpdk.so"  # this does not work, causes seg-fault
INITCMD="export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.7.1/build/lib/ "
fi

RUNCMD="./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} ${COSTFN}"
RUNCMD="./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} ${COSTFN} ${ORACLE} ${CONC} ${CLIENTS}"

if [ ${STACKNAME} == "stack-dpdk2" ] ; then
echo "Using dpdk library"
INITCMD="export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.7.1/build/lib/ "
RUNCMD="./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} ${COSTFN} dpdk ${CONC} -i "
#RUNCMD="./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} ${COSTFN} dpdk  ${CONC} "
fi

if [ ${STACKNAME} == "stack-sf" ] ; then
#sudo LD_PRELOAD=/lib/libciul.so.1.1.1 ./dist/build/${APPNAME}/${APPNAME} $@
EXTRAENV="LD_PRELOAD=/lib/libciul.so.1.1.1"
RUNCMD="./dist/build/${STACKNAME}/${STACKNAME} ${HWQUEUES} ${COSTFN} sf ${CONC} -i "
fi

echo "Using commandline $RUNCMD"
case ${RUNTYPE} in
    'bg')
        echo "running in background"
        #nohup ${INITCMD} ; sudo ${EXTRAENV} ${RUNCMD} > some.log 2>&1 < /dev/null  &
        nohup sudo ${EXTRAENV} ${RUNCMD} > some.log 2>&1 < /dev/null  &
        echo "Waiting for Dragonet to start"
        sleep 4
        ${SCRIPTDIR}/wait_for_dragonet.sh ${STACKNAME} ${HWQUEUES}
        sleep 10
        echo "Dragonet is ready"
        cat some.log
        exit 0
        ;;

    'fgg')
        echo "running in foreground"
        ${INITCMD} ; sudo ${EXTRAENV} ${RUNCMD} | tee some.log
        ;;

    'fg')
        echo "running in foreground"
        ${INITCMD} ; sudo ${EXTRAENV} ${RUNCMD}
        ;;

    'strace')
        echo "running with strace"
        ${INITCMD} ; sudo strace -fCrtT ${EXTRAENV} ${RUNCMD}
        ;;

    'gdb')
        echo "running with gdb"
        echo "cmdline: $RUNCMD"
        ${INITCMD} ; sudo ${EXTRAENV} gdb  ${RUNCMD}
        ;;

       *)
        echo "ERROR: unknown runtype: $RUNTYPE ...!!!"
        echo "We currently only support: bg, fg, strace, gdb"
        show_usage
        exit 1
        ;;
esac
exit 0

