#!/bin/bash

OUTPUTDIRPARENT="./dpdk_test_deleteme/"
OUTPUTDIRPARENT="./dpdk_test_memcached_test_10Q/"
OUTPUTDIRPARENT="./dpdk_test_memcached_test1_5Q/"
OUTPUTDIRPARENT="./output_runs_balance/"
OUTPUTDIRPARENT="./output_runs_deletemev2/"
OUTPUTDIRPARENT="./output_runs_posterV3/"
OUTPUTDIRPARENT="./output_runs_8cores/"
OUTPUTDIRPARENT="./output_runs_linuxTest/"
OUTPUTDIRPARENT="./output_runs_linuxTest_deleteme/"

# For debugging of dynamic connection detection
OUTPUTDIRPARENT="./output_runs_dynamic_debug/"
OUTPUTDIRPARENT="./output_runs_dynamic_debug3/"

OUTPUTDIRPARENT="./output_runs_mixed_static/"
OUTPUTDIRPARENT="./output_runs_mixed_static2/"
OUTPUTDIRPARENT="./output_runs_mixed_static_priority/"

OUTPUTDIRPARENT="./output_runs_mixed_static_priority_test/"
OUTPUTDIRPARENT="./output_runs_mixed_static_balance_test/"

OUTPUTDIRPARENT="./deleteme_output_runs_mixed_static_balance/"
OUTPUTDIRPARENT="./deleteme_output_runs_mixed_static_priority5/"
OUTPUTDIRPARENT="./deleteme_output_runs_mixed_static_priority6/"

OUTPUTDIRPARENT="./deleteme_output_runs_mixed_static_priority7/"
OUTPUTDIRPARENT="./deleteme_output_runs_mixed_static_balance7/"

# Testing with disjoint client sets
OUTPUTDIRPARENT="./deleteme_disjoint_clients_balance/"
OUTPUTDIRPARENT="./deleteme_disjoint_clients_priority4/"

# Fixed the issue of using old variables
OUTPUTDIRPARENT="./deleteme_disjoint_clients_priority5/"

# Aparantly there is still issue of gap in performance of two clients

OUTPUTDIRPARENT="./deleteme_disjoint_clients_priority6/"

OUTPUTDIRPARENT="./output_somewhat_working_priority/"
OUTPUTDIRPARENT="./output_somewhat_working_balance_v4/"
OUTPUTDIRPARENT="./output_somewhat_working_priority_v2/"
OUTPUTDIRPARENT="./output_somewhat_working_balance_30clients/"

OUTPUTDIRPARENT="./output_somewhat_working_priority_big3/"
OUTPUTDIRPARENT="./output_somewhat_working_balance_big3/"
OUTPUTDIRPARENT="./output_debugging_softfiltering_2/"

# First results which worked for online setup
OUTPUTDIRPARENT="./output_debugging_online/"
OUTPUTDIRPARENT="./output_debugging_online2/"

OUTPUTDIRPARENT="./output_debugging_online_automated/"
OUTPUTDIRPARENT="./output_debugging_online_automated_v2/"
OUTPUTDIRPARENT="./output_debugging_online_automated_v4/"
OUTPUTDIRPARENT="./output_debugging_online_automated_v5/"
OUTPUTDIRPARENT="./output_debugging_online_automated_v6/"



OUTPUTDIRPARENT="./output_debugging_online_automated_10Q/"

OUTPUTDIRPARENT="./output_debugging_online_automated_v7/"  # keep this one

OUTPUTDIRPARENT="./output_debugging_online_20Clients_10Q_5Cores/"
OUTPUTDIRPARENT="./output_debugging_online_20Clients_10Q_5Cores_v2/"

OUTPUTDIRPARENT="./output_debugging_online_20Clients_32F_10Q/"
OUTPUTDIRPARENT="./output_debugging_online_20Clients_32F_10Q_s/"

OUTPUTDIRPARENT="./output_debugging_online_40Clients_16F_10Q_s/"

OUTPUTDIRPARENT="./output_debugging_online_40Clients_16F_10Q_allLong/"

OUTPUTDIRPARENT="./output_debugging_online_20Clients_32F_10Q_4P_t1/"

OUTPUTDIRPARENT="./output_debugging_online_20Clients_16F_10Q_4P_t1/"

OUTPUTDIRPARENT="./allResults/20Clients_16F_10Q_4P_long/"

OUTPUTDIRPARENT="./allResults/20Clients_16F_5Q_2P/"

OUTPUTDIRPARENT="./allResults/20Clients_16F_10Q_4P_priority/"

WORKLOADTYPE="priority"
WORKLOADTYPE="priBal"
WORKLOADTYPE="linux"
WORKLOADTYPE="balance"
WORKLOADTYPE="priBal"
WORKLOADTYPE="priSmall"
WORKLOADTYPE="online"



startStack() {
#./cleanupServer.sh

initConcurrency=${LOAD}

title="STARTSTACK_${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${initConcurrency},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/STARTSTACK/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${initConcurrency}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift ${SRVCORESHIFT} ${ClientList} --servercores ${SRVCORES} \
 --serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES}  --packet ${PACKETSIZE} --concurrency ${initConcurrency} \
-I 1 -l 5 -H ${SERVERNAME} -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} \
 --totalClients ${CLIENTCOUNT}  --clientPortShift ${CLIENTPORTSHIFT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"


if [ "${UDP_TEST_NAME}" == "udp_rr" ];
then
    grep -e  'Runner: netperf' -e ' TRANSACTION_RATE='  -e ' THROUGHPUT=' "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

fi
if [ "${UDP_TEST_NAME}" == "memcached_rr" ];
then
    grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

fi

#exit 0
}

doShortRun() {


local title="SHORT_${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
local OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/SHORT/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift ${SRVCORESHIFT}  ${ClientList} --servercores ${SRVCORES} \
 --serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES} --packet ${PACKETSIZE} --concurrency ${LOAD} \
-I 1 -l ${DUMMYRT} -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} \
 --totalClients ${CLIENTCOUNT}  --clientPortShift ${CLIENTPORTSHIFT} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

if [ "${UDP_TEST_NAME}" == "udp_rr" ];
then
    grep -e  'Runner: netperf' -e ' TRANSACTION_RATE='  -e ' THROUGHPUT=' "${OUTDIR}/${title}.runlog"  2>&1 | tee "${OUTDIR}/${title}.output"

fi
if [ "${UDP_TEST_NAME}" == "memcached_rr" ];
then
    grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

fi


#exit 0
}


getData() {
local prefix=$1
local startReportFile="$2"
local startReportFileOption=""
if [ ! "${startReportFile}" == "" ] ;
then
startReportFileOption="--report-start-run  ${startReportFile}"
    echo "Report start time enabled enabled: ${startReportFileOption}"
else
    echo "Report start time not enabled"
fi

local title="${prefix}_${WORKLOADTYPE}_Test_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
local OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/${prefix}/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

REPEAT=1
./netperf-wrapper -d 0 --udp --serverCoreShift ${SRVCORESHIFT}  ${ClientList} --servercores ${SRVCORES} \
 --serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES} --packet ${PACKETSIZE} --concurrency ${LOAD} \
-I ${REPEAT} -l ${REALRT} -H ${SERVERNAME} -T ${SERVERIP} -c noServer ${UDP_TEST_NAME} \
 --totalClients ${CLIENTCOUNT}  --clientPortShift ${CLIENTPORTSHIFT}   ${startReportFileOption} \
-t ${title} -o ${OUTDIR} -L  ${OUTDIR}/${title}.runlog
#-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

if [ "${UDP_TEST_NAME}" == "udp_rr" ];
then
    grep -e  'Runner: netperf' -e ' TRANSACTION_RATE='  -e ' THROUGHPUT=' "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

fi
if [ "${UDP_TEST_NAME}" == "memcached_rr" ];
then
    grep -e  'Runner: memaslap' -e 'Run time:'  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"

fi
}

######################
SERVERNAME="babybel2"
CLIENTCORES=1
######################


#ClientList="-C ziger1 -C ziger2 -C babybel3 -C gottardo -C appenzeller-e1000 -C sbrinz1 -C gruyere -C sbrinz2 "
ClientList="-C ziger1 -C sbrinz1 "
ClientList="-C appenzeller "
ClientList="-C ziger1 -C ziger2 -C sbrinz1 -C sbrinz2 -C gruyere -C appenzeller"

ClientList="-C ziger1 -C ziger2 -C gruyere"
ClientList="-C ziger1 -C ziger2"
ClientList="-C sbrinz1 -C sbrinz2"
ClientList="-C ziger1 -C sbrinz1"

ClientList="-C ziger2 -C sbrinz2"
ClientList="-C ziger2 "
ClientList="-C ziger1 -C sbrinz1 -C sbrinz2"
ClientList="-C ziger1 -C ziger2 -C sbrinz1 -C sbrinz2"

######################

NICTYPE="NIC_SF"
ECHO_SERVER="memcached_onload"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="memcached_linux"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_Intel"
ECHO_SERVER="dpdk2"
SERVERIP=10.113.4.95

######################

#SERVERNAME="sbrinz1"
#SERVERIP=10.113.4.26

######################
UDP_TEST_NAME="udp_rr"
SRVCORESHIFT=5

######################
UDP_TEST_NAME="memcached_rr"
SRVCORESHIFT=0

######################
PACKETSIZE=64
PACKETSIZE=1024

######################

LOAD=1
LOAD=4
LOAD=2
######################

HWQUEUE=8
HWQUEUE=5
HWQUEUE=10
######################

SRVCORES=8
# Giving one less as memcached will start one extra thread for queue-0
SRVCORES=5
SRVCORES=9
######################


CLIENTCOUNT=32
CLIENTCOUNT=2
CLIENTCOUNT=8
CLIENTCOUNT=20
CLIENTCOUNT=40
CLIENTCOUNT=4
CLIENTCOUNT=10

CLIENTPORTSHIFT=0

# Runtimes for dummy and real runs
DUMMYRT=10
REALRT=50

######################

set_echo_test() {
    SRVCORESHIFT=3
    #SRVCORES=7
    SRVCORES=7
    UDP_TEST_NAME="udp_rr"
    LOAD=64
    CLIENTCOUNT=10
    #ClientList="-C sbrinz1 -C sbrinz2 -C ziger1"
    ClientList="-C sbrinz1 -C sbrinz2"
}

set_memcached_test() {
    SRVCORESHIFT=0
    SRVCORES=2
    UDP_TEST_NAME="memcached_rr"
    LOAD=32
    CLIENTCOUNT=2
    ClientList="-C ziger1 -C ziger2"
    #ClientList="-C ziger2 "
}


show_usage() {
        echo "Please select what you want to run"
        echo "Usage: ${0} [-c -s -l]"
        echo "           -t E/M -->  Test type (E --> Echo server, M --> memcached)"
        echo "           -c <n> -->  client count (n clients)"
        echo "           -l <n> -->  load (n concurrent sessions per client)"
        echo "           -s <n> -->  n server cores/threads"
        echo "           -m S/D -->  Mixed run (sending extra HP and LP while big run is happening)"
        echo "                         with static or dynamic setup"
        echo "           -S     -->  start stack"
        echo "           -d     -->  dummy run"
        echo "           -r     -->  Real run"
        echo "           -b     -->  Run simultaneousely both memcached and echo workload"
        echo "           -x     -->  Cleanup involved machines"
        echo "           -X     -->  Delete output/log files"
        echo "           -h     -->  this help"
        echo "Examples (starting stack): ${0} -c 4 -s 4 -C 4 S"
        exit 1
}

if [ $# == 0 ] ;
then
    echo "ERROR: $0: No arguments given"
    show_usage
    exit 1
fi

OPTTEST="no"

while getopts ":c:l:s:SdhxXrbm:t:T" opt; do
  case $opt in
    t)
        echo "Setting up test $OPTARG"
        OPTTEST="yes"
        if [ "$OPTARG" == "E" ] ;
        then
            UDP_TEST_NAME="udp_rr"
            #set_echo_test
        elif [ "$OPTARG" == "M" ] ;
        then
            UDP_TEST_NAME="memcached_rr"
            #set_memcached_test
        else
            echo "Error: Invalid test name given"
            show_usage
            exit 1
        fi
      ;;
    c)
#        if [ "$OPTTEST" == "yes" ] ;
#        then
#            echo "ERROR: clientcount is hardcoded by -t option, so you can't change it!"
#            exit 1
#        fi
        echo "Setting client-count to $OPTARG (instead of default $CLIENTCOUNT)"
        CLIENTCOUNT=$OPTARG
      ;;
    l)
#        if [ "$OPTTEST" == "yes" ] ;
#        then
#            echo "ERROR: load is hardcoded by -t option, so you can't change it!"
#            exit 1
#        fi
        echo "Setting load to $OPTARG (instead of default $LOAD)"
        LOAD=$OPTARG
      ;;
    s)
 #       if [ "$OPTTEST" == "yes" ] ;
 #       then
 #           echo "ERROR: servercores are hardcoded by -t option, so you can't change it!"
 #           exit 1
 #       fi
        echo "Setting server-cores/threads to $OPTARG (instead of default $SRVCORES)"
        SRVCORES=$OPTARG
        HWQUEUE=${SRVCORES}
      ;;
    S)
        RUNSTACK="yes"
      ;;
    m)
        MIXEDRUN="yes"
        if [ "$OPTARG" == "S" ] ;
        then
            WORKLOADTYPE="MixedRunStatic"
        elif [ "$OPTARG" == "D" ] ;
        then
            WORKLOADTYPE="MixedRunDynamic"
        else
            echo "Error: Invalid mix name"
            show_usage
            exit 1
        fi
      ;;
    T)
        TOGETHER="yes"
      ;;
    d)
        RUNDUMMY="yes"
      ;;
    b)
        RUNBOTH="yes"
        echo "This is currently disabled!"
        exit 1
      ;;
    r)
        RUNREAL="yes"
      ;;
    x)
        CLEANUP="yes"
      ;;
    X)
        DELETEFILES="yes"
      ;;

    h)
        show_usage
        exit 0
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

if [ "${RUNSTACK}" == "yes" ] ; then
    if [ "${RUNBOTH}" == "yes" ] ; then
        echo "Starting stack and first server"
        set +x
        set -e

        WORKLOADTYPE="SRunSS"

        ######################
        set_memcached_test
        echo "Starting first server ${UDP_TEST_NAME} with stack"
        startStack 2>&1 | tee deleteme_memcached_stackstart_out.txt

        ######################
        set_echo_test
        echo "Starting second server ${UDP_TEST_NAME} without stack"
        startStack 2>&1 | tee deleteme_echo_stackstart_out.txt
        set +x
        set +e
    else

        if [ "$OPTTEST" == "no" ] ;
        then
            echo "ERROR: Test specific options are not set using '-t'"
            exit 1
        fi

        echo "Running only one type: ${UDP_TEST_NAME}"
        set -x
        set -e
        startStack
        set +x
        set +e
    fi
fi


if [ "${RUNDUMMY}" == "yes" ] ; then

    echo "Assuming that stack is already running, just running dummy for testing"

    if [ "${RUNBOTH}" == "yes" ] ; then
        set +x
        set -e

        WORKLOADTYPE="MixedRun"

        ######################

        set_memcached_test
        echo "Starting first server ${UDP_TEST_NAME} for dummyRun"
        doShortRun 2>&1 > deleteme_memcached_dummy_out.txt &
        ######################

        set_echo_test
        echo "Starting second server ${UDP_TEST_NAME} for dummyRun"
        doShortRun 2>&1 > deleteme_echo_dummy_out.txt &

        echo "waiting for benchmark to finish"
        wait
        echo "done with benchmarking"
        cat deleteme_echo_dummy_out.txt
        cat deleteme_memcached_dummy_out.txt

        set +x
        set +e
    else
        if [ "$OPTTEST" == "no" ] ;
        then
            echo "ERROR: Test specific options are not set using '-t'"
            exit 1
        fi


        echo "Running only one type: ${UDP_TEST_NAME}"
        set -x
        set -e
        WORKLOADTYPE="SingleRun"
        doShortRun
        set +x
        set +e
    fi
fi


if [ "${RUNREAL}" == "yes" ] ; then
    echo "Assuming that stack is already running, doing real run"
    if [ "${RUNBOTH}" == "yes" ] ; then
        set +x
        set -e

        WORKLOADTYPE="MixedRun"
        ######################

        set_memcached_test
        echo "Starting first server ${UDP_TEST_NAME} for realrun"
        getData "LONG" 2>&1 > deleteme_memcached_out.txt &

        ######################
        set_echo_test
        echo "Starting second server ${UDP_TEST_NAME} for realrun"
        getData "LONG" 2>&1 > deleteme_echo_out.txt &

        echo "waiting for benchmark to finish"
        wait
        echo "done with benchmarking"

        cat deleteme_echo_out.txt
        cat deleteme_memcached_out.txt

        set +x
        set +e
    else
        if [ "$OPTTEST" == "no" ] ;
        then
            echo "ERROR: Test specific options are not set using '-t'"
            exit 1
        fi


        echo "Running only one type: ${UDP_TEST_NAME}"
        WORKLOADTYPE="SingleRun"
        set -x
        set -e
        getData "LONG"
        set +x
        set +e
    fi

    set +x
    set +e
fi


if [ "${MIXEDRUN}" == "yes" ] ; then
    echo "Assuming that stack is already running, doing mixed run"

    #WORKLOADTYPE="MixedRun"
    REALRT=130
    echo "Starting big set of clients for the load for test ${UDP_TEST_NAME}"

    rm -f deleteme_longrun_startTime.txt
    getData "LONG" "./deleteme_longrun_startTime.txt"  2>&1 | tee deleteme_mixed_bigrun_out.txt &

    fcountTries=1

    while true;
    do
        if [ -f deleteme_longrun_startTime.txt ] ;
        then
        isStart=`cat deleteme_longrun_startTime.txt | grep ": Start run$" | wc -l`
        if [ "${isStart}" == 1 ] ;
        then
            echo "Bigrun has started running"
            break
        fi
        fi
        #echo "Still waiting for Bigrun to actually start running: ${fcountTries}. sleeping for a second..."
        sleep 1
        fcountTries=`expr 1 + ${fcountTries}`
    done

    ######################

    sleep 10
    echo "Running single LP client"
    # TODO: specify different port used by clients
    ClientList="-C ziger2 "
    CLIENTPORTSHIFT="3000"
    CLIENTCOUNT=1
    REALRT=50
    getData "SHORTLP" 2>&1 | tee deleteme_LP_shortrun_out.txt

    sleep 10

    echo "Running single HP client"
    # TODO: specify different port used by clients
    ClientList="-C ziger2 "
    CLIENTPORTSHIFT="-${LOAD}"
    CLIENTCOUNT=1
    REALRT=50
    getData "SHORTHP" 2>&1 | tee deleteme_HP_shortrun_out.txt

    echo "waiting for benchmark to finish"
    wait
    echo "done with benchmarking"
fi

if [ "${TOGETHER}" == "yes" ] ; then
    echo "Assuming that stack is already running, doing mixed run"

    WORKLOADTYPE="TOGETHER"
    REALRT=50
    echo "Starting big set of clients for the load for test ${UDP_TEST_NAME}"

    rm -f deleteme_longrun_startTime.txt
    getData "LONG" "./deleteme_longrun_startTime.txt"  2>&1 | tee deleteme_mixed_bigrun_out.txt &

    fcountTries=1

    while true;
    do
        if [ -f deleteme_longrun_startTime.txt ] ;
        then
        isStart=`cat deleteme_longrun_startTime.txt | grep ": Start run$" | wc -l`
        if [ "${isStart}" == 1 ] ;
        then
            echo "Bigrun has started running"
            break
        fi
        fi
        #echo "Still waiting for Bigrun to actually start running: ${fcountTries}. sleeping for a second..."
        sleep 1
        fcountTries=`expr 1 + ${fcountTries}`
    done

    ######################

    #sleep 10
    echo "Running single LP client"
    # TODO: specify different port used by clients
    ClientList="-C ziger1 "
    CLIENTPORTSHIFT="3000"
    CLIENTCOUNT=1
    #REALRT=10
    getData "SHORTLP" 2>&1 | tee deleteme_LP_shortrun_out.txt &
    #sleep 5

    echo "Running single HP client"
    # TODO: specify different port used by clients
    ClientList="-C ziger2 "
    CLIENTPORTSHIFT="-${LOAD}"
    CLIENTCOUNT=1
    #REALRT=10
    getData "SHORTHP" 2>&1 | tee deleteme_HP_shortrun_out.txt

    echo "waiting for benchmark to finish"
    wait
    echo "done with benchmarking"
fi





if [ "${DELETEFILES}" == "yes" ] ; then
    echo "Deleting output and log files: ${OUTPUTDIRPARENT}"
    echo "Warning:You might loose all the data from directory ${OUTPUTDIRPARENT}"
    read -p "Are you sure!! (y/n) :" ANSWER
    if [ "${ANSWER}" == "y" ] ;
    then
        set -x
        rm -rf ${OUTPUTDIRPARENT}
        set +x
    else
        echo "You answered ${ANSWER}, so not deleting everything"
    fi
fi
if [ "${CLEANUP}" == "yes" ] ; then
    echo "Cleaning up server: $SERVERNAME"
    ./cleanup.sh $SERVERNAME
    clist=$(echo $ClientList | sed 's/-C//g')
    echo "Cleaning up clients: $clist"
    ./cleanup.sh $clist
fi

