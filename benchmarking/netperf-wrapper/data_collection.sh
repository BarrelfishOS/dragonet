#!/bin/bash


startStack() {
#./cleanupServer.sh

local title="ITest_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
local OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/${DNCOSTFUNCTION}/STARTSTACK/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
mkdir -p "${OUTDIR}"

./netperf-wrapper -d 0 --udp --serverCoreShift ${SRVCORESHIFT} ${ClientList} --servercores ${SRVCORES} \
 --serverInstances 1 --hwqueues ${HWQUEUE} --clientcores ${CLIENTCORES}  --packet ${PACKETSIZE} --concurrency ${LOAD} \
-I 1 -l 5 -H ${SERVERNAME} -T ${SERVERIP} -c ${ECHO_SERVER} ${UDP_TEST_NAME} \
 --totalClients ${CLIENTCOUNT}  --clientPortShift ${CLIENTPORTSHIFT} \
 --dragonet-cost-function ${DNCOSTFUNCTION} \
-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog"
#-t ${title} -o ${OUTDIR} -L  "${OUTDIR}/${title}.runlog" 2>&1 | tee "${OUTDIR}/${title}.output"


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


local title="STest_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"
local OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/${DNCOSTFUNCTION}/SHORT/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
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

local title="LTest_${UDP_TEST_NAME},CONCUR_${LOAD},PKT_${PACKETSIZE},${NICTYPE},SRV_${ECHO_SERVER},CLC_${CLIENTCOUNT}"

local OUTDIR="${OUTPUTDIRPARENT}/${WORKLOADTYPE}/${DNCOSTFUNCTION}/${prefix}/HWQ_${HWQUEUE}/SRVCORE_${SRVCORES}/SRVSHIFT_${SRVCORESHIFT}/CLCORE_${CLIENTCORES}/Test_${UDP_TEST_NAME}/CONCUR_${LOAD}/PKT_${PACKETSIZE}/${NICTYPE}/SRV_${ECHO_SERVER}/"
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
DUMMYRT=30
DUMMYRT=30
REALRT=50

######################

set_echo_test() {
    SRVCORESHIFT=3
    #SRVCORES=7
    SRVCORES=7
    UDP_TEST_NAME="udp_rr"
    LOAD=64
    CLIENTCOUNT=10
    ClientList="-C sbrinz1 -C sbrinz2"
}

set_memcached_test() {
    SRVCORESHIFT=0
    SRVCORES=2
    UDP_TEST_NAME="memcached_rr"
    LOAD=32
    CLIENTCOUNT=2
    ClientList="-C sbrinz1 -C sbrinz2"
}



######################
UDP_TEST_NAME="memcached_rr"
SRVCORESHIFT=0

######################
UDP_TEST_NAME="udp_rr"
SRVCORESHIFT=0

######################
PACKETSIZE=64
PACKETSIZE=1024

######################
DNCOSTFUNCTION="priority"
DNCOSTFUNCTION="static"
DNCOSTFUNCTION="balance"

######################

NICTYPE="NIC_Intel"
ECHO_SERVER="e10k-dpdk"
SERVERIP=10.113.4.95

NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_Intel"
ECHO_SERVER="memcached_linux"
SERVERIP=10.113.4.95

NICTYPE="NIC_SF"
ECHO_SERVER="memcached_onload"
SERVERIP=10.113.4.195


NICTYPE="NIC_SF"
ECHO_SERVER="llvmSF"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="fancyEchoLinux"
SERVERIP=10.113.4.195

NICTYPE="NIC_SF"
ECHO_SERVER="fancyEchoOnload"
SERVERIP=10.113.4.195


NICTYPE="NIC_SF"
ECHO_SERVER="memcached_linux"
SERVERIP=10.113.4.195



######################

show_usage() {
        echo "Please select what you want to run"
        echo "Usage: ${0} [-c -s -l]"
        echo "           -N S/I -->  NIC type (S --> Solarflare, I --> Intel)"
        echo "           -C S/B -->  Cost Function (S --> Static, B --> Balance)"
        echo "           -t E/M -->  Test type (E --> Echo server, M --> memcached)"
        echo "           -c <n> -->  client count (n clients)"
        echo "           -l <n> -->  load (n concurrent sessions per client)"
        echo "           -Q <n> -->  n hardware queues and server threads"
        echo "           -m S/D -->  Mixed run (sending extra HP and LP while big run is happening)"
        echo "                         with static or dynamic setup"
        echo "           -p <n>  -->  packet size == n"
        echo "           -S     -->  start stack"
        echo "           -d     -->  dummy run"
        echo "           -r     -->  Real run"
        echo "           -b     -->  Run simultaneousely both memcached and echo workload"
        echo "           -x     -->  Cleanup involved machines"
        echo "           -X     -->  Delete output/log files"
        echo "           -h     -->  this help"
        echo "           -T     -->  Run ADDDYNAMICFLOWS"
        echo "           -B     -->  Run Big"
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

while getopts ":c:l:Q:p:SdhxXrbm:t:C:N:TB" opt; do
  case $opt in
    N)
        echo "Setting up the NIC type to $OPTARG"
        if [ "$OPTARG" == "S" ] ;
        then
            NICTYPE="NIC_SF"
            ECHO_SERVER="llvmSF"
           # ECHO_SERVER="memcached_linux"
            SERVERIP=10.113.4.195
        elif [ "$OPTARG" == "I" ] ;
        then
            NICTYPE="NIC_Intel"
            ECHO_SERVER="e10k-dpdk"
            SERVERIP=10.113.4.95
        else
            echo "Error: Invalid NIC hardware name given"
            show_usage
            exit 1
        fi
      ;;
    C)
        echo "Setting up cost function to $OPTARG"
        if [ "$OPTARG" == "S" ] ;
        then
            DNCOSTFUNCTION="static"
        elif [ "$OPTARG" == "B" ] ;
        then
            DNCOSTFUNCTION="balance"
        else
            echo "Error: Invalid cost function given"
            show_usage
            exit 1
        fi
      ;;
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
    Q)
 #       if [ "$OPTTEST" == "yes" ] ;
 #       then
 #           echo "ERROR: servercores are hardcoded by -t option, so you can't change it!"
 #           exit 1
 #       fi

        ISHWQUEUESET="yes"
        HWQUEUE=10
        SRVCORES=9
        if [ "$OPTARG" == 10 ] ;
        then
            SRVCORES=10
            HWQUEUE=10
        elif [ "$OPTARG" == 5 ] ;
        then
            SRVCORES=5
            HWQUEUE=5
        else
            echo "Error: Invalid hardware queues given $OPTARG"
            show_usage
            exit 1
        fi
        echo "Setting HWQs ${HWQUEUE} and server-cores/threads to $SRVCORES"
      ;;
    p)
        if [ "$OPTARG" == 64 ] ;
        then
            PACKETSIZE=64
        elif [ "$OPTARG" == 1024 ] ;
        then
            PACKETSIZE=1024
        else
            echo "Error: Invalid packet-size given $OPTARG"
            echo "  Only 64 and 1024 are currently supported"
            show_usage
            exit 1
        fi
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
        ADDDYNAMICFLOWS="yes"
      ;;
    B)
        BIGSTABLE="yes"
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

######################

if [ ! "${ISHWQUEUESET}" == "yes" ] ; then
    echo "ERROR: No HW queues given"
    show_usage
    exit 1
fi

######################

##################################################################
##################################################################
##################################################################
##################################################################
######################
SERVERNAME="babybel2"
CLIENTCORES=1
######################



######################
ClientList="-C sbrinz1 -C sbrinz2 -C ziger1 -C gruyere"
######################

######################

OUTPUTDIRPARENT="./debugSFresults_dbg_devBatch16/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"
OUTPUTDIRPARENT="./trios15_results_v2/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"
OUTPUTDIRPARENT="./trios15_results_linux/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"
OUTPUTDIRPARENT="./trios15_results_stable_small/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"
OUTPUTDIRPARENT="./trios15_results_stable2/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"
OUTPUTDIRPARENT="./debug_3machines/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"

OUTPUTDIRPARENT="./trios15_results_sfDeug/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"

OUTPUTDIRPARENT="./trios15_results_sf_bigmem8/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"

#### following location has good results !!!!!!
        ## backed up at location ./trios15_results_improved_bak
OUTPUTDIRPARENT="./trios15_results_improved/B${PACKETSIZE}/${NICTYPE}_${DNCOSTFUNCTION}_${HWQUEUE}/"

##################################################################
##################################################################


if [ "${RUNSTACK}" == "yes" ] ; then

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


if [ "${RUNDUMMY}" == "yes" ] ; then

    echo "Assuming that stack is already running, just running dummy for testing"

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


if [ "${RUNREAL}" == "yes" ] ; then

    echo "Assuming that stack is already running, doing real long run run"
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

if [ "${MIXEDRUN}" == "yes" ] ; then
    echo "Assuming that stack is already running, running a big run"
    echo " and without waiting, starting LP and HP as soon as big run is running"

    WORKLOADTYPE="MixedRun"
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
    ClientList="-C sbrinz1 "
    CLIENTPORTSHIFT="3000"
    CLIENTCOUNT=1
    #REALRT=10
    getData "SHORTLP" 2>&1 | tee deleteme_LP_shortrun_out.txt &
    #sleep 5

    if [ "${DNCOSTFUNCTION}" == "priority" ] ;
    then
        echo "Not running HP load as priority oracle does not support it yet"
    else

        echo "Running single HP client"
        # TODO: specify different port used by clients
        ClientList="-C sbrinz2 "
        CLIENTPORTSHIFT="-${LOAD}"
        CLIENTCOUNT=1
        #REALRT=10
        getData "SHORTHP" 2>&1 | tee deleteme_HP_shortrun_out.txt
    fi

    echo "waiting for benchmark to finish"
    wait
    echo "done with benchmarking"
fi



if [ "${ADDDYNAMICFLOWS}" == "yes" ] ; then
    echo "Assuming that stack is already running"
    echo " doing big run with additional LP and HP flow"

    WORKLOADTYPE="ADDDYNAMICFLOWS"
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
    ClientList="-C sbrinz2 "
    CLIENTPORTSHIFT="3000"
    CLIENTCOUNT=1
    REALRT=50
    getData "SHORTLP" 2>&1 | tee deleteme_LP_shortrun_out.txt

    sleep 10

    if [ "${DNCOSTFUNCTION}" == "priority" ] ;
    then
        echo "Not running HP load as priority oracle does not support it yet"
    else
        echo "Running single HP client"
        # TODO: specify different port used by clients
        ClientList="-C sbrinz2 "
        CLIENTPORTSHIFT="-${LOAD}"
        CLIENTCOUNT=1
        REALRT=50
        getData "SHORTHP" 2>&1 | tee deleteme_HP_shortrun_out.txt
    fi

    echo "waiting for benchmark to finish"
    wait
    echo "done with benchmarking"
fi


if [ "${BIGSTABLE}" == "yes" ] ; then
    echo "Assuming that stack is already running, doing a big-stable run"

    WORKLOADTYPE="RUNBIGALONE"
    REALRT=130
    echo "Starting big set of clients for the load for test ${UDP_TEST_NAME}"

    rm -f deleteme_longrun_startTime.txt
    getData "LONG" "./deleteme_longrun_startTime.txt"
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

