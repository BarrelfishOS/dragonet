#!/bin/bash

### for memcached
#sudo ../benchmarking/memcached/memcached -N f[${SERVERIP}:7777/10.113.4.71:0]F[${SERVERIP}:7777/10.113.4.29:0]F[${SERVERIP}:7777/10.113.4.51:0]F[${SERVERIP}:7777/10.113.4.57:0] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 8 -l ${SERVERIP}

#sudo ../benchmarking/memcached/memcached -N p[7777] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
#sudo ../benchmarking/memcached/memcached -N T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 2 -l ${SERVERIP}


run_memcached_for_1_cores() {
sudo ../benchmarking/memcached/memcached -N T0.p[7777]F[${SERVERIP}:7777/10.113.4.71:8000]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.71:8001]F[${SERVERIP}:7777/10.113.4.29:8001] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
}

run_memcached_4_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.26:8000]F[${SERVERIP}:7777/10.113.4.26:8001]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.29:8001]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l ${SERVERIP}
}

run_memcached_2_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
 T0.p[7777]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.57:8002]F[${SERVERIP}:7777/10.113.4.57:8003]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l ${SERVERIP}
}

run_memcached_1_clients_1_load() {
sudo ../benchmarking/memcached/memcached -N \
 T0.p[7777]F[${SERVERIP}:7777/10.113.4.57:8000]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l ${SERVERIP}
}

run_memcached_2_clients_3_load() {
set -x
set -e
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.51:8002]F[${SERVERIP}:7777/10.113.4.26:8000]F[${SERVERIP}:7777/10.113.4.26:8001]F[${SERVERIP}:7777/10.113.4.26:8002]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l ${SERVERIP}
echo "Done with running memcached"
}



run_memcached_2_cores_specific() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 2 -l ${SERVERIP}
# use following command from appenzeller to test the flows (10.113.4.51 is ziger1 10G interface)
# ssh ziger1 'cd dragonet/benchmarking/libmemcached-1.0.18 ; memaslap -s ${SERVERIP}:7777 --udp -x100  --cfg_cmd=./bmKey_64_val_1024.conf -T1 -c 2 -b -S10m -z 8000'
}

run_echo_for_4_cores_ziger2() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
    lport=9000
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-W -a t0 -p ${lport} -a t1 -p ${lport} -a t2 -p ${lport} -a t3 -p ${lport} \
-F ${SERVERIP}:${lport}/10.113.4.57:8000 \
-F ${SERVERIP}:${lport}/10.113.4.57:8001 \
-F ${SERVERIP}:${lport}/10.113.4.57:8002 \
-F ${SERVERIP}:${lport}/10.113.4.57:8003 \
-t -q t0 -t -q t1 -t -q t2 -t -q t3
}

run_memcached_5_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.57:8002]F[${SERVERIP}:7777/10.113.4.57:8003]F[${SERVERIP}:7777/10.113.4.57:8004]F[${SERVERIP}:7777/10.113.4.57:8005]F[${SERVERIP}:7777/10.113.4.57:8006]F[${SERVERIP}:7777/10.113.4.57:8007]F[${SERVERIP}:7777/10.113.4.57:8008]F[${SERVERIP}:7777/10.113.4.57:8009]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l ${SERVERIP}
}

run_echo_for_5_cores() {
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-W  -a T0 -p 888 -a T1 -p 888 -a T2 -p 888 -a T3 -p 888 -a T4 -p 888 \
 -F 10.113.4.95:888/10.113.4.51:5000  -F 10.113.4.95:888/10.113.4.57:5000 \
 -F 10.113.4.95:888/10.113.4.26:5000  -F 10.113.4.95:888/10.113.4.29:5000 \
 -F 10.113.4.95:888/10.113.4.51:5001 \
 -t -q T0 -t -q T1 -t -q T2 -t -q T3 -t -q T4
}



run_memcached_10_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
 T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.26:8000]F[${SERVERIP}:7777/10.113.4.26:8001]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.29:8001]F[${SERVERIP}:7777/10.113.4.51:8002]F[${SERVERIP}:7777/10.113.4.51:8003]F[${SERVERIP}:7777/10.113.4.57:8002]F[${SERVERIP}:7777/10.113.4.57:8003]F[${SERVERIP}:7777/10.113.4.26:8002]F[${SERVERIP}:7777/10.113.4.26:8003]F[${SERVERIP}:7777/10.113.4.29:8002]F[${SERVERIP}:7777/10.113.4.29:8003]F[${SERVERIP}:7777/10.113.4.51:8004]F[${SERVERIP}:7777/10.113.4.51:8005]F[${SERVERIP}:7777/10.113.4.57:8004]F[${SERVERIP}:7777/10.113.4.57:8005] \
-c 64000 -m 64000 -u root -p 0 -U  7777 -t 9 -l ${SERVERIP}
}



dpdk_debug_3_cores_test() {
sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -W -a T0 -p 888 -a T1 -p 888 -a T2 -p 888 -a T3 -p 888 -a T4 -p 888  \
    -F ${SERVERIP}:888/10.113.4.51:5000  -F ${SERVERIP}:888/10.113.4.29:5000 \
    -F ${SERVERIP}:888/10.113.4.51:5001  -F ${SERVERIP}:888/10.113.4.29:5001 \
    -F ${SERVERIP}:888/10.113.4.51:5002  -F ${SERVERIP}:888/10.113.4.29:5002 \
    -F ${SERVERIP}:888/10.113.4.51:5003  -F ${SERVERIP}:888/10.113.4.29:5003 \
    -F ${SERVERIP}:888/10.113.4.51:5004  -F ${SERVERIP}:888/10.113.4.29:5004 \
    -t -q T0 -t -q T1 -t -q T2 -t -q T3 -t -q T4
}

##################################################################

set -x
set -e

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    echo "USAGE: $0 <stackname>"
    echo "EXAMPLE: $0 tap"
    echo "EXAMPLE: $0 sf"
    echo "EXAMPLE: $0 e10k"
    echo "EXAMPLE: $0 e10k"
    echo "EXAMPLE: $0 null"
    exit 1
fi


STACKNAME=stack-"${1}"

##################################################################
#               working out the server IP address
##################################################################
# For Solarflare
if [ "$STACKNAME" ==  "stack-sf" ] ; then
    SERVERIP="10.113.4.195"  # for asiago as server on solarflare
else

    # For intel
    if [ "$STACKNAME" ==  "stack-e10k-dpdk" ] ; then
        SERVERIP="10.113.4.96"  # For burrata as server
        SERVERIP="10.113.4.26"  # For sbrinz1 as server
        SERVERIP="10.113.4.95"  # For asiago as server
        echo "NOTE: using ${SERVERIP} as server-IP address"
    else
        if [ "$STACKNAME" ==  "stack-tap" ] ; then
            SERVERIP="192.168.123.1"
        else

            if [ "$STACKNAME" ==  "stack-null" ] ; then
                SERVERIP="127.0.0.1"
            else
                print "Invalid stack type $STACKNAME"
                exit 1
            fi
        fi
    fi
fi


##################################################################
        ## Main ##
##################################################################

./scripts/pravin/wait_for_dragonet.sh 5 ${STACKNAME}
dpdk_debug_3_cores_test
#sudo killall ${STACKNAME}
exit 0
##################################################################

