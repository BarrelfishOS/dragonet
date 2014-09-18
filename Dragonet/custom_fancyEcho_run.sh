#!/bin/bash

#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 888  -a t1  -f ${SERVERIP}:888/10.113.4.71:9000  -f ${SERVERIP}:888/10.113.4.71:9001  -f ${SERVERIP}:888/10.113.4.71:9002  -f ${SERVERIP}:888/10.113.4.71:9003  -a t2  -f ${SERVERIP}:888/10.113.4.57:9000  -f ${SERVERIP}:888/10.113.4.57:9001  -f ${SERVERIP}:888/10.113.4.57:9002  -f ${SERVERIP}:888/10.113.4.57:9003  -a t3  -f ${SERVERIP}:888/10.113.4.29:9000  -f ${SERVERIP}:888/10.113.4.29:9001  -f ${SERVERIP}:888/10.113.4.29:9002  -f ${SERVERIP}:888/10.113.4.29:9003  -t -q t0  -t -q t1 -t -q t2 -t -q t3
#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 888  -a t1  -f ${SERVERIP}:888/10.113.4.71:0   -a t2  -f ${SERVERIP}:888/10.113.4.57:0  -a t3  -f ${SERVERIP}:888/10.113.4.29:0 -t -q t0  -t -q t1 -t -q t2 -t -q t3
#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t1  -f ${SERVERIP}:888/10.113.4.71:0   -a t2  -f ${SERVERIP}:888/10.113.4.57:0  -a t3  -f ${SERVERIP}:888/10.113.4.29:0  -t -q t1 -t -q t2 -t -q t3


run_for_16_cores() {
    echo fancyEcho running on ${SERVERIP} with 16 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001  -a t8   -f ${SERVERIP}:888/10.113.4.51:9002 -a t12  -f ${SERVERIP}:888/10.113.4.51:9003 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001  -a t9   -f ${SERVERIP}:888/10.113.4.71:9002 -a t13  -f ${SERVERIP}:888/10.113.4.71:9003 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001  -a t10  -f ${SERVERIP}:888/10.113.4.57:9002 -a t14  -f ${SERVERIP}:888/10.113.4.57:9003 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001  -a t11  -f ${SERVERIP}:888/10.113.4.29:9002 -a t15  -f ${SERVERIP}:888/10.113.4.29:9003 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11 -t -q t12 -t -q t13  -t -q t14 -t -q t15
}


run_for_12_cores() {
    echo fancyEcho running on ${SERVERIP} with 12 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001  -a t8   -f ${SERVERIP}:888/10.113.4.51:9002 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001  -a t9   -f ${SERVERIP}:888/10.113.4.71:9002 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001  -a t10  -f ${SERVERIP}:888/10.113.4.57:9002 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001  -a t11  -f ${SERVERIP}:888/10.113.4.29:9002 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11
}

run_for_10_cores() {
    echo fancyEcho running on ${SERVERIP} with 10 application threads
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001 \
-a t2  -f ${SERVERIP}:888/10.113.4.57:9000  -a t3  -f ${SERVERIP}:888/10.113.4.57:9001 \
-a t4  -f ${SERVERIP}:888/10.113.4.71:9000  -a t5  -f ${SERVERIP}:888/10.113.4.71:9001 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001 \
-a t8  -f ${SERVERIP}:888/10.113.4.51:9002  -a t9  -f ${SERVERIP}:888/10.113.4.57:9002 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 -t -q t8  -t -q t9
}


run_for_8_cores() {
    echo fancyEcho running on ${SERVERIP} with 8 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7
}

run_for_4_cores() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000 \
-a t1  -f ${SERVERIP}:888/10.113.4.71:9000 \
-a t2  -f ${SERVERIP}:888/10.113.4.57:9000 \
-a t3  -f ${SERVERIP}:888/10.113.4.29:9000 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3
}


run_for_4_cores_with_wildcards() {
# old way, with wildcards
    echo fancyEcho running on ${SERVERIP} with 4 application threads and wildcards
sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:0 \
    -a t1  -f ${SERVERIP}:888/10.113.4.71:0 \
    -a t2  -f ${SERVERIP}:888/10.113.4.57:0 \
    -a t3  -f ${SERVERIP}:888/10.113.4.29:0 \
    -t -q t0  -t -q t1 -t -q t2 -t -q t3
}

run_for_2_cores_SP() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:9000 -f ${SERVERIP}:888/10.113.4.71:9000 \
    -a t1  -f ${SERVERIP}:888/10.113.4.57:9000 -f ${SERVERIP}:888/10.113.4.29:9000 \
    -t -q t0 -t -q t1
}


run_for_2_cores_full_filters() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:9000 \
    -f ${SERVERIP}:888/10.113.4.29:9000 \
    -a t1 -p 888 \
    -t -q t0 -t -q t1
}

run_for_1_cores() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0  -p 888 -t -q t0
}

run_for_1_cores_null() {
    echo fancyEcho running on ${SERVERIP} with single application thread and two sockets for null testing
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0 -p 222 \
    -a t1 -p 111 \
    -a t2 -p 333 \
    -a t3 -p 444 \
    -t -q t0 -q t1 -q t2 -q t3
}


if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    echo "USAGE: $0 <stackname>"
    echo "EXAMPLE: $0 tap"
    echo "EXAMPLE: $0 sf"
    echo "EXAMPLE: $0 e10k"
    echo "EXAMPLE: $0 null"
    exit 1
fi


STACKNAME=stack-"${1}"

# For Solarflare
if [ "$STACKNAME" ==  "stack-sf" ] ; then
    SERVERIP="10.113.4.195"
else

    # For intel
    if [ "$STACKNAME" ==  "stack-e10k" ] ; then
        SERVERIP="10.113.4.95"
        SERVERIP="10.113.4.96"
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


./scripts/pravin/wait_for_dragonet.sh 10 ${STACKNAME}

run_for_10_cores
#run_for_8_cores
#run_for_4_cores
#run_for_1_cores_null

#run_for_4_cores_with_wildcards

# for quick cleanup
sudo killall ${STACKNAME}

exit 0

#run_for_2_cores_SP

#run_for_4_cores

#run_for_2_cores_full_filters

#run_for_16_cores
#run_for_16_cores
#run_for_1_cores
#run_for_12_cores
#run_for_8_cores
#run_for_2_cores

exit 0

## NOTE:
# This code was used for deploying fancyecho manually for initial results with
#       new Dragonet stack.

# 10.113.4.51  # Burrata # 175178848 -- old
# 10.113.4.96  # Burrata #

# 10.113.4.20  # gruyere # 175178772
# 10.113.4.57  # ziger2  # 175178809
# 10.113.4.29  # sbrinz2 # 175178781
# 10.113.4.95  # Asiago  # 175178847  # Intel
# 10.113.4.195 # Asiago  # 175178947  # solarflare

# appenzeller-e1000 : 10.113.4.71
# ziger1 : 10.113.4.51
# ziger2 : 10.113.4.57
# sbrinz2 : 10.113.4.29



