#!/bin/bash

#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 888  -a t1  -f 10.113.4.95:888/10.113.4.20:9000  -f 10.113.4.95:888/10.113.4.20:9001  -f 10.113.4.95:888/10.113.4.20:9002  -f 10.113.4.95:888/10.113.4.20:9003  -a t2  -f 10.113.4.95:888/10.113.4.57:9000  -f 10.113.4.95:888/10.113.4.57:9001  -f 10.113.4.95:888/10.113.4.57:9002  -f 10.113.4.95:888/10.113.4.57:9003  -a t3  -f 10.113.4.95:888/10.113.4.29:9000  -f 10.113.4.95:888/10.113.4.29:9001  -f 10.113.4.95:888/10.113.4.29:9002  -f 10.113.4.95:888/10.113.4.29:9003  -t -q t0  -t -q t1 -t -q t2 -t -q t3
#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 888  -a t1  -f 10.113.4.95:888/10.113.4.20:0   -a t2  -f 10.113.4.95:888/10.113.4.57:0  -a t3  -f 10.113.4.95:888/10.113.4.29:0 -t -q t0  -t -q t1 -t -q t2 -t -q t3
#sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t1  -f 10.113.4.95:888/10.113.4.20:0   -a t2  -f 10.113.4.95:888/10.113.4.57:0  -a t3  -f 10.113.4.95:888/10.113.4.29:0  -t -q t1 -t -q t2 -t -q t3


run_for_16_cores() {
    echo fancyEcho running with 16 application thread and 8 sockets and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f 10.113.4.95:888/10.113.4.96:9000  -a t1  -f 10.113.4.95:888/10.113.4.96:9001  -a t8  -f 10.113.4.95:888/10.113.4.96:9002  -a t12  -f 10.113.4.95:888/10.113.4.96:9003 \
-a t2  -f 10.113.4.95:888/10.113.4.20:9000  -a t3  -f 10.113.4.95:888/10.113.4.20:9001  -a t9  -f 10.113.4.95:888/10.113.4.20:9002  -a t13  -f 10.113.4.95:888/10.113.4.20:9003 \
-a t4  -f 10.113.4.95:888/10.113.4.57:9000  -a t5  -f 10.113.4.95:888/10.113.4.57:9001  -a t10  -f 10.113.4.95:888/10.113.4.57:9002 -a t14  -f 10.113.4.95:888/10.113.4.57:9003 \
-a t6  -f 10.113.4.95:888/10.113.4.29:9000  -a t7  -f 10.113.4.95:888/10.113.4.29:9001  -a t11  -f 10.113.4.95:888/10.113.4.29:9002 -a t15  -f 10.113.4.95:888/10.113.4.29:9003 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11 -t -q t12 -t -q t13  -t -q t14 -t -q t15
}


run_for_12_cores() {
    echo fancyEcho running with 12 application thread and 8 sockets and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f 10.113.4.95:888/10.113.4.96:9000  -a t1  -f 10.113.4.95:888/10.113.4.96:9001  -a t8  -f 10.113.4.95:888/10.113.4.96:9002 \
-a t2  -f 10.113.4.95:888/10.113.4.20:9000  -a t3  -f 10.113.4.95:888/10.113.4.20:9001  -a t9  -f 10.113.4.95:888/10.113.4.20:9002 \
-a t4  -f 10.113.4.95:888/10.113.4.57:9000  -a t5  -f 10.113.4.95:888/10.113.4.57:9001  -a t10  -f 10.113.4.95:888/10.113.4.57:9002 \
-a t6  -f 10.113.4.95:888/10.113.4.29:9000  -a t7  -f 10.113.4.95:888/10.113.4.29:9001  -a t11  -f 10.113.4.95:888/10.113.4.29:9002 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11
}


run_for_8_cores() {
    echo fancyEcho running with 8 application thread and 8 sockets and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f 10.113.4.95:888/10.113.4.96:9000  -a t1  -f 10.113.4.95:888/10.113.4.96:9001 \
-a t2  -f 10.113.4.95:888/10.113.4.20:9000  -a t3  -f 10.113.4.95:888/10.113.4.20:9001 \
-a t4  -f 10.113.4.95:888/10.113.4.57:9000  -a t5  -f 10.113.4.95:888/10.113.4.57:9001 \
-a t6  -f 10.113.4.95:888/10.113.4.29:9000  -a t7  -f 10.113.4.95:888/10.113.4.29:9001 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7
}

run_for_4_cores() {
    echo fancyEcho running with four application thread and four sockets
sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0  -f 10.113.4.95:888/10.113.4.96:0 -a t1  -f 10.113.4.95:888/10.113.4.20:0   -a t2  -f 10.113.4.95:888/10.113.4.57:0  -a t3  -f 10.113.4.95:888/10.113.4.29:0 -t -q t0  -t -q t1 -t -q t2 -t -q t3
}

run_for_2_cores() {
    echo fancyEcho running with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 888 -a t1 -p 888 -t -q t0 -t -q t1
}


run_for_1_cores() {
    echo fancyEcho running with single application thread and single socket
sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0  -p 888 -t -q t0
}

#run_for_16_cores
run_for_1_cores

exit 0

## NOTE:
# This code was used for deploying fancyecho manually for initial results with
#       new Dragonet stack.

10.113.4.96 # Burrata
10.113.4.20 # gruyere
10.113.4.29 # sbrinz2
10.113.4.57 # ziger2

