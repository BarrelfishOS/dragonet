#!/bin/bash

clean_machine() {
    mname=$1
    echo "cleaning up machine $mname"
    ssh ${mname} "sudo killall memaslap"
    ssh ${mname} "sudo killall netperf"
    ssh ${mname} "sudo killall netserver"
    ssh ${mname} "sudo killall memcached"
    #ssh ${mname} "sudo killall llvm-cgen-e10k"
    ssh ${mname} "sudo rm -rf tempResult*"
}

clean_machine asiago
clean_machine sbrinz2
clean_machine ziger2
#clean_machine sbrinz2
#clean_machine gruyere
#clean_machine burrata


