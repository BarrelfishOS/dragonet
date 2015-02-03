#!/bin/bash

clean_machine() {
    mname=$1
    commands_to_kill=""
    commands_to_kill="${commands_to_kill} memaslap"
    commands_to_kill="${commands_to_kill} netperf"
    commands_to_kill="${commands_to_kill} netserver"
    commands_to_kill="${commands_to_kill} memcached"
    commands_to_kill="${commands_to_kill} bench-fancyecho"
    commands_to_kill="${commands_to_kill} stack-sf"
    commands_to_kill="${commands_to_kill} stack-tap"
    commands_to_kill="${commands_to_kill} stack-e10k"
    commands_to_kill="${commands_to_kill} stack-dpdk"
    commands_to_kill="${commands_to_kill} stack-dpdk2"
    commands_to_kill="${commands_to_kill} fancyEchoLinux"
    commands_to_kill="${commands_to_kill} python2"
    echo "ssh ${mname} sudo killall -9 ${commands_to_kill}"
    ssh ${mname} "sudo killall  ${commands_to_kill}"
    # Trying again harder for anything which is still stuck around
    ssh ${mname} "sudo killall -9  ${commands_to_kill}"
    ssh ${mname} "sudo rm -rf tempResult*"
}


for m in $@
do
    #clean_machine $m
    clean_machine $m 2> /dev/null
done
