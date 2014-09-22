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
    commands_to_kill="${commands_to_kill} fancyEchoLinux"
    echo "ssh ${mname} sudo killall ${commands_to_kill}"
    ssh ${mname} "sudo killall ${commands_to_kill}"
    ssh ${mname} "sudo rm -rf tempResult*"
}

ssh asiago "sudo rm -rf tempResult*"
#clean_machine asiago
#clean_machine burrata
clean_machine sbrinz2
clean_machine ziger2
clean_machine gruyere

