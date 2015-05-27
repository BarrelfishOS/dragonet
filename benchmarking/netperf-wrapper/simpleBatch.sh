#!/bin/bash

set -e

do_one_stack_run_big()
{
    if [[ -z ${NICTYPE} ]] || [[ -z ${CFUN} ]] || [[ -z ${QCOUNT} ]]  || [[ -z ${PSIZE} ]]  || [[ -z ${LOAD} ]] || [[ -z ${BMTYPE} ]]
    then
        echo "Error: one of the conf value is empty!!!"
        echo  "BMTYPE = ${BMTYPE} NICTYPE = ${NICTYPE},  CFUN = ${CFUN}, QCOUNT = ${QCOUNT}, PSIZE = ${PSIZE},  LOAD = ${LOAD}"
        exit 1
    fi
    # NOTE: -t should be always the first parameter

    set -x
    ./data_collection.sh -x -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT}
    ./data_collection.sh -t ${BMTYPE} -S -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -c 20 -l ${LOAD}

    sleep 5
    ./data_collection.sh -t ${BMTYPE}  -B -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -c 20 -l ${LOAD}

}



do_one_stack_run_dynamic()
{
    if [[ -z ${NICTYPE} ]] || [[ -z ${CFUN} ]] || [[ -z ${QCOUNT} ]]  || [[ -z ${PSIZE} ]]  || [[ -z ${LOAD} ]] || [[ -z ${BMTYPE} ]]
    then
        echo "Error: one of the conf value is empty!!!"
        echo  "BMTYPE = ${BMTYPE} NICTYPE = ${NICTYPE},  CFUN = ${CFUN}, QCOUNT = ${QCOUNT}, PSIZE = ${PSIZE},  LOAD = ${LOAD}"
        exit 1
    fi
    # NOTE: -t should be always the first parameter

    set -x
    ./data_collection.sh -x -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT}
    ./data_collection.sh -t ${BMTYPE} -S -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -c 20 -l ${LOAD}

    sleep 5
    ./data_collection.sh -t ${BMTYPE} -T -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -c 20 -l ${LOAD}
}


run_queue_num_combinations()
{
    QCOUNT=10
    do_one_stack_run_dynamic

    QCOUNT=5
    do_one_stack_run_dynamic
}



run_pkt_size_combinations()
{

    PSIZE=1024
    do_one_stack_run_dynamic

    PSIZE=64
    do_one_stack_run_dynamic
}

run_cost_fun_combinations()
{
    # for static costfun
    CFUN="S"
    run_pkt_size_combinations

    # for balance costfun
    CFUN="B"
    run_pkt_size_combinations
}


run_nic_hw_combinations()
{

    ####### For Intel NIC
    NICTYPE="I"
    run_pkt_size_combinations

    ####### For Solarflar NIC
    NICTYPE="S"
    run_pkt_size_combinations
}

run_load_combinations()
{
    LOAD=1
    do_one_stack_run_dynamic
    LOAD=4
    do_one_stack_run_dynamic
    LOAD=8
    do_one_stack_run_dynamic
    LOAD=16
    do_one_stack_run_dynamic
}


echoserver_linux_numbers()
{
    BMTYPE=E
    LOAD=32
    PSIZE=1024
    QCOUNT=10
    CFUN="B"
    NICTYPE="SO"
    do_one_stack_run_big

    NICTYPE="SL"
    do_one_stack_run_big
}

memcached_linux_numbers()
{
    BMTYPE=M
    LOAD=16
    QCOUNT=10
    CFUN="B"
    NICTYPE="I"
    PSIZE=1024
    do_one_stack_run_big
    PSIZE=64
    do_one_stack_run_big
}


############################################################################
#                   main
############################################################################

echoserver_linux_numbers
exit 0

BMTYPE=M
LOAD=16
PSIZE=64
PSIZE=1024
QCOUNT=10
CFUN="S"
NICTYPE="I"
run_cost_fun_combinations
sleep 5
QCOUNT=5
run_cost_fun_combinations
exit 0

do_one_stack_run_big
run_cost_fun_combinations

QCOUNT=5
exit 0


run_pkt_size_combinations

NICTYPE="SL"
run_pkt_size_combinations

NICTYPE="SO"
run_pkt_size_combinations
exit 0

do_one_stack_run_dynamic
NICTYPE="SL"
run_pkt_size_combinations
NICTYPE="S"
run_pkt_size_combinations
exit 0


############################################################################
#               for echo server
BMTYPE=E
############################################################################
NICTYPE="I"
memcached_linux_numbers

exit 0
echoserver_linux_numbers
exit 0


