#!/bin/bash

do_one_stack_run()
{
    if [[ -z ${NICTYPE} ]] || [[ -z ${CFUN} ]] || [[ -z ${QCOUNT} ]]  || [[ -z ${PSIZE} ]]  || [[ -z ${LOAD} ]]
    then
        echo "Error: one of the conf value is empty!!!"
        echo  "NICTYPE = ${NICTYPE},  CFUN = ${CFUN}, QCOUNT = ${QCOUNT}, PSIZE = ${PSIZE},  LOAD = ${LOAD}"
        exit 1
    fi
    ./data_collection.sh -x -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT}
    sleep 5
    ./data_collection.sh -S -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -t M -c 20 -l ${LOAD}
    sleep 5
#    ./data_collection.sh -B -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -t M -c 20 -l ${LOAD}
#    sleep 5
    ./data_collection.sh -T -N ${NICTYPE} -C ${CFUN} -Q ${QCOUNT} -p ${PSIZE} -t M -c 20 -l ${LOAD}
}


run_queue_num_combinations()
{
    QCOUNT=10
    do_one_stack_run

    QCOUNT=5
    do_one_stack_run
}


run_cost_fun_combinations()
{
    # for static costfun
    CFUN="S"
    run_queue_num_combinations

    # for balance costfun
    CFUN="B"
    run_queue_num_combinations
}

run_pkt_size_combinations()
{

    PSIZE=1024
    run_cost_fun_combinations

    PSIZE=64
    run_cost_fun_combinations
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
    do_one_stack_run
    LOAD=4
    do_one_stack_run
    LOAD=8
    do_one_stack_run
    LOAD=16
    do_one_stack_run
}


############################################################################
#                   main
############################################################################

NICTYPE="S"
LOAD=16
PSIZE=64
QCOUNT=10
CFUN="S"
run_pkt_size_combinations
exit 0

run_cost_fun_combinations

do_one_stack_run
CFUN="S"
do_one_stack_run
exit 0


NICTYPE="I"
QCOUNT=10
CFUN="S"
do_one_stack_run
exit 0

NICTYPE="S"
do_one_stack_run

exit 0


LOAD=16
do_one_stack_run


NICTYPE="S"
do_one_stack_run
exit 0

####### and now balancing cost function
CFUN="B"
do_one_stack_run
exit 0

######################## running everything   ###############
LOAD=16
PSIZE=1024
# Run all combinations ####
run_nic_hw_combinations
exit 0


