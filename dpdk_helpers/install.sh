#!/bin/bash

on_machine_nosudo() {
    local mname=$1
    if [ -z ${mname} ] ; then
        echo "executing command without giving any machine name"
        exit 1
    fi

    #fullName="ubuntu@${mname}.in.barrelfish.org"
    fullName="ubuntu@${mname}"
    #check_connectivity ${fullName}
    ssh ${fullName} bash 2>&1 | sed "s/^/${mname}: /"
}

on_machine() {
    local mname=$1
    if [ -z ${mname} ] ; then
        echo "executing command without giving any machine name"
        exit 1
    fi

    #fullName="ubuntu@${mname}.in.barrelfish.org"
    fullName="ubuntu@${mname}"
    #check_connectivity ${fullName}
    ssh ${fullName} sudo bash 2>&1 | sed "s/^/${mname}: /"
}

prepare_machine() {
    echo 'apt-get update' | on_machine ${MACHINE}
    echo 'apt-get install -y screen byobu tree vim ctags cscope' | on_machine ${MACHINE}
    echo 'apt-get install -y build-essential' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-$(uname -r)' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-generic' | on_machine ${MACHINE}
    scp  "vimconf.tar" "${HOST}:"
    echo 'tar -xvf vimconf.tar' | on_machine_nosudo ${MACHINE}
    echo 'cd /root/ ; tar -xvf /home/ubuntu/vimconf.tar' | on_machine ${MACHINE}
}

copy_code() {

    # removing old code (if any)
    echo "rm -rf ${SOURCE_DIR}" | on_machine ${MACHINE}
    echo "rm -rf ${SRC_ARCHIVE}" | on_machine ${MACHINE}

    # Copying new codebase
    scp  "../${SRC_ARCHIVE}" "${HOST}:"
    echo "tar -xf ${SRC_ARCHIVE}" | on_machine ${MACHINE}

    # Copying the updated file to take care of the compile error
    scp "../${SOURCE_DIR}/${updatedFile}"  "${HOST}:"
    echo "cp `basename ${updatedFile}` ${SOURCE_DIR}/${updatedFile}" | on_machine ${MACHINE}
}

compile_code() {
    echo "cd ${SOURCE_DIR} ; make config T=x86_64-default-linuxapp-gcc" | on_machine ${MACHINE}
    echo "cd ${SOURCE_DIR} ; make" | on_machine ${MACHINE}
}

setup_huge_pages() {
    echo "mkdir -p /mnt/huge"  | on_machine ${MACHINE}
    echo "mount -t hugetlbfs nodev /mnt/huge"  | on_machine ${MACHINE}
    echo "echo 64 > /sys/devices/system/node/node0/hugepages/hugepages-2048kB/nr_hugepages"  | on_machine ${MACHINE}
}

install_new_driver() {
    # Removing older versions of drivers
    echo "lsmod | grep igb_uio && rmmod igb_uio" | on_machine ${MACHINE}
    echo "lsmod | grep uio && rmmod uio" | on_machine ${MACHINE}
    sleep 5
    echo "modprobe uio" | on_machine ${MACHINE}
    echo "cd ${SOURCE_DIR} ; insmod build/kmod/igb_uio.ko" | on_machine ${MACHINE}
    echo "Current drivers list"
    echo "lsmod | grep igb_uio" | on_machine ${MACHINE}
}

run_test_command() {
    # Removing older versions of drivers
    echo "cd ${SOURCE_DIR}; build/app/testpmd -c7 -n3 -- -i --nb-cores=2 --nb-ports=2"  | on_machine ${MACHINE}
}

quick_test() {
    # runs part of the script as very quick test

    echo "installing new driver"
    install_new_driver

    echo "Running test command to check working"
    run_test_command
    exit 0
}

MACHINE="ziger1"
#HOST="ubuntu@${MACHINE}.in.barrelfish.org"
MACHINE="10.110.5.34"
HOST="ubuntu@${MACHINE}"
SSH="ssh ${HOST}"
SRC_ARCHIVE="dpdk-1.5.0r1.tar.gz"
SOURCE_DIR="dpdk-1.5.0r1"
updatedFile="lib/librte_sched/Makefile"


#set -x
set -e
# Quick test
#quick_test

echo "Preparing system"
prepare_machine

echo "Copying code"
copy_code

echo "Compiling code"
compile_code

exit 0

echo "setup huge pages"
install_new_driver

echo "installing new driver"
install_new_driver

echo "Running test command to check working"
run_test_command


