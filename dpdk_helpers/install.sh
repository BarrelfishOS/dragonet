#!/bin/bash

to_full_name() {
    local mname=$1
    fullName="ubuntu@${mname}.in.barrelfish.org"
    #fullName="ubuntu@${mname}"
    echo ${fullName}
}

reload_resolver() {
    local mname=$1
    fullName="ubuntu@${mname}.in.barrelfish.org"
    scp "./resolv.conf" "${fullName}://home/ubuntu/resolv.conf"
    ssh ${fullName} sudo "cp /home/ubuntu/resolv.conf /etc/resolv.conf"
}

on_machine_nosudo() {
    local mname=$1
    if [ -z ${mname} ] ; then
        echo "executing command without giving any machine name"
        exit 1
    fi
    fullName="ubuntu@${mname}.in.barrelfish.org"
    #fullName="ubuntu@${mname}"
    #check_connectivity ${fullName}
    ssh ${fullName} bash 2>&1 | sed "s/^/${mname}: /"
}


on_machine() {
    local mname=$1
    if [ -z ${mname} ] ; then
        echo "executing command without giving any machine name"
        exit 1
    fi

    fullName="ubuntu@${mname}.in.barrelfish.org"
    #fullName="ubuntu@${mname}"
    #check_connectivity ${fullName}
    ssh ${fullName} sudo bash 2>&1 | sed "s/^/${mname}: /"
}

prepare_machine() {

    reload_resolver ${MACHINE}
    echo 'apt-get update' | on_machine ${MACHINE}
    echo 'apt-get install -y screen byobu tree vim ctags cscope' | on_machine ${MACHINE}
    echo 'apt-get install -y build-essential' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-$(uname -r)' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-generic' | on_machine ${MACHINE}
    echo 'apt-get install -y netcat.traditional' | on_machine ${MACHINE}
    echo 'apt-get install -y git-core' | on_machine ${MACHINE}

    # for Dragonet
    echo 'apt-get install -y ghc cabal-install clang graphviz pdftk libghc-parsec2-dev libghc-stm-dev minisat' | on_machine ${MACHINE}

    reload_resolver ${MACHINE}

    reload_resolver ${MACHINE}
    echo 'cabal update' | on_machine_nosudo ${MACHINE}
    reload_resolver ${MACHINE}
    echo 'cabal install graphviz' | on_machine_nosudo ${MACHINE}
    echo 'cabal install pretty-show' | on_machine_nosudo ${MACHINE}
    echo 'cabal install MonadRandom' | on_machine_nosudo ${MACHINE}

    # For simplified source code navigation
    reload_resolver ${MACHINE}
    echo 'apt-get install -y happy' | on_machine ${MACHINE}
    echo 'cabal install hscope' | on_machine_nosudo ${MACHINE}
    echo 'cabal install SourceGraph' | on_machine_nosudo ${MACHINE}

    # my vim configuration
    scp  "vimconf.tar" "${HOST}:"
    echo 'tar -xvf vimconf.tar' | on_machine_nosudo ${MACHINE}
    echo 'cd /root/ ; tar -xvf /home/ubuntu/vimconf.tar' | on_machine ${MACHINE}
}

copy_code() {

    echo "rm -rf ${SOURCE_DIR}" | on_machine ${MACHINE}
    echo 'git clone ssh://shindep@129.132.186.96:8006/git/dragonet' | on_machine_nosudo ${MACHINE}
}

compile_code() {
    echo "cd ${SOURCE_DIR} ; make config T=x86_64-default-linuxapp-gcc" | on_machine ${MACHINE}
    echo "cd ${SOURCE_DIR} ; make" | on_machine ${MACHINE}
}

MACHINE="ziger1"
HOST="ubuntu@${MACHINE}.in.barrelfish.org"
#MACHINE="10.110.5.34"
#HOST="ubuntu@${MACHINE}"
SSH="ssh ${HOST}"
SRC_ARCHIVE="dpdk-1.5.0r1.tar.gz"
SOURCE_DIR="dpdk-1.5.0r1"
SOURCE_DIR="dragonet/dpdk-1.5.0r1"
updatedFile="lib/librte_sched/Makefile"


#set -x
set -e

echo "Preparing system"
prepare_machine

echo "Copying code"
copy_code

echo "Compiling code"
compile_code

echo "Done.."

