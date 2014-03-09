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
    ssh ${fullName} sudo "chmod +w /etc/resolv.conf"
    ssh ${fullName} sudo "cp /home/ubuntu/resolv.conf /etc/resolv.conf"
    ssh ${fullName} sudo "chmod -w /etc/resolv.conf"
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

#    reload_resolver ${MACHINE}
    echo 'apt-get update' | on_machine ${MACHINE}
    echo 'apt-get install -y screen byobu tree vim ctags cscope vim-gnome ack-grep' | on_machine ${MACHINE}
    echo 'apt-get install -y build-essential' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-$(uname -r)' | on_machine ${MACHINE}
    echo 'apt-get install -y linux-headers-generic' | on_machine ${MACHINE}
    echo 'apt-get install -y netcat.traditional' | on_machine ${MACHINE}
    echo 'apt-get install -y git-core tig' | on_machine ${MACHINE}

    # for Dragonet
    echo 'apt-get install -y ghc cabal-install clang graphviz pdftk libghc-parsec2-dev libghc-stm-dev minisat' | on_machine ${MACHINE}


    echo 'cabal update' | on_machine_nosudo ${MACHINE}
    echo 'cabal install graphviz' | on_machine_nosudo ${MACHINE}
    echo 'cabal install pretty-show' | on_machine_nosudo ${MACHINE}
    echo 'cabal install MonadRandom' | on_machine_nosudo ${MACHINE}
    echo 'cabal install fgl pretty-show mtl random' | on_machine_nosudo ${MACHINE}

    # For simplified source code navigation
    echo 'apt-get install -y happy' | on_machine ${MACHINE}
    echo 'cabal install hscope' | on_machine_nosudo ${MACHINE}
    echo 'cabal install SourceGraph' | on_machine_nosudo ${MACHINE}

    # my vim configuration
    scp  "vimconf.tar" "${HOST}:"
    echo 'tar -xvf vimconf.tar' | on_machine_nosudo ${MACHINE}
    echo 'mkdir -p .vimbackup/backup' | on_machine_nosudo ${MACHINE}
    echo 'cd /root/ ; tar -xvf /home/ubuntu/vimconf.tar' | on_machine ${MACHINE}
    echo 'cd /root/ ; mkdir -p .vimbackup/backup' | on_machine ${MACHINE}
    echo 'mkdir bin' | on_machine_nosudo ${MACHINE}
    scp "~/bin/cs_create.sh" "${HOST}:bin/"
    #FIXME: Copy the ~/bin/ folder (or atleast useful part of it)
}

copy_code() {

    #echo "rm -rf .ssh/known_hosts" | on_machine_nosudo ${MACHINE}
    echo "rm -rf ${DPDK_SOURCE_DIR}" | on_machine ${MACHINE}
    echo 'git clone ssh://shindep@129.132.186.96:8006//home/shindep/git/dragonet -b openonload' | on_machine_nosudo ${MACHINE}
}

compile_dpdk() {
    echo "cd ${DPDK_SOURCE_DIR} ; make config T=x86_64-default-linuxapp-gcc" | on_machine ${MACHINE}
    echo "cd ${DPDK_SOURCE_DIR} ; make" | on_machine ${MACHINE}
}

compile_openonload() {
    # Compiling and installing SF driver which supports userspace networking
    echo "cd ${OPENONLOAD_SOURCE_DIR} ; ./scripts/onload_install " | on_machine ${MACHINE}
    # actually inserting the newly compiled and installed driver
    echo "cd ${OPENONLOAD_SOURCE_DIR} ; onload_tool reload " | on_machine ${MACHINE}
    # Reconfiguring the IP addresses of all interaces as SF interaces will be reset after insertion of new driver
    echo "bash ./setIPaddress.sh" | on_machine ${MACHINE}
    # Changing the owner back to normal user so that normal user can run build
    echo "cd ${OPENONLOAD_SOURCE_DIR} ; chown -R ubuntu.ubuntu ./build " | on_machine ${MACHINE}
}

install_haskell_dep() {

    echo "cd ${Dragonet_SOURCE_DIR} ; ./INSTALL.sh " | on_machine_nosudo ${MACHINE}
}



show_usage() {
        echo "Please specify the machine name"
        echo "Usage: ${0} -m <machineName> [-i -g -c]"
        echo "           -i -->  Installation of required packages"
        echo "           -c -->  copy the code to server"
        echo "           -d -->  Compile the DPDK code (only for dpdk)"
        echo "           -o -->  Compile the openonload code (only for openonload)"
        echo "           -h -->  Install haskell dependencies"
        echo "Examples: ${0} -m appenzeller -i -c"
        echo "Examples: ${0} -m ziger1 -d"
        exit 1
}

while getopts ":m:icdoh" opt; do
  case $opt in
    m)
        echo "-m was triggered, machine name: $OPTARG"
        MACHINE="$OPTARG"
      ;;
    i)
        echo "-i was triggered, setting for installation"
        INSTALL="yes"
      ;;
    c)
        echo "-c was triggered, copying the code"
        COPYCODE="yes"
      ;;
    d)
        echo "-d was triggered, setting for compilation of dpdk code"
        DPDK="yes"
      ;;
    o)
        echo "-o was triggered, setting for compilation of openonload code"
        OPENONLOAD="yes"
      ;;
    h)
        echo "-h was triggered, setting for installation of Haskell libraries"
        HASKELLLIB="yes"
      ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        show_usage
        exit 1
        ;;
    :)
        echo "Option -$OPTARG requires an argument." >&2
        show_usage
        exit 1
        ;;
  esac
done

if [ -z "${MACHINE}" ]; then
        show_usage
fi


# TODO: test if the HOST is pingable or not
HOST="ubuntu@${MACHINE}.in.barrelfish.org"
#MACHINE="10.110.5.34"
#HOST="ubuntu@${MACHINE}"
SSH="ssh ${HOST}"
DPDK_SOURCE_DIR="dragonet/dpdk-1.5.0r1/"
OPENONLOAD_SOURCE_DIR="dragonet/openonload-201310-u2/"
Dragonet_SOURCE_DIR="dragonet/Dragonet/"


#set -x
set -e

if [ "${INSTALL}" == "yes" ] ; then

    echo "Preparing system by installing required packages on remote ${HOST} machine"
    prepare_machine
else
    echo "Skipping installation of packages on remote ${HOST} machine"
fi


if [ "${COPYCODE}" == "yes" ] ; then
    echo "Doing git clone to copy the code on remote ${HOST} machine"
    copy_code
else
    echo "Skipping phase: code copy with git clone on remote ${HOST} machine"
fi

if [ "${DPDK}" == "yes" ] ; then
    echo "Compiling the DPDK code on remote ${HOST} machine"
    compile_dpdk
else
    echo "Skipping phase: compilation of DPDK on ${HOST} machine"
fi


if [ "${OPENONLOAD}" == "yes" ] ; then
    echo "Compiling the OPENONLOAD code on remote ${HOST} machine"
    compile_openonload
else
    echo "Skipping phase: compilation of OPENONLOAD on ${HOST} machine"
fi


if [ "${HASKELLLIB}" == "yes" ] ; then
    echo "Installing haskell dependencies remote ${HOST} machine"
    install_haskell_dep
else
    echo "Skipping phase: Haskell dependency installation on ${HOST} machine"
fi

echo "Done.."

