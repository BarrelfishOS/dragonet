#!/bin/bash
#
# Dragonet setup helper script
#

#sudo cat >>/etc/apt/sources.list << EOF
#deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu precise main
#deb http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#deb-src http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#EOF

#wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo pt-key add -
#sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1E9377A2BA9EF27F

[ -z ${DRAGONET_BASE} ]      && DRAGONET_BASE="$HOME"
[ -z ${Z3LOCATION}    ]      && Z3LOCATION="${DRAGONET_BASE}/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10"
[ -z ${DRAGONET_CABAL_DIR} ] && DRAGONET_CABAL_DIR="${DRAGONET_BASE}/dragonet-cabal"

DRAGONET_SOURCE="${DRAGONET_BASE}/dragonet"
DRAGONET_GIT="/cdrom/casper/mount/repository/dragonet"

SYSTEM_CABAL=cabal
DRAGONET_CABAL=${DRAGONET_CABAL_DIR}/bin/cabal

DRAGONET_DPDK=$DRAGONET_SOURCE/dpdk-1.7.1
DRAGONET_ONLOAD=$DRAGONET_SOURCE/openonload-201310-u2

echo "Dragonet base   : $DRAGONET_BASE"
echo "Dragonet source : $DRAGONET_SOURCE"

check_file_exists(){
  local message
  if [ -z "$1" ]
  then
    echo "-Parameter #1 is missing. Please try again.-"  # Or no parameter passed.
    exit 1
  fi
  if [ -z "$2" ]
  then
    message="File"
  else
    message=$2
  fi

  if [ ! -f $1 ]
  then
    echo "$message $1 does not exist"
    exit 1
  fi
}

check_file_executable(){
  local message
  if [ -z "$1" ]
  then
    echo "-Parameter #1 is missing. Please try again.-"  # Or no parameter passed.
    exit 1
  fi
  if [ -z "$2" ]
  then
    message="File"
  else
    message=$2
  fi

  if [ ! -x $1 ]
  then
    echo "$message $1 does not have execute permission"
    exit 1
  fi
}

check_dir_exists(){
  local message
  if [ -z "$1" ]
  then
    echo "-Parameter #1 is missing. Please try again.-"  # Or no parameter passed.
    exit 1
  fi
  if [ -z "$2" ]
  then
    message="Directory"
  else
    message=$2
  fi

  if [ ! -d $1 ]
  then
    echo "$message $1 does not exist"
    exit 1
  fi
}

do_if_yes() {
    [ -z "$3" ] && echo "do_if_yes requires 3 arguments" && exit 1
    if [ "$1" == "yes" ]; then
       echo "DO  : $2"
       $3
    else
       echo "SKIP: $2"
    fi
}

install_base () {
## BASE: For both server and client ###################
   sudo apt-get update
   # KK: removed vim, htop, and libevent-dev
   sudo apt-get install -y git build-essential autoconf automake
}

install_dragonet_cabal() {
    # Dragonet-specific cabal-install
    # NOTE: Using specific version of the cabal to avoid conflict with llvm-gen
    # https://github.com/bscarlet/llvm-general/issues/122
    ${SYSTEM_CABAL} update
    ${SYSTEM_CABAL} install --prefix=${DRAGONET_CABAL_DIR} cabal-install-1.20.0.6
    if [ $? != 0 ]; then
        echo "Dragonet cabal-install installation failed"
        exit 1
    fi

    check_dir_exists ${Z3LOCATION} "z3 installation location"
    link_z3

    echo "Preparing sandbox"
    #export PATH="${HOME}/.cabal/bin:$PATH"
    check_dir_exists  ${DRAGONET_SOURCE}/Dragonet/ "Dragonet codebase"
    cd ${DRAGONET_SOURCE}/Dragonet/
    PATH="${DRAGONET_CABAL_DIR}/bin":$PATH ./prepare_sandbox.sh
}


install_server_packets() {
    sudo apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev \
    libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev  \
    libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc   \
    llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 \
    python-clang-3.4 lldb-3.4-dev

    sudo apt-get install -y ghc cabal-install zlib1g-dev g++-4.6 happy
    sudo apt-get install -y ghc*-prof
}

install_server_deps() {
    install_base
    install_server_packets
    install_dragonet_cabal
}


install_client_deps() {
##### for client side ####################

sudo apt-get install -y autoconf automake
sudo apt-get install -y makeinfo

check_dir_exists  ${DRAGONET_SOURCE}/benchmarking/netperf-2.6.0 "netperf codebase"
cd ${DRAGONET_SOURCE}/benchmarking/netperf-2.6.0
./configure_compile.sh
make
sudo make install

# Creating symbolic links as this brain-dead tool wants exactly 1.13 version of both.
sudo ln -s /usr/bin/aclocal /usr/bin/aclocal-1.13
sudo ln -s /usr/bin/automake /usr/bin/automake-1.13

check_dir_exists  ${DRAGONET_SOURCE}/benchmarking/libmemcached-1.0.18 "memaslap codebase"
cd ../libmemcached-1.0.18/
./configure_install_dragonet.sh

cd ${DRAGONET_SOURCE}/benchmarking/dstat
sudo make install

sudo apt-get install -y ethtool


#automake --add-missing

# Avoiding as installing this takes around 1G space!!!
#sudo apt-get install asciidoc
#cd ${DRAGONET_SOURCE}/benchmarking/dstat
#sudo make install

# FIXME: Currently not present in pipelines-02 branch
#cd ${DRAGONET_SOURCE}/benchmarking/latencyBench/
#make compile

}




setup_work_env_pravin() {
    cd ${HOME}
    for f in  .bash_history  .bashrc  .gitconfig  .htoprc  .screenrc
    do
        cp /cdrom/casper/mount/bin/session_history/$f .
    done
    cp /cdrom/casper/mount/bin/session_history/.vimrc .
    cp -r /cdrom/casper/mount/bin/session_history/.vim .
}



setup_vim_pravin() {
    cd ${HOME}
    tar -xvf /cdrom/casper/mount/bin/vimconf.tar
    mkdir -p .vimbackup/backup
    sudo bash -c 'cd /root/ ; tar -xvf  /cdrom/casper/mount/bin/vimconf.tar'
    sudo mkdir -p /root/.vimbackup/backup
}

install_useful_tools() {
    sudo apt-get install -y screen byobu tree vim ctags cscope vim-gnome ack-grep
    sudo apt-get install -y build-essential
    sudo apt-get install -y ia32-libs
    sudo apt-get install -y gcc-multilib libc6-dev-i386
    sudo apt-get install -y python-dev
    sudo apt-get install -y linux-headers-$(uname -r)
    sudo apt-get install -y linux-headers-generic
    sudo apt-get install -y netcat.traditional
    sudo apt-get install -y socat
    sudo apt-get install -y dstat
    sudo apt-get install -y netperf iperf
    sudo apt-get install -y git-core tig
    sudo apt-get install -y ethtool
    sudo apt-get install -y  chkconfig
    # for compiling latest dstat dstat
    sudo apt-get install -y  asciidoc
    # for netperf-wrapper
    sudo apt-get install -y  python-matplotlib
    sudo apt-get install -y  python-numpy
}

clone_repository () {
    if [[ -d ${DRAGONET_SOURCE} ]] ; then
        echo "Dragonet source (${DRAGONET_GIT}) already exists. You may want to delete it, or run without -g option";
        exit 1
    fi
    git clone ${DRAGONET_GIT} ${DRAGONET_SOURCE}
    cd ${DRAGONET_SOURCE}
    git checkout master
}


install_dpdk() {
    check_dir_exists ${DRAGONET_DPDK} "dpdk codebase"
    cd ${DRAGONET_DPDK}
    ./doConfig.sh
}

install_openonload() {
    # Note: There seems to be some capability for onload to be installed on a
    # separate directory by setting $i_prefix. However, this might not work
    # well with things like /etc/modprobe.d/*.conf for configuring the onload
    # kernel modules. Having to handle this seems not trivial, so we use sudo
    # to install it on /
    check_dir_exists ${DRAGONET_ONLOAD} "onload codebase"
    cd ${DRAGONET_ONLOAD}
    sudo ./scripts/onload_install
    sudo onload_tool reload

    # No need to use this, see Dragonet.cabal
    #sudo cp build/gnu_x86_64/lib/ciul/libciul.so.1.1.1 /lib/
}


install_Dragonet() {

    check_file_exists  $DRAGONET_CABAL "Dragonet cabal file"
    check_dir_exists  ${DRAGONET_SOURCE} "Dragonet codebase"

    cd $DRAGONET_SOURCE/Dragonet
    $DRAGONET_CABAL build bench-fancyecho
    $DRAGONET_CABAL build stack-e10k-base
    [ -f $DRAGONET_DPDK/build/lib/libintel_dpdk.so ] && $DRAGONET_CABAL build stack-e10k-dpdk
    [ -f $DRAGONET_ONLOAD/build/gnu_x86_64/lib/ciul/libciul.so ] && $DRAGONET_CABAL build stack-sf
}


install_memcached() {
    cd ${DRAGONET_SOURCE}/benchmarking/memcached/
    ./autogen.sh && ./configure && make memcached
}


copy_and_install_z3() {
    cp -av /cdrom/casper/mount/bin/z3Solver/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10 ${Z3LOCATION}
    link_z3
}


link_z3() {

    check_file_exists ${Z3LOCATION}/bin/z3 "z3 executable"
    check_file_exists ${Z3LOCATION}/bin/libz3.so "z3 library"
    check_dir_exists  ${DRAGONET_SOURCE} "Dragonet codebase"

    # link it to the source so that cabal can find it
    # see Dragonet.cabal
    rm -f ${DRAGONET_SOURCE}/z3
    ln -s ${Z3LOCATION} $DRAGONET_SOURCE/z3

    # No need to add it globally. See: Dragonet.cabal
    #cd /usr/bin/
    #sudo ln -s ${Z3LOCATION}/bin/z3 z3
    #sudo cp ${Z3LOCATION}/bin/libz3.so /lib/x86_64-linux-gnu/
}

install_server_related() {
    install_Dragonet
    install_memcached
}


install_spark_related() {
    #sudo apt-get update
    sudo apt-get install -y openjdk-7-jre-headless scala openjdk-7-jdk
    cd ${HOME}
    #tar -xf /cdrom/casper/mount/spark/spark-1.1.0-bin-hadoop2.4.tgz
    install_spark_nfs
}

refresh_nfs() {
    sudo umount "${HOME}/spark"
    sudo mount -t nfs -o proto=tcp,port=2049  10.110.4.4:/local/nfs/pravin/spark/spark-1.1.0   ./spark
}


install_spark_nfs() {
    sudo apt-get -o Dpkg::Options::="--force-confnew" install -y nfs-common
    mkdir -p "${HOME}/spark"
    sudo mount -t nfs -o proto=tcp,port=2049  10.110.4.4:/local/nfs/pravin/spark/spark-1.1.0   ./spark
}

show_usage() {
        echo "Please specify what you want to install"
        echo "Usage: ${0} [-g -s -c -o -d -v -i]"
        echo "           -g -->  clone the git repository over ETHZ NFS"
        echo "           -i -->  install all base tools"
        echo "           -s -->  install server dependencies"
        echo "           -c -->  install client dependencies"
        echo "           -z -->  Copy z3 from ETHZ NFS"
        echo "           -l -->  link and compile DN with z3"
        echo "           -d -->  install dpdk"
        echo "           -o -->  install onload"
        echo "           -S -->  Compile and Install server side of dragonet"
        echo "Examples (clone repo and z3 on eth network): ${0} -g -z"
        echo "Examples (installing server): ${0} -i -s -d -o -S"
        echo "Examples (installing client): ${0} -i -c"
        exit 1
}

while getopts ":gisczldoSCv" opt; do
  case $opt in
    g)
        echo "-g was triggered, cloning git repo"
        CLONE="yes"
      ;;
    i)
        echo "-i was triggered, install base tools"
        INSTALLT="yes"
        ;;
    c)
        echo "-c was triggered, installing clientside tools"
        CLIENTDEPS="yes"
      ;;
    s)
        echo "-s was triggered, compiling server side"
        SERVERDEPS="yes"
      ;;
    S)
        echo "-S was triggered, Compiling server side of Dragonet"
        SERVERCOMPILE="yes"
      ;;
    d)
        echo "-d was triggered, setting for compilation of dpdk code"
        DPDK="yes"
      ;;
    o)
        echo "-o was triggered, setting for compilation of openonload code"
        OPENONLOAD="yes"
      ;;
    v)
        echo "-v was triggered, setting up pravin's vim configuration"
        VIMSETUP="yes"
      ;;
    z)
        echo "-z was triggered, setting up z3"
        Z3SETUP="yes"
      ;;
    l)
        echo "-l was triggered, linking z3"
        Z3LINK="yes"
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

do_if_yes "$INSTALLT" "Install base packages" install_base
do_if_yes "$CLONE" "CLone Dragonet git repo"  clone_repository
do_if_yes "$CLIENTDEPS" "Install client packages" install_client_deps
do_if_yes "$SERVERDEPS" "Install server packages" install_server_deps
do_if_yes "$OPENONLOAD" "Install OpenOnload driver" install_openonload
do_if_yes "$DPDK" "Install DPDK driver" install_dpdk
do_if_yes "$Z3SETUP" "Install Z3" copy_and_install_z3
do_if_yes "$SERVERCOMPILE" "Compiler Dragonet stack/server programs"  install_server_related
do_if_yes "$Z3LINK"  "Link Z3" link_z3


if [ "${SPARKSETUP}" == "yes" ] ; then
    echo "Installing spark setup"
    install_spark_related
else
    echo "Skipping Installing z3 setup"
fi


if [ "${REFRESHNFS}" == "yes" ] ; then
    echo "Refreshing Spark NFS mount"
    refresh_nfs
else
    echo "Skipping Refreshing Spark NFS mount"
fi

if [ "${VIMSETUP}" == "yes" ] ; then
    echo "Copying vim setup"
    #setup_vim_pravin
    setup_work_env_pravin
fi
