
#sudo cat >>/etc/apt/sources.list << EOF
#deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu precise main
#deb http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#deb-src http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#EOF

#wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo pt-key add -
#sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1E9377A2BA9EF27F


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



Z3LOCATION="/home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/"

install_base () {
## BASE: For both server and client ###################
sudo apt-get update
sudo apt-get install -y git build-essential vim
sudo apt-get install -y autoconf automake
sudo apt-get install -y libevent-dev
sudo apt-get install -y htop
}


install_server_deps() {
    # For haskell part of the Dragonet
    sudo apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev \
    libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev  \
    libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc   \
    llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 \
    python-clang-3.4 lldb-3.4-dev



    sudo apt-get install -y ghc cabal-install zlib1g-dev g++-4.6 happy
    sudo apt-get install -y ghc*-prof

    cd ${MYBASE}

    cabal update
    # NOTE: Using specific version of the cabal to avoid conflict with llvm-gen
    # https://github.com/bscarlet/llvm-general/issues/122
    #cabal install cabal-install
    cabal install cabal-install-1.20.0.6
}


install_client_deps() {
##### for client side ####################

sudo apt-get install -y autoconf automake
sudo apt-get install -y makeinfo

check_dir_exists  ${MYBASE}/dragonet/benchmarking/netperf-2.6.0 "netperf codebase"
cd ${MYBASE}/dragonet/benchmarking/netperf-2.6.0
./configure_compile.sh
make
sudo make install

# Creating symbolic links as this brain-dead tool wants exactly 1.13 version of both.
sudo ln -s /usr/bin/aclocal /usr/bin/aclocal-1.13
sudo ln -s /usr/bin/automake /usr/bin/automake-1.13

check_dir_exists  ${MYBASE}/dragonet/benchmarking/libmemcached-1.0.18 "memaslap codebase"
cd ../libmemcached-1.0.18/
./configure_install_dragonet.sh

cd ${MYBASE}/dragonet/benchmarking/dstat
sudo make install

sudo apt-get install -y ethtool


#automake --add-missing

# Avoiding as installing this takes around 1G space!!!
#sudo apt-get install asciidoc
#cd ${MYBASE}/dragonet/benchmarking/dstat
#sudo make install

# FIXME: Currently not present in pipelines-02 branch
#cd ${MYBASE}/dragonet/benchmarking/latencyBench/
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

get_repository () {
    cd ${MYBASE}
    if [[ -d "dragonet" ]] ; then
        echo "dragonet already exists. You may want to delete it, or run without -g option" ;
        exit 1
    fi
    git clone /cdrom/casper/mount/repository/dragonet
    cd dragonet
    git checkout master
    #git checkout execmodel
    #git checkout kkourt-nsdi15
}


install_dpdk() {
    check_dir_exists ${MYBASE}/dragonet/dpdk-1.7.1/ "dpdk codebase"
    cd ${MYBASE}/dragonet/dpdk-1.7.1/
    ./doConfig.sh
}

install_openonload() {
    check_dir_exists ${MYBASE}/dragonet/openonload-201310-u2/ "onload codebase"
    cd ${MYBASE}/dragonet/openonload-201310-u2/
    sudo ./scripts/onload_install
    sudo onload_tool reload
    sudo cp build/gnu_x86_64/lib/ciul/libciul.so.1.1.1 /lib/

    cd ${MYBASE}
    #setIPaddress.sh
}

clean_prepare_Dragonet() {

    check_dir_exists  ${MYBASE}/dragonet/Dragonet/ "Dragonet codebase"
    cd ${MYBASE}/dragonet/Dragonet/

    export PATH="${HOME}/.cabal/bin:$PATH"

    echo "Cleaning up old installation"
    rm -rf ./dist
    rm -rf .cabal_sandbox

    echo "Preparing sandbox"
    ./prepare_sandbox.sh

    check_dir_exists ${Z3LOCATION}  "z3 installation location"

    cabal install z3 --extra-include-dirs=${Z3LOCATION}/include/ --extra-lib-dirs=${Z3LOCATION}/bin/
}


install_Dragonet() {

    check_dir_exists  ${HOME}/.cabal/bin  "Local cabal installation"
    check_file_exists  ${HOME}/.cabal/bin/cabal  "local cabal binary"

    export PATH="${HOME}/.cabal/bin:$PATH"
    clean_prepare_Dragonet
    cabal build bench-fancyecho stack-dpdk stack-sf
}


install_memcached() {
    cd ${MYBASE}/dragonet/benchmarking/memcached/
    ./autogen.sh && ./configure && make memcached
}


copy_and_install_z3() {
    cd  "${MYBASE}"
    cp -r /cdrom/casper/mount/bin/z3Solver/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/ .
    Z3LOCATION="${MYBASE}/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/"
    link_z3
}


link_z3() {

    check_file_exists ${Z3LOCATION}/bin/z3 "z3 executable"
    check_file_exists ${Z3LOCATION}/bin/libz3.so "z3 library"

    cd /usr/bin/
    sudo ln -s ${Z3LOCATION}/bin/z3 z3
    sudo cp ${Z3LOCATION}/bin/libz3.so /lib/x86_64-linux-gnu/
#    install_Dragonet
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
        echo "           -s -->  install server dependncy packages"
        echo "           -c -->  install client dependncy packages, and tools"
        echo "           -z -->  Copy z3 from ETHZ NFS"
        echo "           -l -->  link and compile DN with z3"
        echo "           -d -->  install dpdk"
        echo "           -o -->  install onload"
        echo "           -S -->  Compile and Install server side of dragonet"
        echo "Examples (clone repo and z3 on eth network): ${0} -g -z"
        echo "Examples (installing server): ${0} -i -s -d -o -S"
        echo "Examples (installing client):: ${0} -i -c"
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

MYBASE="$HOME"

if [ "${INSTALLT}" == "yes" ] ; then
install_base
fi

if [ "${CLONE}" == "yes" ] ; then
    echo "cloning Dragonet repository"
    get_repository
else
    echo "Skipping cloning of Dragonet repository"
fi

if [ "${CLIENTDEPS}" == "yes" ] ; then
    echo "Installing clientside"
    install_client_deps
else
    echo "Skipping Installing clientside"
fi

if [ "${SERVERDEPS}" == "yes" ] ; then
    echo "Installing server deps"
    install_server_deps
else
    echo "Skipping Installing server deps"
fi


if [ "${OPENONLOAD}" == "yes" ] ; then
    echo "Installing openonload driver"
    install_openonload
else
    echo "Skipping Installing openonload driver"
fi

if [ "${DPDK}" == "yes" ] ; then
    echo "Installing dpdk driver"
    install_dpdk
else
    echo "Skipping Installing dpdk driver"
fi

if [ "${Z3SETUP}" == "yes" ] ; then
    echo "Installing z3 setup"
    copy_and_install_z3
else
    echo "Skipping Installing z3 setup"
fi

if [ "${Z3LINK}" == "yes" ] ; then
    echo "Linking z3 and compiling dragonet with it"
    link_z3
else
    echo "Linking z3 and compiling it"
fi

if [ "${SERVERCOMPILE}" == "yes" ] ; then
    echo "Compiling Dragonet server side"
    install_server_related
else
    echo "Skipping Compiling Dragonet server side"
fi

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

if [ "${CABALCLEANPREPARE}" == "yes" ] ; then
    clean_prepare_Dragonet
fi

if [ "${VIMSETUP}" == "yes" ] ; then
    echo "Copying vim setup"
    #setup_vim_pravin
    setup_work_env_pravin
fi

