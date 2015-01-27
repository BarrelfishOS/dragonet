
#sudo cat >>/etc/apt/sources.list << EOF
#deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu precise main
#deb http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#deb-src http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
#EOF

#wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo pt-key add -
#sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1E9377A2BA9EF27F

install_base () {
## BASE: For both server and client ###################
sudo apt-get update
sudo apt-get install -y git build-essential vim
sudo apt-get install -y autoconf automake
sudo apt-get install -y libevent-dev
}



install_server_deps() {
## For server:  ###################

#sudo apt-get dist-upgrade -y
sudo apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 python-clang-3.4 lldb-3.4-dev

sudo apt-get install -y ghc cabal-install zlib1g-dev g++-4.6 happy
cabal update
cabal install cabal-install
export PATH="${HOME}/.cabal/bin:$PATH"


cd ${MYBASE}/dragonet/dpdk-1.5.0r1/
./doConfig.sh
make

cd ${MYBASE}/dragonet/Dragonet/
./prepare_sandbox.sh
cabal build -j

cd ${MYBASE}/dragonet/benchmarking/memcached/
./autogen.sh && ./configure && make memcached
}

install_client_deps() {
##### for client side ####################

sudo apt-get install -y autoconf automake
sudo apt-get install -y makeinfo

cd ${MYBASE}/dragonet/benchmarking/netperf-2.6.0
./configure_compile.sh
make
sudo make install

# Creating symbolic links as this brain-dead tool wants exactly 1.13 version of both.
sudo ln -s /usr/bin/aclocal /usr/bin/aclocal-1.13
sudo ln -s /usr/bin/automake /usr/bin/automake-1.13

cd ../libmemcached-1.0.18/
./configure_install_dragonet.sh
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


old_installation() {
    install_base
#    get_repository
#    install_server_deps
#    install_client_deps
#    setup_vim_pravin
#    install_useful_tools
}

install_new_tools() {
    sudo apt-get update
    sudo apt-get install -y htop
}


get_repository () {
    install_new_tools
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
    cd ${MYBASE}/dragonet/dpdk-1.7.1/
    ./doConfig.sh
}

install_openonload() {
    cd ${MYBASE}/dragonet/openonload-201310-u2/
    sudo ./scripts/onload_install
    sudo onload_tool reload
    sudo cp build/gnu_x86_64/lib/ciul/libciul.so.1.1.1 /lib/

    cd ${MYBASE}
    setIPaddress.sh
}

clean_prepare_Dragonet() {
    cd ${MYBASE}/dragonet/Dragonet/

    export PATH="${HOME}/.cabal/bin:$PATH"

    echo "Cleaning up old installation"
    rm -rf ./dist
    rm -rf .cabal_sandbox

    echo "Preparing sandbox"
    ./prepare_sandbox.sh

    # FIXME: make sure that the z3 library exists

    cabal install z3 --extra-include-dirs=/home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/include/ --extra-lib-dirs=/home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/
}


install_Dragonet() {

sudo apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 python-clang-3.4 lldb-3.4-dev

sudo apt-get install -y ghc cabal-install zlib1g-dev g++-4.6 happy
sudo apt-get install -y ghc*-prof

    cd ${MYBASE}

    cabal update
    cabal install cabal-install
    export PATH="${HOME}/.cabal/bin:$PATH"

    clean_prepare_Dragonet

    #cabal build llvm-cgen llvm-cgen-e10k bench-echo llvm-cgen-sf
    cabal build bench-fancyecho stack-dpdk stack-sf stack-e10k
}


install_memcached() {
    cd ${MYBASE}/dragonet/benchmarking/memcached/
    ./autogen.sh && ./configure && make memcached
}

install_memcached_client() {
    cd ${MYBASE}/dragonet/benchmarking/libmemcached-1.0.18/
    ./configure_install_dragonet.sh
}

install_others() {
    cd ${MYBASE}/dragonet/benchmarking/netperf-2.6.0
    ./configure_compile.sh
    make
    sudo make install
    cd ${MYBASE}/dragonet/benchmarking/dstat
    sudo make install
}

install_z3_related() {
    cd ~/
    cp -r /cdrom/casper/mount/bin/z3Solver/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/ .
    link_and_compile_z3
}

link_and_compile_z3() {
    cd /usr/bin/
    sudo ln -s /home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/z3 z3
    sudo cp ~/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/libz3.so /lib/x86_64-linux-gnu/

    install_Dragonet
}

install_server_related() {
    #install_Dragonet
    install_z3_related
    install_memcached
}

install_client_related() {
    install_memcached_client
    install_others
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
        echo "           -g -->  clone the git repository"
        echo "           -i -->  install all tools"
        echo "           -d -->  install dpdk"
        echo "           -o -->  install onload"
        echo "           -c -->  compile and install client side of dragonet"
        echo "           -C -->  Clean and re-prepare Dragonet cabal setup"
        echo "           -s -->  compile and install server side of dragonet"
        echo "           -v -->  setup vim configuration (pravin's configuration)"
        echo "           -z -->  install z3 related"
        echo "           -l -->  Only link and compile DN with z3"
        echo "           -p -->  install spark deps"
        echo "           -r -->  refresh NFS mount for spark"
        echo "Examples (installing everything): ${0} -g -d -o -c -s"
        echo "Examples (installing minimal client):: ${0} -g -c"
        exit 1
}

while getopts ":igcsdovzprlC" opt; do
  case $opt in
    i)
        echo "-i was triggered, installing all tools"
        INSTALLT="yes"
        ;;
    g)
        echo "-g was triggered, cloning git repo"
        CLONE="yes"
      ;;
    c)
        echo "-c was triggered, compiling client side"
        CLIENTCOMPILE="yes"
      ;;
    s)
        echo "-s was triggered, compiling server side"
        SERVERCOMPILE="yes"
      ;;
    d)
        echo "-d was triggered, setting for compilation of dpdk code"
        DPDK="yes"
      ;;
    C)
        echo "-C Clean preparing cabal installation for Dragonet "
        CABALCLEANPREPARE="yes"
      ;;
    o)
        echo "-o was triggered, setting for compilation of openonload code"
        OPENONLOAD="yes"
      ;;
    v)
        echo "-v was triggered, setting up pravin's vim configuration"
        VIMSETUP="yes"
      ;;
    p)
        echo "-p was triggered, setting up spark deps"
        SPARKSETUP="yes"
      ;;
    r)
        echo "-r was triggered, refreshing spark nfs mount by remounting it"
        REFRESHNFS="yes"
      ;;
    z)
        echo "-z was triggered, setting up z3"
        Z3SETUP="yes"
      ;;
    l)
        echo "-z was triggered, setting up z3"
        Z3LINKCOMPILE="yes"
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
install_new_tools
fi

if [ "${CLONE}" == "yes" ] ; then
    echo "cloning Dragonet repository"
    get_repository
else
    echo "Skipping cloning of Dragonet repository"
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

if [ "${Z3SETUP}" == "yes" ] ; then
    echo "Installing z3 setup"
    install_z3_related
else
    echo "Skipping Installing z3 setup"
fi

if [ "${Z3LINKCOMPILE}" == "yes" ] ; then
    echo "Linking z3 and compiling dragonet with it"
    link_and_compile_z3
else
    echo "Linking z3 and compiling it"
fi

if [ "${CABALCLEANPREPARE}" == "yes" ] ; then
    clean_prepare_Dragonet
fi


if [ "${CLIENTCOMPILE}" == "yes" ] ; then
    echo "Compiling Dragonet client side"
    install_client_related
else
    echo "Skipping Compiling Dragonet client side"
fi

if [ "${VIMSETUP}" == "yes" ] ; then
    echo "Copying vim setup"
    #setup_vim_pravin
    setup_work_env_pravin
fi

