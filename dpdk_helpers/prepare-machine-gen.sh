
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
    #git checkout execmodel
    git checkout nsdi15
}


install_dpdk() {
    cd ${MYBASE}/dragonet/dpdk-1.5.0r1/
    ./doConfig.sh
    make
}

install_openonload() {
    cd ${MYBASE}/dragonet/openonload-201310-u2/
    sudo ./scripts/onload_install
    sudo onload_tool reload
    sudo cp build/gnu_x86_64/lib/ciul/libciul.so.1.1.1 /lib/

    cd ${MYBASE}
    setIPaddress.sh
}

install_Dragonet() {

sudo apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 python-clang-3.4 lldb-3.4-dev

sudo apt-get install -y ghc cabal-install zlib1g-dev g++-4.6 happy

    cd ${MYBASE}
    # copy cabal installation from root if you are not root
    if [[ "$HOME" == "/root" ]]; then
        echo "installing as root user, so skipping cabal-install as it should be already there!"
    else
        cabal update
        cabal install cabal-install
        export PATH="${HOME}/.cabal/bin:$PATH"
    fi

    cd ${MYBASE}/dragonet/Dragonet/
    ./prepare_sandbox.sh
    cabal build llvm-cgen llvm-cgen-e10k bench-echo llvm-cgen-sf
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
    cd /usr/bin/
    sudo ln -s /home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/z3 z3
    sudo cp ~/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/libz3.so /lib/x86_64-linux-gnu/

    cd ~/dragonet/Dragonet
    cabal install z3 --extra-include-dirs=/home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/include/ --extra-lib-dirs=/home/ubuntu/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/
}

install_server_related() {
    install_Dragonet
    install_memcached
}

install_client_related() {
    install_memcached_client
    install_others
}


show_usage() {
        echo "Please specify what you want to install"
        echo "Usage: ${0} [-g -s -c -o -d -v]"
        echo "           -g -->  clone the git repository"
        echo "           -d -->  install dpdk"
        echo "           -o -->  install onload"
        echo "           -c -->  compile and install client side of dragonet"
        echo "           -s -->  compile and install server side of dragonet"
        echo "           -v -->  setup vim configuration (pravin's configuration)"
        echo "Examples (installing everything): ${0} -g -d -o -c -s"
        echo "Examples (installing minimal client):: ${0} -g -c"
        exit 1
}

while getopts ":gcsdov" opt; do
  case $opt in
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
    o)
        echo "-o was triggered, setting for compilation of openonload code"
        OPENONLOAD="yes"
      ;;
    v)
        echo "-v was triggered, setting up pravin's vim configuration"
        VIMSETUP="yes"
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

if [ "${CLIENTCOMPILE}" == "yes" ] ; then
    echo "Compiling Dragonet client side"
    install_client_related
else
    echo "Skipping Compiling Dragonet client side"
fi

if [ "${VIMSETUP}" == "yes" ] ; then
    echo "Copying vim setup"
    setup_vim_pravin
fi

