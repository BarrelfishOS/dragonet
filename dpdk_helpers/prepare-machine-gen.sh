
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


get_repository () {

MYBASE="/home/ubuntu/"
cd ${MYBASE}
#git clone ssh://shindep@129.132.186.96:8006//home/shindep/git/dragonet
git clone /cdrom/casper/mount/repository/dragonet
cd dragonet
git checkout pipelines-02

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

# Creating symbolic links as this brain-dead tool wants evactly 1.13 version of both.
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

install_base
get_repository
install_server_deps
install_client_deps
install_useful_tools

