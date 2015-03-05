cat >>/etc/apt/sources.list << EOF
deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu precise main
deb http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
deb-src http://llvm.org/apt/precise/ llvm-toolchain-precise-3.4 main
EOF

wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | apt-key add -
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1E9377A2BA9EF27F
apt-get update
apt-get dist-upgrade -y
apt-get install -y clang-3.4 clang-3.4-doc libclang-common-3.4-dev libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 python-clang-3.4 lldb-3.4-dev
apt-get install -y git ghc cabal-install zlib1g-dev g++-4.6 happy make
cabal update
cabal install cabal-install
export PATH="/root/.cabal/bin:$PATH"

git clone ssh://antoinek@sgd-dalcoi5-25.ethz.ch/home/antoinek/dragonet_pipelines

cd dragonet_pipelines/benchmarking/netperf-2.6.0
./configure_compile.sh
make install

cd ../../Dragonet
./prepare_sandbox.sh

cabal build -j
#read
#rm -f /dev/shm/dragonet*
#rm -f /dev/shm/bulk_pool_*
#dist/build/llvm-cgen/llvm-cgen lpgImpl.unicorn
