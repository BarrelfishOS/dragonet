#!/bin/bash
set -x
set -e

sudo apt-get update
sudo apt-get install htop

mkdir -p mnt
sudo mount /dev/sda3 ./mnt
ln -s ~/mnt/home/pravin/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10 z3-4.3.2.24961dc5f166-x64-ubuntu-13.10

sudo ln -s /home/ubuntu/mnt/home/pravin/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/z3 /bin/z3
sudo cp ~/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/libz3.so /lib/x86_64-linux-gnu/

ln -s ~/mnt/home/pravin pravin
ln -s ~/pravin/dragonet dragonet

cd pravin/session_history
cp .bashrc .screenrc  .gitconfig .htoprc .bash_history ~/
cp .vimrc ~/
cp -r .vim ~/
cd ~
mkdir ~/.vimbackup/

cd ~
source .bashrc

echo "Here are the things that you may want to do now!!!"
echo "
prepare-machine-gen.sh -i -s -d -o
setIPaddress.sh
cd dragonet/Dragonet
./cleanCompile.sh

# for dpdk
cd ../dpdk-1.7.1
sudo ./tools/setup.sh dragonet

# for memcached
cd ../benchmarking/memcached
make clean
./autogen.sh && ./configure && make memcached -j1

# verify that everything is fine
cd ../../Dragonet

./scripts/pravin/deployDragonetGen.sh fg sf 4 priority greedy 0 0
"

