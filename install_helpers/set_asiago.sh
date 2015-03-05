#!/bin/bash
set -x
set -e
mkdir -p mnt
sudo mount /dev/sda3 ./mnt
mv dragonet dragonet_live
ln -s ~/mnt/home/pravin/dragonet dragonet
ln -s ~/mnt/home/pravin/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10 z3-4.3.2.24961dc5f166-x64-ubuntu-13.10

sudo ln -s /home/ubuntu/mnt/home/pravin/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/z3 /bin/z3
sudo cp ~/z3-4.3.2.24961dc5f166-x64-ubuntu-13.10/bin/libz3.so /lib/x86_64-linux-gnu/

cd dragonet/session_history
cp .bashrc .screenrc  .gitconfig .htoprc .bash_history ~/
cd ~
echo "start new bash shell and then create a screen session now"

