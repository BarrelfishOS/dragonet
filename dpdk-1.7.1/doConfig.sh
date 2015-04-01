TF=''
TF='TOOLCHAIN_CFLAGS=-msse4'

make $TF config T=x86_64-native-linuxapp-gcc -j20
make $TF T=x86_64-native-linuxapp-gcc -j20

# No need to use these, see Dragonet.cabal
#echo "`pwd`/build/lib/"  | sudo tee /etc/ld.so.conf.d/dpdk.conf
#sudo rm /etc/ld.so.cache
#sudo ldconfig

