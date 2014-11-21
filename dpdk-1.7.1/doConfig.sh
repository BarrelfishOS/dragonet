make config T=x86_64-native-linuxapp-gcc -j20
make T=x86_64-native-linuxapp-gcc -j20
echo "`pwd`/build/lib/"  | sudo tee /etc/ld.so.conf.d/dpdk.conf
sudo rm /etc/ld.so.cache
sudo ldconfig

