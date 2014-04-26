
# Command to deploy memcached without dragonet
./memcached -p 0 -U 7777 -t 1 -l 127.0.0.1 -u pravin
# Command to deploy memcached with dragonet
cd ../../Dragonet/ ; ./myRun.sh
sudo ./memcached -N -p 0 -U 7 -t 1 -l 192.168.123.1 -u pravin

