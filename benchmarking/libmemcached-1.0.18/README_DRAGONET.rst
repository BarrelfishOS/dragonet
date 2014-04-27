Installation
----------------

You can install the benchmarking tool by running the script
``configure_install_dragonet.sh`` from this directory ::

    configure_install_dragonet.sh


Running benchmark
----------------------

Sample command to test the memcached running on localhost on port 7777 ::

    memaslap  -s 127.0.0.1:7777 --udp -x1000000  --cfg_cmd=./bmKey_64_val_1024.conf -T4 -b -S10m

