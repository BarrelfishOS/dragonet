
Prerequisites
--------------

You need to build the Dragonet network stack first before you can compile
memcached as it depends on Dragonet libraries.  You also need to build
``llvm-cgen`` if you want to use tuntap driver. It can be done with following
commands ::

    cd ../../Dragonet/
    ./prepare_sandbox.sh
    cabal build bench-echo
    cabal build llvm-cgen

Or you can build everything with ``cabal build`` if you have time.

Compilation
--------------

Memcached with Dragonet support can be compiled with following command ::

    ./autogen.sh && ./configure && make memcached -j1

Note: In case there are any modifications in application interface of Dragonet
then it is important to run ``./prepare_sandbox.sh`` as it will compile the
``LibHSApps`` needed by memcached.


Deployment
--------------

Command to deploy memcached without dragonet ::
    ./memcached -p 0 -U 7777 -t 1 -l 127.0.0.1 -u pravin

Deployment of memcached with Dragonet depends on having Dragonet network
stack running.  The ``myRun.sh`` script deploys the Dragonet stack on ``tuntap``
device.  Note that you need to run fresh Dragonet stack for each run of memcached.

Command to deploy memcached with dragonet ::
    cd ../../Dragonet/ ; ./deployDragonetNS.sh
    sudo ./memcached -N -p 0 -U 7 -t 1 -l 192.168.123.1 -u pravin


Command to deploy memcached on real hardware with dragonet ::
    sudo ./memcached -N -p 0 -U 7777 -t 1 -l 10.113.4.71 -u ubuntu

