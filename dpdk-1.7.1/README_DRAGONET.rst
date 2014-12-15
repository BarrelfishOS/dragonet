==================================================
Commands for quick installation and deployment
==================================================

After running following commands, dpdk should be in ready state to be used by
Dragonet ::

    ./doConfig.sh
    sudo ./tools/setup.sh dragonet
    sudo ./dragonet_bind_ethernet.sh


Now, following text explains these steps in more details

===========================
Compiling for Dragonet
===========================

You can compile the dpdk-1.7.1 for Dragonet by using following command ::

    ./doConfig.sh

Running this command will make sure that you can compile haskell part
of Dragonet code. To actually run the stack, you need to look also
perform "Deployment of DPDK" (details bellow).

------------------------------
Details about ldconfig setup
------------------------------

NOTE: This is for information purpose only, and above script should take
care of this automatically.


In addition to compiling the dpdk, `./doConfig.sh` script will also add the
library location into ldconfig path so that Dragonet can link with it.
It does following things to make sure of it ::

    echo "$PWD/build/lib" | sudo tee /etc/ld.so.conf.d/dpdk.conf
    sudo ldconfig

The first line will configure and compile following shared libraries

 * `libintel_dpdk.so`
 * `libdpdk_driver.so`
 * and other supporting libs in `./build/lib/` directory.

The second line will add the `./build/lib/` in `ldconfig` so that Dragonet
driver can find it.

The third line runs the `ldconfig` to make sure that these libraries are in
the cache.

=======================
Deployment of DPDK
=======================

Dpdk depends on kernel modules, hugepages and proper NIC bindings to work.
This can be done by running following scripts ::

    sudo ./tools/setup.sh dragonet
    sudo ./dragonet_bind_ethernet.sh


