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
    sudo ./dragonet_bind_ethernet.sh <interface name>

The Interface name here is the 82599 NIC interface which is to be moved
from Kernel driver to uio driver which then can be used by Dragonet.

-----------------------------------
ETHZ machine-room specific hints
-----------------------------------

In ETHZ setup, you can get this interface name with following command ::

    cat ${HOME}/minfo/used_if.log | grep "intel" | grep "switch" | cut -d',' -f1 | head -n1

This uses `${HOME}/minfo/used_if.log` file to find out which NIC interfaces
are connected to switch and which one are to be used.

=========================
Restoring kernel driver
=========================

In theory, you can use the `./tools/dpdk_nic_bind.py` tool to restore the
NIC back to the kernel driver.  But it did not work for us on kernel version
2 3.13.0 as it was triggering kernel bug which was causing dmesg log and
then everything related to that particular interface stopped working (both
in dpdk and in kernel).



