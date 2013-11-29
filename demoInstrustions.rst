
----------------------
Server side setup
----------------------

Here are the instructions for setting up server (ziger1) part ::

    # Login into ziger1 from one of the emmentaler
 ssh ubuntu@10.110.4.51
    # or you can also use ssh ubuntu@ziger1.in.barrelfish.org

    # Step-2: Get to correct screen session
 sudo -i
 byobu-screen

    # You want to be on 3rd screen session with name "build Dragonet"
    # You can do that with keystroke "CTRL+a 3"
    # You should be in a directory ``/home/ubuntu/dragonet/Dragonet``
    # Now, you can either run the existing executable ./dpdkDragonet
    # or run the whole compilation + run process.
 ./dpdkDragonet

    # In case you want to re-run the whole make process
 make dpdk_run

----------------------
Client side setup
----------------------

Here are the instructions for running the client (ziger2) part ::

    # Login into ziger2 from one of the emmentaler
 ssh ubuntu@10.110.4.61

    # Step-2: Get to correct screen session
 sudo -i
 byobu-screen

    # You want to be on 1st screen session with name "ping_test"
    # You can do that with keystroke "CTRL+a 0"
    # You should be in a directory ``/home/ubuntu/dragonet/dpdk-1.5.0r1/examples/uDragonet``
    # Now, you can either run the existing executable ./test_ping.sh
    # or run the ping yourself.
 ./test_ping.sh

    # in case you want to run the ping manually.
 ping -I eth2 10.111.4.37



NOTE: The pings will not work when dragonet network stack is not running

------------------------
Additional information
------------------------

The directory /home/ubuntu/dragonet is essentially the clone of the ``dragonet``
repository.  I normally checkout from my home directory, but you should be able
to check it out from netos home directory as well.

    git clone ssh://shindep@129.132.186.96:8006//home/netos/git/dragonet

The dpdk related code is in ``dragonet/dpdk-1.5.0r1`` directory. Hopefully you
don't have to deal with this code for demo.

Other thing which I should mention is that ziger1 is running dpdk on following
NIC interface ::

    PCI address = 81:00.0 || eth1 || 00:1b:21:8f:18:64 || 10.111.4.37

And for ziger2, the interface is ::

    eth2 || 00:0f:53:07:48:d5 || 10.111.4.36

These to NIC ports are directly connected to each other via cable without
connecting to a switch.

In case ziger1 reboots then we may have a small problem.  This will imply
that everything needs to be reinstalled. There is a script to automate
most of the steps needed to setup the machine ``dragonet/dpdk_heldpers/install.sh``.
This script not completely updated, but it has lot of information about
all the modules needed.

There is a script named
``dragonet/dpdk-1.5.0r1/tools/mySetup.sh``
which will automate most of the steps to run a sample DPDK application, and it
needs only few inputs.

 * 8: Hugepage mappings: 32 pages per NUMA node, 3 NUMA nodes
 * 12: core mask: 0xf

This script is essentially automated version of following script which
is designed to compile and configure the dpdk part ::
``dragonet/dpdk-1.5.0r1/tools/setup.sh``.    Following are the options which
should be selected when running a sample dpdk application :

 * 3: Compile the app
 * 5: Insert UIO module
 * 8: Setup hugepage mappings for NUMA system (32 pages per NUMA node (3 NUMA
   nodes))
 * 10: Bind Ethernet device to IGB UIO module (bind "82574L if=eth0 drv=e1000e")
 * 13: Make sure that all huge pages are actually free
 * 12: To actually run the application (core mask used: 0xf)



