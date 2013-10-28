================================================
STEP : Thinking "Why not click?"
================================================

My use-case:
 - A packet can go out on one or more ports
 - All elements will always have only one input, except operators
 - Operators:
   - Operators can have more than one input
   - They synchronize the inputs before generating the output

================================================
STEP : Building new application with DPDK
================================================

My current problem is that I am not able to compile completely new application
with DPDK.  Their build system is very complex and I am not able to understand
where all I need to do modifications to compile new app.

For example, I can't compile dpdk+Dragonet code because Dragonet is in
haskell and I don't know which libraries are needed to be linked in compilation
process to be able to generate final binary.  With GHC, I am always getting
these missing symbols which I have no idea how to include in compilation.


When I include all possible files which can be included from dpdk, I get
following error ::

  final link ... ghc: ofiles/virtio_rxtx.o: unknown symbol `per_lcore__lcore_id'
  linking extra libraries/objects failed

http://stackoverflow.com/questions/15004383/linking-extra-libraries-objects-failed

On other hand, if I want to move my code in DPDK then their Makefile
infrastructure is too complex that I would not know how to configure it
to compile something completely new and include correct libraries.


As a solution to this, I tried to compile the whole intel_dpdk as single
big library (by modifying global configuration) then I get following error ::

  undefined reference to `ceil'

I don't know how something as simple as ceiling function will be missing.  Also
this happens only when I am trying generate one big image, otherwise it works.
Also, this error comes only when I try to build with
``CONFIG_RTE_BUILD_SHARED_LIB=y``.  Otherwise it compiles.


I am worried that I will run into same problem when I work with click router.
I won't be able to compile the click router while calling functionality
from DPDK.


I couldn't take care of whatever library dependency issues DPDK had in static
compilation.  I just re-implemented the 'ceil' function and then code worked.
Luckly no other function from these conflicting library was used, which
helped me a bit in getting the work-around.


================================================
STEP : Integrating DPDK with Haskell simulator
================================================

 * I need to convert the DPDK l2fwd code into the which sends/receives single
    packet with one function call.
 * Add initialization code which needs to get called only once.
 * Eventually make it as a library
 * Write wrapper functions which will look like the the function in
   haskell wrapper ``tun.c``


================================================
STEP : Sending actual packets with E10K card
================================================

Aim is to send/receive packets while knowing what exactly is going on in the
code.  I should be able to control the memory used, callbacks involved, etc.

There are some UDP packets sent which it reports.  Which code is doing that?

 * Monitering the traffic on ziger2 (needs rewiring)
 * Reviewing the source code to see what exactly is happening

ziger1:
0000:81:00.0 '82599EB 10-Gigabit SFI/SFP+ Network Connection' if=eth1 drv=ixgbe
unused=igb_uio *Active*
eth1      Link encap:Ethernet  HWaddr 00:1b:21:8f:18:64
          inet addr:10.111.4.37  Bcast:10.255.255.255  Mask:255.0.0.0


ziger2:
eth2:     Link encap:Ethernet  HWaddr 00:0f:53:07:48:d5
          inet addr:10.111.4.36  Bcast:10.255.255.255  Mask:255.0.0.0


start tx_first
show port stats all

tcpdump -i eth2 -A -n -N -w packetCapture


So, the code is sending the packets.  I just need to find the code.

What is a port in this context?
--> They are talking about NIC ports.  So, I would assume that they are usual
NIC ports.

I have a code which sends/receives packets over DPDK and now I need to
integrate this code with haskell based simulator.

================================================
STEP : Running sample application on E10K card
================================================

The error I am getting is from file ``app/test-pmd/testpmd.c`` (line 1710) in
``main`` function.

Why am I getting this error?
I need an ability to debug the application.
 * Trying to run application directly.
   - Seems too complicated as I am not even able to compile it outside of
     script.
 * Modify the application, and see if the script runs a newer version or not.


It seems that code does see the device I am trying use.  Here are the logs
about it from the run ::

 EAL: PCI device 0000:81:00.0 on NUMA socket -1
 EAL:   probe driver: 8086:10fb rte_ixgbe_pmd
 EAL:   0000:81:00.0 not managed by UIO driver, skipping
 EAL: PCI device 0000:81:00.1 on NUMA socket -1
 EAL:   probe driver: 8086:10fb rte_ixgbe_pmd
 EAL:   PCI memory mapped at 0x7f9a81036000
 EAL:   PCI memory mapped at 0x7f9a81176000
 function: rte_eth_dev_allocate called
 EAL: PCI device 0000:82:00.0 on NUMA socket -1
 EAL:   probe driver: 8086:10d3 rte_em_pmd
 EAL:   0000:82:00.0 not managed by UIO driver, skipping
 ### nb_ports has value of 0

New Run with more debug information.  This shows that ``eth_dev_init`` is
faling ::

 EAL: PCI device 0000:81:00.0 on NUMA socket -1
 EAL:   probe driver: 8086:10fb rte_ixgbe_pmd
 EAL:   0000:81:00.0 not managed by UIO driver, skipping
 EAL: PCI device 0000:81:00.1 on NUMA socket -1
 EAL:   probe driver: 8086:10fb rte_ixgbe_pmd
 EAL:   PCI memory mapped at 0x7f4f1baa7000
 EAL:   PCI memory mapped at 0x7f4f1bbe7000
 function: rte_eth_dev_allocate called 1
 driver rte_ixgbe_pmd: eth_dev_init(vendor_id=0x32902 device_id=0x10fb) failed
 function:rte_eth_dev_init called --- 0
 EAL: PCI device 0000:82:00.0 on NUMA socket -1
 EAL:   probe driver: 8086:10d3 rte_em_pmd
 EAL:   0000:82:00.0 not managed by UIO driver, skipping
 ### nb_ports has value of 0


Finally I found a problem and fixed it.  The problem was that while resetting
the pipeline in ixgbe 82599 card, the system was not waiting for long enough.
The original version of the code only tried 10 times before giving up and
reporting the error.  When I modified the code to wait 100 times before giving
up, then code worked.

Function modified: ``ixgbe_reset_pipeline_82599`` in file
``lib/librte_pmd_ixgbe/ixgbe/ixgbe_82599.c``.

====================================
STEP: Run Sample application
====================================

Running a sample application which uses 10G NIC to make sure that everything
is in order.

-------------------------
Application: testpmd
-------------------------

Right now, I am trying to run the sample application ``testpmd`` from dpdk,
but it is not running. Here are my debugging attempts:

 * Remove ixgbe driver as it might be conflicting
   - Did not help out of the box
   - Configure interfaces when igb_uio driver is active and see if it is pingable
     - DIVERSION: how do I know which interface is powered by which kernel module?
       lshw does not tell the logical name (which is interface name)
       when ``igb_uio`` driver is used.
     - DIVERSION: What are these NVIDIA manufacured Network cards which don't
       even fall in Network class?
        - it seems that its a bridge which works as Ethernet and the driver
          is ``forcedeth``.
        - OK.  Its Nvidia driver for integrated Ethernet port on the
          motherboard.  Somehow it is shown with class bridge by lshw
 * Read the documentation of how to use test command.
   - Maybe I need to pass a parameter about which interface to use

-------------------------
Application: rte-app
-------------------------

 * While reading detailed documentation, it seems that other application should
   be tried first.
 * By reading the instructions, I learned that most of the time I had spent into
   writing the script is already there.

----------------------------------
scripts/setup.sh: running apps
----------------------------------

 * With this script, I ran the application again and I am getting following
   error ::

 Cause: No probed ethernet devices - check that CONFIG_RTE_LIBRTE_IGB_PMD=y and
 that CONFIG_RTE_LIBRTE_EM_PMD=y and that CONFIG_RTE_LIBRTE_IXGBE_PMD=y in your
 configuration file

I have verified that the setup works when used with 1G (Intel 82574L) card,
so maybe 10G card (Intel 82599EB) is not supported.  As per following email,
both of these cards should have been supported

http://dpdk.org/ml/archives/dev/2013-May/000060.html

Working setup:
-------------------

 * 3: Compile the app
 * 5: Insert UIO module
 * 8: Setup hugepage mappings for NUMA system (32 pages per NUMA node (3 NUMA
   nodes))
 * 10: Bind Ethernet device to IGB UIO module (bind "82574L if=eth0 drv=e1000e")
 * 13: Make sure that all huge pages are actually free
 * 12: To actually run the application (core mask used: 0xf)

NOTES:
 * Any other device ends up giving above mentioned error.
 * Allocating memory as Non-NUMA node leads to crash (ziger1 is NUMA system).
 * If number of huge pages is less than 32 (say 16) then it does not work
    I get an error saying ``Creation of mbuf pool for socket 0 failed``


vim:tw=80:ts=2:softtabstop=2:shiftwidth=2:expandtab:
