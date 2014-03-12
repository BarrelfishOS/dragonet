=====================
Installation
=====================

Please refer  ``INSTALL.sh`` and ``../dpdk_helpers/install.sh`` for
generic installation help.

=====================
Test: ping
=====================


To compile the needed code ::

 make tuntap_icmp

To run the application, you need sudo access.  So, following command will
ask for your password ::

 make tuntap_icmp_run

To end the application, use CTRL+C to kill the application.

In another terminal, run following command to send ping packets ::

  ./test_tuntap_ping.sh

This code will send two ICMP packets and get their response.


NOTE:
-------------

Please note that you may see some messages saying "This protocol not supported".
Don't worry about that as these are mostly due to other packets flying around.

