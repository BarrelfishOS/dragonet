Compiling for Dragonet
===========================

You can compile the dpdk-1.7.1 for Dragonet by using following command ::

    ./doConfig.sh
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

