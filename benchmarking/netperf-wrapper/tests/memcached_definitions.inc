## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2


onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

def get_proto_specific_vals():
    """retuns values for MEMCACHED_PORT_TCP, MEMCACHED_PORT_UDP, USING_UDP """
    if (USE_TCP) :
        return (7777, 0, "")
    return (0, 7777, " --udp ")

MEMCACHED_PORT_TCP, MEMCACHED_PORT_UDP, USING_UDP = get_proto_specific_vals()


# USE UDP
MEMCACHED_PORT_UDP = 7777
MEMCACHED_PORT_TCP = 0
USING_UDP = " --udp "

# USE TCP
MEMCACHED_PORT_UDP = 0
MEMCACHED_PORT_TCP = 7777
USING_UDP = ""

MEMCACHED_PORT =  7777
MEMCACHED_PORT_DN = 7
#MEMASLAP_CMD ="sudo ./runBetter.sh memaslap %s  -t%ds -S%dm " % (
MEMASLAP_CMD ="sudo ./runBetter.sh memaslap %s  -t%ds --cfg_cmd=../libmemcached-1.0.18/bmKey_64_val_1024.conf -S%dm " % (
        USING_UDP, TOTAL_LENGTH, TOTAL_LENGTH
        )

SRV_CMDS = {
    "memcached" : {
                    "client_extra_opts" : "%d" % (MEMCACHED_PORT),
                    "init_cmd" : ["sudo killall memcached || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -p %d  -u ubuntu -U %d" % (MEMCACHED_PORT_TCP, MEMCACHED_PORT_UDP),
                    "kill_cmd" : ["sudo killall memcached || true"],
                    "out_cmd" : [],
                },
}

