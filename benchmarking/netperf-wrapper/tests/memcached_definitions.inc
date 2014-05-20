## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

MEMCACHED_PORT =  7777
MEMASLAP_CMD ="sudo ./runBetter.sh memaslap -t%ds --cfg_cmd=../libmemcached-1.0.18/bmKey_64_val_1024.conf -S%dm " % (
        TOTAL_LENGTH, TOTAL_LENGTH
        )

SRV_CMDS = {
    "memcached" : {
                    "client_extra_opts" : "%d" % (MEMCACHED_PORT),
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                    #"exec_cmd" : "../memcached/memcached -d -u ubuntu ",
                    #"exec_cmd" : "./memcached_wrapper.sh -u ubuntu ",
                    "kill_cmd" : ["sudo killall memcached || true"],
                    "out_cmd" : [],
                },
    "memcached_onload" : {
                    "client_extra_opts" : "%d" % (MEMCACHED_PORT),
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "sudo ./runBetter.sh %s ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                        "kill_cmd" : ["sudo killall memcached || true"],
                        "out_cmd" : [],
                },
    "memcached_poll" : {
                    "client_extra_opts" : "%d" % (MEMCACHED_PORT),
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        "echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 50 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                    "kill_cmd" : [
                        #"sudo killall memcached || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "out_cmd" : [],
                },
}

