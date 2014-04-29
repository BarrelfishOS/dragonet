## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2



onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"
MEMCACHED_PORT = 7777
MEMCACHED_PORT_DN = 7
MEMASLAP_CMD="memaslap  --udp -t%ds --cfg_cmd=../libmemcached-1.0.18/bmKey_64_val_1024.conf -S%dm " % (
        TOTAL_LENGTH, TOTAL_LENGTH
        )

SRV_CMDS = {
    "memcached" : {
                    "client_extra_opts" : "%d" % (MEMCACHED_PORT),
                    "init_cmd" : ["sudo killall memcached || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "../memcached/memcached  -p 0  -u ubuntu -t 1  -U %d" % (MEMCACHED_PORT),
                    "kill_cmd" : ["sudo killall memcached || true"],
                    "out_cmd" : [],
                },
}

