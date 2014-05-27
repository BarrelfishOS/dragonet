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
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                    #"kill_cmd" : ["sudo killall memcached || true"],
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "memcached_dragonet" : {
                    "init_cmd" : [
                        "cd dragonet/Dragonet/ ; sudo taskset -c 0,1,2,3 ./deployDragonetE10k.sh ",
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -N -c 64000 -m 64000 -u ubuntu ",
                    "kill_cmd" : [
                                "sudo killall memcached || true",
                                "sudo killall llvm-cgen-e10k || true",
                                 ],
                    "out_cmd" : [],
                },
    "memcached_onload" : {
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "sudo ./runBetter.sh %s ../memcached/memcached -c 64000 -m 64000 -u ubuntu " % (onload_prefix),
                        "kill_cmd" : ["sudo killall memcached || true"],
                        "out_cmd" : [],
                },
    "memcached_poll" : {
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

