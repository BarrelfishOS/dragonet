## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

MEMCACHED_PORT =  7777
MEMASLAP_CMD ="sudo ./runBetter.sh memaslap -t%ds --cfg_cmd=../libmemcached-1.0.18/bmKey_64_val_1024.conf -S%dm " % (
        TOTAL_LENGTH, TOTAL_LENGTH
        )
QUEUES = 4
APPS = SERVERS_INSTANCES * SERVER_CORES
def toCoreList2(clist, separator=",", prefix=""):
    ret = ""
    for c in clist:
        if ret == "" :
            ret = "%s%d" % (prefix, c)
        else :
            ret = "%s%s%s%d" % (ret, separator, prefix, c)
    print "toCoreList2 %s" % (ret)
    return ret


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
                    "init_cmd_3" : [
                        "cd dragonet/Dragonet/ ; sudo ./deployDragonetE10k.sh %s" % (
                                toCoreList2(range(0, APPS), separator=" ", prefix="t"),
                            ),
                                ],

                    "init_cmd_1" : [
                        "cd dragonet/Dragonet/ ; sudo taskset -c %s ./deployDragonetE10k.sh %s" % (
                                toCoreList2(range(0, (QUEUES * 2))),
                                toCoreList2(range(0, APPS), separator=" ", prefix="t"),
                            ),
                                ],
                    "init_cmd_2" : [],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -N -c 64000 -m 64000 -u ubuntu ",
                    "kill_cmd" : [
                                "sudo killall memcached || true",
                                "sudo killall llvm-cgen-e10k || true",
                                 ],
                    "out_cmd" : [],
                },
    "memcached_dn" : {
                    "init_cmd" : [],
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
                        "kill_cmd" : [],
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

