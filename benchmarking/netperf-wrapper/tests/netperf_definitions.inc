## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

#SERVER_INITIAL_PORT =  7
SERVER_INITIAL_PORT =  888
NETPERF_CMD="../netperf-2.6.0/src/netperf -fg -4"

def toCoreList2(clist, separator=",", prefix=""):
    ret = ""
    for c in clist:
        if ret == "" :
            ret = "%s%d" % (prefix, c)
        else :
            ret = "%s%s%s%d" % (ret, separator, prefix, c)
    print "toCoreList2 %s" % (ret)
    return ret


def genFancyEchoParameters(noPorts, threadsPerPort):
    ret = ""
    # socket info per slot
    appID = 0
    for p in range(0, noPorts):
        portNo = SERVER_INITIAL_PORT + p
        for c in range(0, threadsPerPort):
            ret = ret + (" -a t%d -p %d " % (appID, portNo))
            appID = appID + 1

    # thread info per slot
    appID = 0
    for p in range(0, noPorts):
        for c in range(0, threadsPerPort):
            ret = ret + (" -t -q t%d " % (appID))
            appID = appID + 1

    print "FancyEcho Parameters: %s" % (ret)
    return ret


SRV_CMDS = {
    "llvmE10k" : {
                    "client_extra_opts" : "-N",
                    "init_cmd_special_min" : [
                      "cd dragonet/Dragonet/ ; sudo ./deployDragonetE10k.sh %d %s" % (
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                                    ],

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo ./deployDragonetE10k.sh %d %s" % (
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; sudo ./runBetterBg.sh 0 ./ ./fancyecho-out.log  ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES)),

                      "cd dragonet/Dragonet/ ; ./wait_for_dn_app.sh %d %d " % (HWQUEUES, SERVERS_INSTANCES * SERVER_CORES),
                                    ],

                    "init_cmd_special2" : [
                        "cd dragonet/Dragonet/ ; sudo taskset -c %s ./deployDragonetE10k.sh %d %s" % (
                                toCoreList2(range(0, (HWQUEUES* 2))), HWQUEUES,
                                toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; ./runBetterBg.sh 2 ./ ./fancyecho-out.log  ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES)),
                                ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "exec_cmd_complex" : "./runBetterCD.sh ../../Dragonet/ ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES)),
                    "kill_cmd" : [
                                "sudo killall llvm-cgen-e10k || true",
                                "sudo killall bench-fancyecho || true",
                                 ],
                    "out_cmd" : [],
                },

    "noServer" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "HImplOnload" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "CImplOnload" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Assuming server is deployed'",
                    "kill_cmd" : [],  # sudo killall sfOnload
                    "out_cmd" : [],
                },
    "netserver" : {
                    "client_extra_opts" : "",
                    "init_cmd" : ["sudo killall netserver || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "../netperf-2.6.0/src/netserver -D ",
                    "kill_cmd" : ["sudo killall netserver || true"],
                    "out_cmd" : [],
                },

    "socat" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall socat || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee  /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "socat -b 64000 PIPE UDP-LISTEN:7,fork ",
                    "kill_cmd" : ["sudo killall socat || true"],
                    "out_cmd" : [],
                },
    "socat_poll" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall socat || true",
                        "echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 50 | sudo tee  /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "socat -b 64000 PIPE UDP-LISTEN:7,fork ",
                    "kill_cmd" : ["sudo killall socat || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                            ],
                    "out_cmd" : [],
                },
    "socat_pollL" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall socat || true",
                        "echo 5000 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 5000 | sudo tee  /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "socat -b 64000 PIPE UDP-LISTEN:7,fork ",
                    "kill_cmd" : ["sudo killall socat || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                            ],
                    "out_cmd" : [],
                },
    "socat_onload" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall socat || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "%s socat -b 64000 PIPE UDP-LISTEN:7,fork " % (onload_prefix),
                    "kill_cmd" : ["sudo killall socat || true",
                            ],
                    "out_cmd" : [],
                },
    "netcat" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : ["sudo killall nc.traditional || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "nc.traditional -nlup 7 -e /bin/cat",
                    "kill_cmd" : ["sudo killall nc.traditional || true"],
                    "out_cmd" : [],
                },
    "netcat_poll" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall nc.traditional || true",
                        "echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 50 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "nc.traditional -nlup 7 -e /bin/cat",
                        "kill_cmd" : ["sudo killall nc.traditional || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                        ],
                    "out_cmd" : [],
                },
    "netcat_onload" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall nc.traditional || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "%s nc.traditional -nlup 7 -e /bin/cat" % (onload_prefix),
                        "kill_cmd" : ["sudo killall nc.traditional || true",
                        ],
                    "out_cmd" : [],
                },
    "HImplDpdk" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "CImplDpdk" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "llvmDpdk" : {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ", # "/home/ubuntu/dragonet/Dragonet/ ; sudo LD_LIBRARY_PATH=.cabal-sandbox/lib/x86_64-linux-ghc-7.4.1/dpdk-0.1.0.0/ ./dist/build/llvm-cgen-dpdk/llvm-cgen-dpdk ./lpgImpl.unicorn "
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },

}

