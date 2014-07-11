## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

SERVER_INITIAL_PORT =  888
CLIENT_INITIAL_PORT =  9000
UNIQUE_CLIENTS = list(set(CLIENTS))
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

def genFancyEchoParameters(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    unique_clients = list(set(CLIENTS))
    instances_per_client = (len(CLIENTS)/len(UNIQUE_CLIENTS))
    if not (len(CLIENTS) % len(UNIQUE_CLIENTS)) == 0:
        print "Not enough not all clients are repeated same times.  All clients: %d, unique clients: %d  " % (
                len(CLIENTS),  len(UNIQUE_CLIENTS))
        raise Exception("Not enough not all clients are repeated same times."
                + "  All clients: %d, unique clients: %d  " % (
                len(CLIENTS),  len(UNIQUE_CLIENTS)))

    # Make sure that number of unique clients is more than threadsPerPort
    if createDedicatedFlows and len(unique_clients) < threadsPerPort :
        raise Exception("Not enough unique clients (%d) for load balancing per threads %d" % (
            len(unique_clients),  threadsPerPort))


    for p in range(0, noPorts):
        portNo = SERVER_INITIAL_PORT + p
        for c in range(0, threadsPerPort):
            if createDedicatedFlows:
                if c == 0 :
                    ret = ret + (" -a t%d -p %d " % (appID, portNo))
                else :
                    ret = ret + (" -a t%d " % (appID))
                    for ff in range(0, instances_per_client):
                        ret = ret + (" -f %s:%d/%s:%d " % (
                            TARGET, portNo,
                            MINFO_CLIENT[unique_clients[c]]["EGRESS_INFO"]["src"],
                            (CLIENT_INITIAL_PORT + ff)
                        ))
            else: # createDedicatedFlows
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


def SRV_CMDS(name):

    if name == "llvmE10k" :
        return {
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
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),

                      "cd dragonet/Dragonet/ ; ./wait_for_dn_app.sh %d %d " % (HWQUEUES, SERVERS_INSTANCES * SERVER_CORES),
                                    ],

                    "init_cmd_special2" : [
                        "cd dragonet/Dragonet/ ; sudo taskset -c %s ./deployDragonetE10k.sh %d %s" % (
                                toCoreList2(range(0, (HWQUEUES* 2))), HWQUEUES,
                                toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; ./runBetterBg.sh 2 ./ ./fancyecho-out.log  ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                                ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "exec_cmd_complex" : "./runBetterCD.sh ../../Dragonet/ ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                    "kill_cmd_special" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/fancyecho-out.log",
                                "sudo killall bench-fancyecho || true",
                                "sudo killall llvm-cgen-e10k",
                                 ],
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }


    if name == "llvmSF" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd_special_min" : [
                      "cd dragonet/Dragonet/ ; sudo ./deployDragonetSF.sh %d %s" % (
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                                    ],

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo ./deployDragonetSF.sh %d %s" % (
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; sudo ./runBetterBg.sh 0 ./ ./fancyecho-out.log  ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),

                      "cd dragonet/Dragonet/ ; ./wait_for_dn_app.sh %d %d " % (HWQUEUES, SERVERS_INSTANCES * SERVER_CORES),
                                    ],

                    "init_cmd_special2" : [
                        "cd dragonet/Dragonet/ ; sudo taskset -c %s ./deployDragonetSF.sh %d %s" % (
                                toCoreList2(range(0, (HWQUEUES* 2))), HWQUEUES,
                                toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; ./runBetterBg.sh 2 ./ ./fancyecho-out.log  ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                                ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "exec_cmd_complex" : "./runBetterCD.sh ../../Dragonet/ ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                    "kill_cmd_special" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/fancyecho-out.log",
                                "sudo killall bench-fancyecho || true",
                                "sudo killall llvm-cgen-sf",
                                 ],
                    "kill_cmd" : [],

                    "out_cmd" : [],
                }

    if name == "noServer" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }

    if name == "fancyEchoLinux" :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                      + " ./runBetterBg.sh 1 ../benchmarking/micro ./fancyEchoLinux-out.log  "
                      + "./fancyEchoLinux %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, False)),
                        "sleep 2",
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoLinux should be already running'",
                    "kill_cmd" : [
                                "sudo killall fancyEchoLinux || true",
                                 ],
                    "out_cmd" : [],
                }

    if name == "fancyEchoOnload" :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                      + " ./runBetterBg.sh 1 ../benchmarking/micro ./fancyEchoLinux-out.log  "
                      + "%s ./fancyEchoLinux %s " % (onload_prefix,
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, False)),
                        "sleep 2",
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoOnload should be already running'",
                    "kill_cmd" : [
                                "sudo killall fancyEchoLinux || true",
                                 ],
                    "out_cmd" : [],
                }


    if name == "HImplOnload" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }
    if name == "CImplOnload" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Assuming server is deployed'",
                    "kill_cmd" : [],  # sudo killall sfOnload
                    "out_cmd" : [],
                }
    if name == "netserver" :
        return {
                    "client_extra_opts" : "",
                    "init_cmd" : ["sudo killall netserver || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "../netperf-2.6.0/src/netserver -D ",
                    "kill_cmd" : ["sudo killall netserver || true"],
                    "out_cmd" : [],
                }

    if name == "socat" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [
                        "sudo killall socat || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee  /proc/sys/net/core/busy_read",
                                ],
                    "exec_cmd" : "socat -b 64000 PIPE UDP-LISTEN:7,fork ",
                    "kill_cmd" : ["sudo killall socat || true"],
                    "out_cmd" : [],
                }

    if name == "socat_poll" :
        return {
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
                }

    if name == "socat_pollL" :
        return {
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
                }

    if name == "socat_onload" :
        return {
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
                }

    if name == "netcat" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : ["sudo killall nc.traditional || true",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        "echo 0 | sudo tee /proc/sys/net/core/busy_read",
                    ],
                    "exec_cmd" : "nc.traditional -nlup %d -e /bin/cat" % (SERVER_INITIAL_PORT),
                    "kill_cmd" : ["sudo killall nc.traditional || true"],
                    "out_cmd" : [],
                }

    if name == "netcat_poll" :
        return {
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
                }
    if name == "netcat_onload" :
        return {
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
                }
    if name == "HImplDpdk" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }
    if name == "CImplDpdk" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }
    if name == "llvmDpdk" :
        return {
                    "client_extra_opts" : "-N",
                    "init_cmd" : [],
                    "exec_cmd" : "echo ", # "/home/ubuntu/dragonet/Dragonet/ ; sudo LD_LIBRARY_PATH=.cabal-sandbox/lib/x86_64-linux-ghc-7.4.1/dpdk-0.1.0.0/ ./dist/build/llvm-cgen-dpdk/llvm-cgen-dpdk ./lpgImpl.unicorn "
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }

    raise Exception("Invalid echo server name: [%s]" % (name))


