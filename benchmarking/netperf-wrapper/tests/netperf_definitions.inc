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
    #print "toCoreList2 %s" % (ret)
    return ret

def getClientPortList(clientList, startPort, noPorts=1) :
    portList = []
    for cl in range(0, len(clientList)):
        prev_entries = [i for i, x in enumerate(clientList[:cl]) if x == clientList[cl]]
        portList.append(startPort + len(prev_entries))
    return portList

ENDPOINTS_TO_WAIT = 0

def genFancyEchoParameters(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    portNo = SERVER_INITIAL_PORT
    unique_clients = list(set(CLIENTS))
    sorted_clients = sorted(CLIENTS)
    sorted_clients_port_mapping = getClientPortList(sorted_clients, CLIENT_INITIAL_PORT, noPorts)
    #    print "Sorted clients are %s" % ( sorted_clients )
    #print "Sorted clients port mapping %s" % (sorted_clients_port_mapping)


    # Make sure that number of unique clients is more than threadsPerPort
    if createDedicatedFlows and len(sorted_clients ) < threadsPerPort :
        raise Exception("Not enough unique clients (%d) for load balancing per threads %d" % (
            len(sorted_clients),  threadsPerPort))
    if noPorts > 1:
        raise Exception("Multiport setup requested, which needs some more hacking  noPorts = %d" % (
                noPorts))
        sys.exit()

#    assert(len(unique_clients) >= 4)
    clientsPerThread = len(CLIENTS) / threadsPerPort

    if threadsPerPort == 1 :
        ret = ret + (" -a t%d -p %d " % (appID, SERVER_INITIAL_PORT))
        ENDPOINTS_TO_WAIT = 1
    else :
        ENDPOINTS_TO_WAIT = len(CLIENTS)
        alreadyCovered = 0
        for kk in range(0, threadsPerPort):
            ret = ret + (" -a t%d " % (kk))
            if createDedicatedFlows :
                for ii in range(alreadyCovered, alreadyCovered + clientsPerThread):
                    if (ii >= len(sorted_clients)) :
                       break
                    ret = ret + (" -f %s:%d/%s:%d " % (
                            TARGET, SERVER_INITIAL_PORT,
                            MINFO_CLIENT[sorted_clients[ii]]["EGRESS_INFO"]["src"],
                            sorted_clients_port_mapping[ii]
                        ))
                alreadyCovered = alreadyCovered + clientsPerThread

                # Handling case where number of clients cant equally balanced between threads
                if kk < (len(CLIENTS) % threadsPerPort) :
                    ret = ret + (" -f %s:%d/%s:%d " % (
                            TARGET, SERVER_INITIAL_PORT,
                            MINFO_CLIENT[sorted_clients[alreadyCovered]]["EGRESS_INFO"]["src"],
                            sorted_clients_port_mapping[alreadyCovered]
                        ))
                    alreadyCovered = alreadyCovered + 1


            else: # createDedicatedFlows
                print "don't create dedicated flows %s" % (ret)
                ret = ret + (" -a t%d -p %d " % (kk, SERVER_INITIAL_PORT))

    for kk in range(0, threadsPerPort):
        ret = ret + (" -t -q t%d " % (kk))
    print "FancyEcho Parameters (updated) : %s" % (ret)
    return ret

def endpoints_to_wait():
    if SERVER_CORES == 1:
        return 1
    return len(CLIENTS)

MAX_CORES = 40 # when using hyperthreads in asiago
MAX_CORES = 20

def SRV_CMDS(name):

    dragonet_container = {
                "llvmSF":   ["./scripts/pravin/deployDragonetGen.sh", "stack-sf" , "sf"],
                "llvmE10k": ["./scripts/pravin/deployDragonetGen.sh", "stack-e10k", "e10k"],
                }


    if name in  dragonet_container.keys() :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special_pinned_unpinned" : [
                            #"init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo %s %s %d %s" % (
                          dragonet_container[name][0],
                          dragonet_container[name][2],
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),

                      "cd dragonet/Dragonet/ ; sudo  ./scripts/pravin/runBetterBg.sh 4 ./ ./fancyecho-out.log  "
                        + " ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                                    ],


                    "init_cmd_special" : [
                        #"init_cmd_special_pinned" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s %s %s %d %s" % (
                          toCoreList2(range(MAX_CORES - (HWQUEUES), MAX_CORES)),
                          dragonet_container[name][0],
                          dragonet_container[name][2],
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),

                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                        toCoreList2(range((0 + SERVER_CORESHIFT), (SERVER_CORESHIFT + (SERVERS_INSTANCES*SERVER_CORES)))))
                        + " ./scripts/pravin/runBetterBg.sh 10  ./ ./fancyecho-out.log  "
                        + " ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True)),
                                    ],

                    "is_ready_cmd_special" : [
                      "cd dragonet/Dragonet/ ; ./scripts/pravin/wait_for_dn_app.sh %d %d %s %s " % (
                          HWQUEUES, endpoints_to_wait(),
                            dragonet_container[name][1], "bench-fancyecho"),
                                    ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "kill_cmd_special" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/fancyecho-out.log",
                                #"sudo killall bench-fancyecho",
                                # "sudo killall %s" % (dragonet_container[name][1]),
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
                        "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoLinux should be already running'",
                    "kill_cmd" : [
                                "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ./diff_stats.py %s %s " % (
                                    "./ethtool_out_1", "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ./diff_stats.py %s %s 1000 | grep rx_packets " % (
                                    "./ethtool_out_1", "./ethtool_out_2"),
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


    if name == "fancyEchoLinuxOpt" or name == "fancyEchoLinuxPoll" :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special" : [
                      "echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                      "echo 50 | sudo tee  /proc/sys/net/core/busy_read",
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
                                "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                                "echo 0 | sudo tee  /proc/sys/net/core/busy_read",
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


