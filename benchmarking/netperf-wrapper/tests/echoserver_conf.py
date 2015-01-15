## -*- mode: python; coding: utf-8 -*-

include('dragonet_stack.py')

def getClientPortList(clientList, startPort, noPorts=1) :
    portList = []
    for cl in range(0, len(clientList)):
        prev_entries = [i for i, x in enumerate(clientList[:cl]) if x == clientList[cl]]
        portList.append(startPort + len(prev_entries))
    return portList

def genFancyEchoParameters(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    portNo = SERVER_INITIAL_PORT
    sorted_clients = (CLIENTS)
    sorted_clients.reverse()
    sorted_clients_port_mapping = (getClientPortList(sorted_clients, CLIENT_INITIAL_PORT, noPorts))
    sorted_clients_port_mapping.reverse()

    # Make sure that number of unique clients is more than threadsPerPort
    if createDedicatedFlows and len(sorted_clients ) < threadsPerPort :
        raise Exception("Not enough unique clients (%d) for load balancing per threads %d" % (
            len(sorted_clients),  threadsPerPort))
    if noPorts > 1:
        raise Exception("Multiport setup requested, which needs some more hacking  noPorts = %d" % (
                noPorts))
        sys.exit()

    clientsPerThread = len(CLIENTS) / threadsPerPort

    if threadsPerPort == 1 :

        ret = ret + (" -a t%d -p %d " % (appID, SERVER_INITIAL_PORT))
    else :
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
                ret = ret + (" -p %d " % (SERVER_INITIAL_PORT))

    for kk in range(0, threadsPerPort):
        ret = ret + (" -t -q t%d " % (kk))
    print "FancyEcho Parameters (updated) : %s" % (ret)
    return ret

def endpoints_to_wait():
    if SERVER_CORES == 1:
        return 1
    return len(CLIENTS)

def SRV_CMDS(name):


    if name in  dragonet_container.keys() :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special" : [

                      # run stack
                      "cd %s ; " % (dragonet_container[name]['base_dir'])
                        + "sudo %s " % (get_isolation_container(is_server=False))
                        + " %s %d %s " % (
                          #dragonet_container[name]['deploy_stack'],
                          #HWQUEUES, "priority")
                          dragonet_container[name]['deploy_stack'],
                          HWQUEUES, "priority")
#                        + " %s %d %d " % ( "hardcoded", CONCURRENCY,
#                            len(client_names))
                        ,


                      # run server
                      "cd %s ; " % (dragonet_container[name]['base_dir'])
                        + "sudo %s " % (get_isolation_container(is_server=True))
                        + " ./scripts/pravin/runBetterBg.sh 10  ./ ./fancyecho-out.log  "
                        + " ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, True))
                        ,


                      "sleep 4",
                    ],

                    "is_ready_cmd_special" : [
                        "cd %s ; %s %d %d %s " % (
                          dragonet_container[name]['base_dir'],
                          dragonet_container[name]['is_stack_ready'],
                          HWQUEUES,
                          endpoints_to_wait(),
                          "bench-fancyecho"),
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

                    # sudo ethtool -n eth7 rx-flow-hash udp4 # for setting up RSS
                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                      + " ./scripts/pravin/runBetterBg.sh 1 ../benchmarking/micro ./fancyEchoLinux-out.log  "
                      + "./fancyEchoLinux %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, False)),
                        "sleep 2",
                        "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoLinux should be already running'",
                    "kill_cmd" : [
                                "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                                    "./ethtool_out_1", "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
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
                      + " ./scripts/pravin/runBetterBg.sh 1 ../benchmarking/micro ./fancyEchoLinux-out.log  "
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
                      + " ./scripts/pravin/runBetterBg.sh 1 ../benchmarking/micro ./fancyEchoLinux-out.log  "
                      + "./fancyEchoLinux %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, False)),
                        "sleep 2",
                        "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoLinux should be already running'",
                    "kill_cmd" : [
                                "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                                    "./ethtool_out_1", "./ethtool_out_2"),
                                "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
                                    "./ethtool_out_1", "./ethtool_out_2"),
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


def get_proto_specific_server_ports():
    """retuns values for to be used for ports at server end """
    port_list = range(SERVER_INITIAL_PORT, (SERVER_INITIAL_PORT + (len(server_names))))

    cl_port_list = range(CLIENT_INITIAL_PORT, (CLIENT_INITIAL_PORT +
                ((len(CLIENTS)/len(UNIQUE_CLIENTS)) * len(server_names))
                ))
    port_list_empty = [0 for x in port_list]
    port_list_for_clients = repeat_list_elem_n_times(port_list, len(CLIENTS))
    #port_list_cl_src = repeat_list_elem_n_times(cl_port_list, (len(UNIQUE_CLIENTS)))
    port_list_cl_src = []
    for cidx in range(0, len(CLIENTS)):
        prev_entries = [i for i, x in enumerate(CLIENTS[:cidx]) if x == CLIENTS[cidx]]
        port_list_cl_src.append(CLIENT_INITIAL_PORT + len(prev_entries))

    print "port_list_for_clients dst: %s" % (port_list_for_clients)
    print "port_list_for_clients src: %s" % (port_list_cl_src)
    if (USE_TCP) :
        return (port_list_for_clients, port_list_cl_src,       port_list_empty, "")
    else:
        return (port_list_for_clients, port_list_cl_src, port_list,       " --udp ")



def get_cmd_server(id, cmd) :
    """Returns special command when it is first server and there is special command present"""
    special_cmd = "%s_special" % (cmd)
    if id == 0 and special_cmd  in echo_server_cmds.keys() :
            return echo_server_cmds[special_cmd]

    if cmd in echo_server_cmds.keys() :
        return echo_server_cmds[cmd]
    return []


def create_server(id):
    srv = ('server-%d' % (id), {
     'deployment_host': server_names[id],
     'result_location': '%s_server%d' % (RESULT_LOCATION_BASE, id),
     'tools_location': TOOLS_LOCATION1,
     'is_server': True,
     'TOOLS': o([

       ( 'echoServer%d' % (id),
       {
           'command': 'sudo taskset -c %s %s %s ' % (
                toCoreList(server_core_list[id]), server_onload,
                echo_server_cmds['exec_cmd'],
                ),

           'runner': 'process',
           'units': 'Gbits/s',
           'wait_for': False,
           'is_catastrophic': False,
           'init_cmd': get_cmd_server(id, "init_cmd"),
           'kill_cmd': get_cmd_server(id, "kill_cmd"),
           'out_cmd': get_cmd_server(id, "out_cmd"),
           'is_ready_cmd': get_cmd_server(id, "is_ready_cmd"),
       }),

       ('dstat',
       {
           'command': dstatCmd (mname = 'server%d' % (id),
               cpus=toCoreList(server_core_list[id]), netdev=server_if[server_names[id]],
               interrupts=getInterruptLines(MINFO_SERVER[server_names[id]])),
           'runner': 'dstat_json',
           'wait_for': True,
           'delay': DELAY,
           'is_catastrophic': True,
           'init_cmd': [],
           'out_cmd': [],
           'kill_cmd': [],
       }),


#       ('ethtool',
#       {
#           'command': dstatCmd (mname = 'server%d' % (id),
#               cpus=toCoreList(server_core_list[id]), netdev=server_if[server_names[id]],
#               interrupts=getInterruptLines(MINFO_SERVER[server_names[id]])),
#           'runner': 'ethtool_json',
#           'wait_for': True,
#           'is_catastrophic': True,
#           'init_cmd': [],
#           'out_cmd': [],
#           'kill_cmd': [],
#       }),

       ])}
    )
    return srv

def create_server_list(server_names, startid=SPECIAL_SERVERS_COUNT):
    slist = []
    if startid >= len(server_names):
        return slist
    for i in range(startid, len(server_names)):
       slist.append(create_server(i))
    return slist







