## -*- mode: python; coding: utf-8 -*-

include('dragonet_stack.py')

def flow2Str(f):
    (dst_ip, dst_p, clName, src_p, srv_id, cli_id) = f
    return ("%s:%d/%s:%d" % (
                            dst_ip, dst_p,
                            MINFO_CLIENT[clName]["EGRESS_INFO"]["src"],
                            src_p
                        ))

def flows_to_arg_fancyEcho(flows, listenPort, threads):

    ret = " -W "
    for i in range(0, threads):
        ret = ret + ( "-a T%d -p %d " % (i, listenPort))
    for f in flows:
        ret = ret + (" -F %s " % (flow2Str(f)) )
    for i in range(0, threads):
        ret = ret + ( "-t -q T%d " % (i))
    return ret

def flows_to_arg_fancyEcho_linux_full(flows, listenPort, threads):

    flowsPerThread = []
    for tid in range(0, threads):
        flowsPerThread.append([])

    for fid in range(0, len(flows)) :
        flowsPerThread[(fid % threads)].append(flows[fid])

    ret = " "

    for tid in range(0, len(flowsPerThread)):
        ret = ret + ( "-a T%d " % (tid))
        for i in range(0, len(flowsPerThread[tid])):
            ret = ret + (" -f %s " % (flow2Str(flowsPerThread[tid][i])))

    for tid in range(0, len(flowsPerThread)):
        ret = ret + ( "-t -q T%d " % (tid))
    print "fancyecho Linux args [%s]" % (ret)
    return ret


def flows_to_arg_fancyEcho_linux(flows, listenPort, threads):

    ret = " "

    for tid in range(0, threads):
        ret = ret + ( "-a T%d -p %d " % (tid, listenPort))

    for tid in range(0, threads):
        ret = ret + ( "-t -q T%d " % (tid))

    print "fancyecho Linux args [%s]" % (ret)
    return ret


def getClientPortList(clientList, startPort, noPorts=1) :
    portList = []
    for cl in range(0, len(clientList)):
        prev_entries = [i for i, x in enumerate(clientList[:cl]) if x == clientList[cl]]
        portList.append(startPort + len(prev_entries))
    return portList

def SRV_CMDS(name):


    if name in  dragonet_container.keys() :
        return {
                    "client_extra_opts" : "-N",

                    "init_cmd_special" : [

                     get_stack_cmd(name),

                      # run server
                      "cd %s ; " % (dragonet_container[name]['base_dir'])
                        + "sudo %s " % (
                            get_isolation_container(is_server=True, cores_needed=(SERVER_CORES * SERVERS_INSTANCES)))
                        + " ./scripts/pravin/runBetterBg.sh 10  ./ ./fancyecho-out.log  "
                        + " ./dist/build/bench-fancyecho/bench-fancyecho %s " % (
                             flows_to_arg_fancyEcho(FLOWS, SERVER_INITIAL_PORT, SERVER_CORES))
                        ,
                      "sleep 4",
                    ],

                    "is_ready_cmd_special" : [
                        "cd %s ; %s %d %d %s %d" % (
                          dragonet_container[name]['base_dir'],
                          dragonet_container[name]['is_stack_ready'],
                          HWQUEUES,
                          dragonet_container[name]['is_ready_wait_events'],
                          "bench-fancyecho",
                          (len(FLOWS))
                          ),
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
                             flows_to_arg_fancyEcho_linux(FLOWS, SERVER_INITIAL_PORT, SERVER_CORES)),
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
#                                "sudo killall fancyEchoLinux || true",
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
                             flows_to_arg_fancyEcho_linux(FLOWS, SERVER_INITIAL_PORT, SERVER_CORES)),
                        "sleep 2",
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'fancyEchoOnload should be already running'",
                    "kill_cmd" : [
                                #"sudo killall fancyEchoLinux || true",
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
                             flows_to_arg_fancyEcho_linux(FLOWS, SERVER_INITIAL_PORT, SERVER_CORES)),
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

