## -*- mode: python; coding: utf-8 -*-


include('dragonet_stack.py')


def getClientPortList(clientList, startPort, noPorts=1) :
    portList = []
    for cl in range(0, len(clientList)):
        prev_entries = [i for i, x in enumerate(clientList[:cl]) if x == clientList[cl]]
        portList.append(startPort + len(prev_entries))
    return portList

ENDPOINTS_TO_WAIT = 0


def flow2Str(f):
    (dst_ip, dst_p, clName, src_p, srv_id, cli_id) = f
    return ("%s:%d/%s:%d" % (
                            dst_ip, dst_p,
                            MINFO_CLIENT[clName]["EGRESS_INFO"]["src"],
                            src_p
                        ))

def flowMap_to_arg_memcached(flowMap):
    ret = ""
    for i in range(0, len(flowMap)):
        ret = ret + ("T%d." % i)
        for f in flowMap[i]:
            ret = ret + ("f[%s]" % (flow2Str(f)) )
    return ret



def balance_flows_sCores_memcached(sThreads, flowsList):
    """ Map flows to server cores, and create proper arguments for it """
    ret = ""

    pp.pprint(flowsList)

    print "%d Flows after balancing in %d threads" % (len(flowsList), sThreads)
    flows_per_sThread = balanced_chunks(flowsList, sThreads)
    pp.pprint(flows_per_sThread)

    arglist = flowMap_to_arg_memcached(flows_per_sThread)
    print "args are "
    print arglist
    #sys.exit(0)
    return arglist


# remnant of old code, can be deleted safely
def client_to_10G_IP(client) :
    # NOTE: It is assumed that multi-NIC clients will append the IP address in
    #       their name as follows
    #       burrata--10.113.4.96 burrata--10.113.4.196

    isMultiNICMachine =  client.find("--")
    if isMultiNICMachine  == -1 :
        return MINFO_CLIENT[client]["EGRESS_INFO"]["src"]
        # return "ANY"
    else :
        return client[isMultiNICMachine+2:]


def SRV_CMDS(name):


    if name == "memcached" :
        return {
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                        "kill_cmd" : ["sudo killall memcached || true"],
                        "out_cmd" : [],
                }

    if name in  dragonet_container.keys() :
        return {

                    "init_cmd_special" : [

                      # run stack
                      "cd %s ; " % (dragonet_container[name]['base_dir'])
                        + "sudo %s " % (get_isolation_container(is_server=False))
                        + " %s %d %s " % (
                          dragonet_container[name]['deploy_stack'],
                          HWQUEUES,
                          "priority"
                          #"balance"
                          ),


                      # run server
                      "cd %s ; " % (
                        dragonet_container[name]['base_dir'])
                        + "sudo %s " % (get_isolation_container(is_server=False))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -N %s " % (
                            balance_flows_sCores_memcached(SERVER_CORES, FLOWS))
                        + " -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),

                      "sleep 4",
                     ],


                    "is_ready_cmd_special" : [
                        "cd %s ; %s %d %d %s " % (
                          dragonet_container[name]['base_dir'],
                          dragonet_container[name]['is_stack_ready'],
                          HWQUEUES,
                          len(FLOWS),
                          "memcached"),
                     ],


                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "kill_cmd" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/memcached-out.log",
                                #"sudo killall memcached || true",
                                #"sudo killall %s || true" % (dragonet_container[name][1]),
                                 ],
                    "out_cmd" : [],
                }



    if name == "memcached_dn" :
        return {
                    "init_cmd" : [],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -N -c 64000 -m 64000 -u ubuntu ",
                    "kill_cmd" : [
                                "sudo killall memcached || true",
                                "sudo killall llvm-cgen-e10k || true",
                                 ],
                    "out_cmd" : [],
                }


    if name == "memcached_onload" :
        return {

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " %s ../benchmarking/memcached/memcached -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            onload_prefix, "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                      "sleep 2",
                      "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'memcached should be already running'",
                    "kill_cmd" : [
                       "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "sudo memcached || true",
                                 ],
                    "out_cmd" : [],
                }


    if name == "memcached_rss" :
        return {

                    "init_cmd_special" : [
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                      "sleep 2",
                      "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'memcached should be already running'",
                    "kill_cmd" : [
                       "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "sudo memcached || true",
                                 ],
                    "out_cmd" : [],
                }



    if name == "memcached_poll" :
        return {

                    "init_cmd_special" : [
                      "echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                      "echo 50 | sudo tee  /proc/sys/net/core/busy_read",

                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                          toCoreList2(range(0, SERVERS_INSTANCES*SERVER_CORES)))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                      "sleep 2",
                      "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'memcached should be already running'",
                    "kill_cmd" : [
                       "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "sudo memcached || true",
                       "echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                       "echo 0 | sudo tee  /proc/sys/net/core/busy_read",
                                 ],
                    "out_cmd" : [],
                }




    if name == "noServer" :
        return {
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }


def get_proto_specific_server_ports():
    """retuns values for to be used for ports at server end """
    port_list = range(SERVER_INITIAL_PORT, (SERVER_INITIAL_PORT + (len(server_names))))
    port_list_empty = [0 for x in port_list]
    port_list_for_clients = repeat_list_elem_n_times(port_list, len(CLIENTS))
    port_list_cl_src = []
    port_list_cl_lip = []
    for cidx in range(0, len(CLIENTS)):
        prev_entries = [i for i, x in enumerate(CLIENTS[:cidx]) if x == CLIENTS[cidx]]
        port_list_cl_src.append(CLIENT_INITIAL_PORT + (len(prev_entries) * CONCURRENCY))
        # if client name contains --, then use pafr after -- as ip
        #      otherwise use "ANY"
        port_list_cl_lip.append(client_to_10G_IP(CLIENTS[cidx]))

    print "port_list_for_clients: %s" % (port_list_for_clients)
    print "port_list_for_clients src: %s" % (port_list_cl_src)
    print "local IP_list_for multiNIC_clients  %s" % (port_list_cl_lip)

    if (USE_TCP) :
        return (port_list_cl_lip, port_list_cl_src, port_list_for_clients, port_list,       port_list_empty, "")
    else:
        return (port_list_cl_lip, port_list_cl_src, port_list_for_clients, port_list_empty, port_list,       " --udp ")



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

       ( 'memcached%d' % (id),
       {

           'command': 'sudo taskset -c %s %s -p %d -U %d -t %d -l %s' % (
                toCoreList(server_core_list[id]),
                echo_server_cmds['exec_cmd'],
                MEMCACHED_PORT_TCP[id],     # -p
                MEMCACHED_PORT_UDP[id],     # -U
                len(server_core_list[id]),  # -t (no of threads)
                TARGET                      # -l (listen address)
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


