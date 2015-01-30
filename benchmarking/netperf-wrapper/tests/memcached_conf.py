## -*- mode: python; coding: utf-8 -*-




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

def flows_to_arg_memcached(flows, listenPort):
    ret = "T0.p[%d]" % (listenPort)
    for f in flows:
        ret = ret + ("F[%s]" % (flow2Str(f)) )
    return ret


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

                     get_stack_cmd(name),

                      # run server
                      "cd %s ; " % (
                        dragonet_container[name]['base_dir'])
                        + "sudo %s " % (
                            get_isolation_container(is_server=True,
                                # giving an extra core for polling queue-0
                                cores_needed=((SERVERS_INSTANCES*SERVER_CORES) + 1)
                                #cores_needed=((SERVERS_INSTANCES*SERVER_CORES))
                                    ))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -N %s " % (
                            flows_to_arg_memcached(FLOWS, SERVER_INITIAL_PORT))
                        + " -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET)
                        ,

                      "sleep 4",
                     ],


                    "is_ready_cmd_special" : [
                        "cd %s ; %s %d %d %s %d" % (
                          dragonet_container[name]['base_dir'],
                          dragonet_container[name]['is_stack_ready'],
                          HWQUEUES,
                          dragonet_container[name]['is_ready_wait_events'],
                          "memcached",
                          ((CONCURRENCY) * len(client_names))
                            ),
                     ],


                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "kill_cmd" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/memcached-out.log",
                                "cat dragonet/Dragonet/stack.filtready || true",
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


    if name == "memcached_linux" :
        return {

                    "init_cmd_special" : [

                      # run server
                      "cd %s ; " % (
                        "dragonet/Dragonet/ ")
                        + "sudo %s " % (get_isolation_container(is_server=True,
                                cores_needed=((SERVERS_INSTANCES*SERVER_CORES)))
                            )
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached "
                        + " -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET)
                        ,

                      "sleep 2",

                      "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                     ],

                    "is_ready_cmd_special" : [],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'memcached should be already running'",
                    "kill_cmd" : [
                       "cd dragonet/Dragonet/ ; ethtool -S %s | tee %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       "cd dragonet/Dragonet/ ; ../benchmarking/netperf-wrapper/diff_stats.py %s %s 1000 | grep rx_packets " % (
                           "./ethtool_out_1", "./ethtool_out_2"),
                       #"sudo killall memcached || true",
                                 ],
                    "out_cmd" : [],
                }



    if name == "memcached_onload" :
        return {


                    "init_cmd_special" : [


                      # run server
                      "cd %s ; " % (
                        "dragonet/Dragonet/ ")
                        + "sudo %s " % (get_isolation_container(is_server=True,
                                cores_needed=((SERVERS_INSTANCES*SERVER_CORES)))
                            )
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " %s ../benchmarking/memcached/memcached " % (onload_prefix)
                        + " -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET)
                        ,

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
                       #"sudo killall memcached || true",
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
                    "kill_cmd" : [
                                "cat dragonet/Dragonet/stack.filtready || true",
                        ],
                    "out_cmd" : [],
                }



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

           'command':  'sudo %s ' % (get_isolation_container(is_server=True,
                            cores_needed=((SERVERS_INSTANCES*SERVER_CORES))))
                + ' %s -p %d -U %d -t %d -l %s ' % (
                    echo_server_cmds['exec_cmd'],
                    0,                          # -p (TCP)
                    CLIENT_INITIAL_PORT,        # -U
                    SERVER_CORES,
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
               cpus=get_isolation_container(is_server=True,
                       cores_needed=((SERVERS_INSTANCES*SERVER_CORES))
                       )[len(' testset -c '):],
               netdev=server_if[server_names[id]],
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
#               cpus=get_isolation_container(is_server=True,
#                       cores_needed=((SERVERS_INSTANCES*SERVER_CORES))
#                        )[len(' testset -c '):]
#               netdev=server_if[server_names[id]],
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


