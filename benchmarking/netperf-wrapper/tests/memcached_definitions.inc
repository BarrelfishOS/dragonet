## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

MEMCACHED_PORT =  7777
MEMASLAP_CMD ="sudo ./runBetter.sh memaslap -t%ds  -S%dm " % (
        TOTAL_LENGTH, TOTAL_LENGTH
        )
QUEUES = 4


SERVER_INITIAL_PORT = 7777
CLIENT_INITIAL_PORT = 8000

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
        ret = ret + ("T%d.p[%d]" % (appID, SERVER_INITIAL_PORT))
        ENDPOINTS_TO_WAIT = 1
    else :
        ENDPOINTS_TO_WAIT = len(CLIENTS)
        alreadyCovered = 0
        for kk in range(0, threadsPerPort):
            ret = ret + ("T%d." % (kk))
            if createDedicatedFlows :
                for ii in range(alreadyCovered, alreadyCovered + clientsPerThread):
                    if (ii >= len(sorted_clients)) :
                       break
                    ret = ret + ("f[%s:%d/%s:%d]" % (
                            TARGET, SERVER_INITIAL_PORT,
                            MINFO_CLIENT[sorted_clients[ii]]["EGRESS_INFO"]["src"],
                            sorted_clients_port_mapping[ii]
                        ))
                alreadyCovered = alreadyCovered + clientsPerThread

                # Handling case where number of clients cant equally balanced between threads
                if kk < (len(CLIENTS) % threadsPerPort) :
                    ret = ret + ("f[%s:%d/%s:%d]" % (
                            TARGET, SERVER_INITIAL_PORT,
                            MINFO_CLIENT[sorted_clients[alreadyCovered]]["EGRESS_INFO"]["src"],
                            sorted_clients_port_mapping[alreadyCovered]
                        ))
                    alreadyCovered = alreadyCovered + 1


            else: # createDedicatedFlows
                print "don't create dedicated flows %s" % (ret)
                ret = ret + ("p[%d]" % (SERVER_INITIAL_PORT))
    print "FancyEcho Parameters (updated) : %s" % (ret)
    #sys.exit()
    return ret


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


def genMemcachedParameters(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    unique_clients = list(set(CLIENTS))

    assert(noPorts == 1)

    # Make sure that number of unique clients is more than threadsPerPort
    #if createDedicatedFlows and len(unique_clients) < threadsPerPort :
    #if createDedicatedFlows and len(CLIENTS) < threadsPerPort :
    #if createDedicatedFlows and len(unique_clients) < HWQUEUES:
        #raise Exception("Not enough unique clients (%d) for load balancing per HW queue %d" % (
        #    len(CLIENTS),  threadsPerPort))
    if noPorts > 1:
        raise Exception("Multiport setup requested, which needs some more hacking  noPorts = %d" % (
                noPorts))
        sys.exit()

    if createDedicatedFlows and (len(unique_clients) > HWQUEUES):
        print "Not enough HWQueues to listen dedicatedly per client, so using listen filter instead of flow flilters"
        createDedicatedFlows = False

    if createDedicatedFlows and ( threadsPerPort < len(unique_clients)):
        print "Not enough threads to listen dedicatedly per client, so using listen filter instead of flow flilters"
        createDedicatedFlows = False

    for p in range(0, noPorts):
        portNo = SERVER_INITIAL_PORT + p
        for c in range(0, threadsPerPort):
            if createDedicatedFlows:
                    if c < len(unique_clients) :
                        curr_client = unique_clients[c]
                        curr_client_port = 0
                        ret = ret + ("T%d.f[%s:%d/%s:%d]" % (
                            c, TARGET, portNo, client_to_10G_IP(curr_client),
                            curr_client_port
                            ))
                    else :
                        break
            else: # createDedicatedFlows
                ret = ret + ("T%d.p[%d]" % (c, portNo))
                break

            appID = appID + 1
    print "memcached dragonet Parameters: %s" % (ret)
    sys.exit(1)
    return ret

def genMemcachedParameters2(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    unique_clients = list(set(CLIENTS))

    assert(SERVERS_INSTANCES == 1)
    assert(noPorts == 1)

    # Make sure that number of unique clients is more than threadsPerPort
    if createDedicatedFlows and len(CLIENTS) < threadsPerPort :
        raise Exception("Not enough unique clients (%d) for load balancing per threads %d" % (
            len(CLIENTS),  threadsPerPort))
    if noPorts > 1:
        raise Exception("Multiport setup requested, which needs some more hacking  noPorts = %d" % (
                noPorts))
        sys.exit()

    if USE_TCP :
        active_proto = "-U 0 -p "
    else :
        active_proto = "-p 0 -U "

    ret = ("%s %d -t %d -l %s " % ( active_proto,
                        SERVER_INITIAL_PORT, threadsPerPort, TARGET))
    print "memecached arguments################: %s" % (ret)
    sys.exit(0)
    return ret


def SRV_CMDS(name):


    dragonet_container = {
                "llvmSF":   ["./scripts/pravin/deployDragonetGen.sh", "stack-sf" , "sf"],
                "llvmE10k": ["./scripts/pravin/deployDragonetGen.sh", "stack-e10k", "e10k"],
                }

    dragonet_filter_selector = {
                "llvmSF":   False,
                "llvmE10k": True,
            }




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

                    "init_cmd_special_pinning" : [

                      "cd dragonet/Dragonet/ ; sudo taskset -c %s %s %s %d %s" % (
                          #toCoreList2(range(20 - (HWQUEUES+1), 20)),
                          toCoreList2(range(0, (HWQUEUES+1))),
                          dragonet_container[name][0],
                          dragonet_container[name][2],
                          HWQUEUES,
                          toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                        toCoreList2(range((20 - (SERVERS_INSTANCES*SERVER_CORES)), 20 )))
                        #toCoreList2(range((0 + SERVER_CORESHIFT), (SERVER_CORESHIFT + (SERVERS_INSTANCES*SERVER_CORES)))))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -N %s -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, dragonet_filter_selector[name]),
                            #genMemcachedParameters(SERVERS_INSTANCES, SERVER_CORES, dragonet_filter_selector[name]),
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                       "sleep 4",
                                    ],



                    "init_cmd_special" : [

                      "cd dragonet/Dragonet/ ; sudo %s %s %d " % (
                          dragonet_container[name][0],
                          dragonet_container[name][2],
                          HWQUEUES
                            ),
                      "cd dragonet/Dragonet/ ; "
                        + " sudo ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -N %s -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            genFancyEchoParameters(SERVERS_INSTANCES, SERVER_CORES, dragonet_filter_selector[name]),
                            #genMemcachedParameters(SERVERS_INSTANCES, SERVER_CORES, dragonet_filter_selector[name]),
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                       "sleep 4",
                                    ],


                    "is_ready_cmd_special" : [
                      "cd dragonet/Dragonet/ ; ./scripts/pravin/wait_for_dn_app.sh %d %d %s %s" % (
                            HWQUEUES, TOTAL_CLIENTS,
                            dragonet_container[name][1],
                            "memcached"),
                       "sleep 5",
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
                    "init_cmd" : [
                        #"sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                                ],
                        "exec_cmd" : "sudo ./runBetter.sh %s ../memcached/memcached -c 64000 -m 64000 -u ubuntu " % (onload_prefix),
                        "kill_cmd" : ["sudo killall memcached || true"],
                        "out_cmd" : [],
                }

    if name == "memcached_poll" :
        return {

                    "init_cmd_special" : [
                        #"echo 50 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 50 | sudo tee /proc/sys/net/core/busy_read",
                        "cd dragonet/Dragonet/ ; rm -f ./ethtool_out_1 ./ethtool_out_2",
                        "cd dragonet/Dragonet/ ; ethtool -S %s > %s" % (SERVERS_IF[SERVERS[0]], "./ethtool_out_1"),
                                    ],
                    "init_cmd" : [],
                    "exec_cmd" : "sudo ./runBetter.sh ../memcached/memcached -c 64000 -m 64000 -u ubuntu ",
                    "kill_cmd_special" : [
                        "sudo killall memcached || true",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_poll",
                        #"echo 0 | sudo tee /proc/sys/net/core/busy_read",
                        "cd dragonet/Dragonet/ ; ethtool -S %s > %s " % (SERVERS_IF[SERVERS[0]], "./ethtool_out_2"),
                        "cd dragonet/Dragonet/ ; ./diff_stats.py %s %s " % (
                            "./ethtool_out_1", "./ethtool_out_2"),
                        "cd dragonet/Dragonet/ ; ./diff_stats.py %s %s 1000 | grep rx | grep packets " % (
                            "./ethtool_out_1", "./ethtool_out_2"),
                    ],
                    "kill_cmd" : [],
                    "out_cmd" : []
                }

    if name == "noServer" :
        return {
                    "init_cmd" : [],
                    "exec_cmd" : "echo 'test output'",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                }


