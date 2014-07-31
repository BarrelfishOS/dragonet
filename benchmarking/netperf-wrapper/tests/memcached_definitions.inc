## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH + 2

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

MEMCACHED_PORT =  7777
MEMASLAP_CMD ="sudo ./runBetter.sh memaslap -t%ds --cfg_cmd=../libmemcached-1.0.18/bmKey_64_val_1024.conf -S%dm " % (
        TOTAL_LENGTH, TOTAL_LENGTH
        )
QUEUES = 4


SERVER_INITIAL_PORT = 7777

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




def genMemcachedParameters(noPorts, threadsPerPort, createDedicatedFlows = True):
    ret = ""
    # socket info per slot
    appID = 0
    unique_clients = list(set(CLIENTS))

    assert(noPorts == 1)

    # Make sure that number of unique clients is more than threadsPerPort
    #if createDedicatedFlows and len(unique_clients) < threadsPerPort :
    if createDedicatedFlows and len(unique_clients) < HWQUEUES:
        raise Exception("Not enough unique clients (%d) for load balancing per HW queue %d" % (
            len(CLIENTS),  threadsPerPort))
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
            #if c == 0 :
                #        ret = ret + ("p[%d]" % (portNo))
            #else :
                    #prev_entries = [i for i, x in enumerate(CLIENTS[:c]) if x == CLIENTS[c]]
                    #curr_client = CLIENTS[c]
                    #curr_client_port = (CLIENT_INITIAL_PORT + len(prev_entries))
                    curr_client = unique_clients[c%len(unique_clients)]
                    curr_client_port = 0
                    ret = ret + ("f[%s:%d/%s:%d]" % (
                            TARGET, portNo,
                            MINFO_CLIENT[curr_client]["EGRESS_INFO"]["src"],
                            curr_client_port
                        ))
            else: # createDedicatedFlows
                ret = ret + ("p[%d]" % (portNo))

            appID = appID + 1
    print "memcached dragonet Parameters: %s" % (ret)
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
    print "memecached arguments: %s" % (ret)
    return ret


def SRV_CMDS(name):

    dragonet_container = {
                "llvmSF":   ["./scripts/pravin/deployDragonetSF.sh", "stack-sf" ],
                "llvmE10k": ["./scripts/pravin/deployDragonetE10k.sh", "stack-e10k" ],
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

                    "init_cmd_special" : [

                      "cd dragonet/Dragonet/ ; sudo taskset -c %s %s %d %s" % (
                          toCoreList2(range(20 - (HWQUEUES * 2), 20)),
                          dragonet_container[name][0],
                          HWQUEUES, toCoreList2(range(0, (SERVERS_INSTANCES *SERVER_CORES)), separator=" ", prefix="t")
                            ),
                      "sleep 4",
                      "cd dragonet/Dragonet/ ; sudo taskset -c %s " % (
                        toCoreList2(range((0 + SERVER_CORESHIFT), (SERVER_CORESHIFT + (SERVERS_INSTANCES*SERVER_CORES)))))
                        + " ./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log  "
                        + " ../benchmarking/memcached/memcached -N %s -c 64000 -m 64000 -u root %s %d -t %d -l %s " % (
                            genMemcachedParameters(SERVERS_INSTANCES, SERVER_CORES, True),
                            "-p 0 -U ",
                            SERVER_INITIAL_PORT, SERVER_CORES, TARGET),
                      "sleep 4",
                                    ],

                    "is_ready_cmd_special" : [
                                        "sleep 4 ",
                                        #"cd dragonet/Dragonet/ ; ./wait_for_dn_app.sh %d %d ./ " % (HWQUEUES, SERVERS_INSTANCES * SERVER_CORES),
                                    ],

                    "init_cmd" : [],
                    "exec_cmd" : "echo 'Server should already be running'" ,
                    "kill_cmd" : [
                                "tail dragonet/Dragonet/some.log",
                                "tail dragonet/Dragonet/memcached-out.log",
                                "sudo killall memcached || true",
                                "sudo killall %s || true" % (dragonet_container[name][1]),
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


