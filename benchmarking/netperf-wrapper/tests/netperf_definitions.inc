## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH+2*DELAY
NETPERF_CMD="../netperf-2.6.0/src/netperf -fg "

SRV_CMDS = {
    "netserver" : {
                    "init_cmd" : ["sudo killall netserver || true"],
                    "exec_cmd" : "../netperf-2.6.0/src/netserver -D ",
                    "kill_cmd" : ["sudo killall netserver || true"],
                    "out_cmd" : [],
                },
    "netcat" : {
                    "init_cmd" : ["sudo killall nc.traditional || true"],
                    "exec_cmd" : "nc.traditional -nlup 7 -e /bin/cat",
                    "kill_cmd" : ["sudo killall nc.traditional || true"],
                    "out_cmd" : [],
                },
    "HImplOnload" : {
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "CImplOnload" : {
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "HImplDpdk" : {
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
    "CImplDpdk" : {
                    "init_cmd" : [],
                    "exec_cmd" : "echo ",
                    "kill_cmd" : [],
                    "out_cmd" : [],
                },
}

PING4="ping"
PING6="ping6"
PING="ping"
if IP_VERSION == 4:
    NETPERF_CMD += " -4"
elif IP_VERSION == 6:
    NETPERF_CMD += " -6"
    PING="ping6"

