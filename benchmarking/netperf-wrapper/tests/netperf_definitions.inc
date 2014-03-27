## -*- mode: python; coding: utf-8 -*-

AGGREGATOR="timeseries"
TOTAL_LENGTH=LENGTH+2*DELAY
NETPERF_CMD="../netperf-2.6.0/src/netperf -fg "

PING4="ping"
PING6="ping6"
PING="ping"
if IP_VERSION == 4:
    NETPERF_CMD += " -4"
elif IP_VERSION == 6:
    NETPERF_CMD += " -6"
    PING="ping6"

