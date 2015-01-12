#!/usr/bin/env python
import pprint as PP
import gzip
import json
from pprint import pprint
import subprocess as SP
import os
import sys
import pylab
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec

home = os.path.expanduser("~")
resultDir = "./dpdk_5tuple_results/"
DEBUG = True

def get_files(resultDir, workload, testType, concurrency, pktsize, nic,
        clients):

    rlocation = "%s/%s/" % (resultDir, workload)
#              '*Test_memcached_rr_CONCUR_*_PKT_1024_*_Intel_SRV_*_CLC_40*'
    pattern = "*%s*Test_%s_CONCUR_%s_PKT_%s_*%s_SRV_%s_CLC_%s*" % (
            workload.title(), testType, concurrency, pktsize, nic,
            '*',  clients)

    if not os.path.isdir(rlocation):
        print "WARNING: invalid location [%s] (needed for pattern [%s])" % (
                rlocation, pattern)
        return []

    try:
        flist =  SP.check_output(["find", rlocation,
                "-name", pattern]).decode("utf-8").split()
    except:
        print "WARNING: No files found for pattern [%s]" % (pattern)
        raise
        return []

    if DEBUG:
        print "files used for pattern [%s] bellow" % (pattern)
        PP.pprint(flist)
    return flist[0]




metric_expl = {
    "Total_Avg" : "Average latency of READ/WRITE operations in memcached"
}

metric_units = {
    "THROUGHPUT" : "Gbit/s",
    "MEAN_LATENCY" : "usecs",
    "Total_Avg" : "usecs",
}

metric_error = {
    #"Total_Avg" : "Total_Std",
    #"MEAN_LATENCY" : "STDDEV_LATENCY",
}

def clientId(c):
    if len(c.split('-')) > 1 :
        return int(c.split('-')[1])
    return int(c[6:])

def plot_per_client(fnames, fids, pid, app, metric, client_class=None):

    def get_result(client, res):
        cid = clientId(c)
        #results[client]
        clientName = client[6:]
        #return results[client][app + str(cid)]["CMD_OUTPUT"][res]
        return results[client][app + clientName]["CMD_OUTPUT"][res]

    def xcmp(x,y):
        xi = clientId(x)
        yi = clientId(y)
        return cmp(xi,yi)

    outputFileList = [
            ("nsdiplotsV3/" + pid + "-%s-%s.png" % (app,metric)),
        ]

    print "plot per client called"
    colors = ["r", "b", "g"]
    bwidth = .4

    f = gzip.open(fnames[0], 'r')
    j = json.load(f)
    clients = filter(lambda x : x.startswith("client"), j["results"]["1"].keys())
    nclients = len(clients)
    index = np.arange(nclients)

    fig = plt.figure(figsize=(10,2))
    fig.clf()
    gs = gridspec.GridSpec(1,2, width_ratios=[4,1])
    ax = plt.subplot(gs[0])
    ax.tick_params(axis='both', which='major', labelsize=7)

    idx = 0
    xresults = {}
    box = ax.get_position()
    for fname in fnames:
        f = gzip.open(fname, 'r')
        j = json.load(f)
        results = j["results"]["1"]

        clients =  filter(lambda x : x.startswith("client"), results.keys())
        print clients

        notClients =  filter(lambda x : not x.startswith("client"), results.keys())
        print "and not client keys are"
        print notClients

        clients.sort(cmp=xcmp)
        #client_ids = [clientId(c) for c in clients]

        yvals = [float(get_result(c,metric)) for c in clients ]
        yerr  = None
        if metric in metric_error:
            error = metric_error[metric]
            errs = [float(get_result(c,error)) for c in clients]
            yerr = [errs, errs]
        xresults[fids[idx]] = yvals
        #plt.figure(figsize=(10,2))
        xvals = index + (idx*bwidth)
        #xvals = xvals[:len(yvals)]
        #print "xals are: %d" % (len(xvals))
        #print xvals
        #print "and yvals are: %d"  % (len(yvals))
        #print "yerr is: %s "  % (yerr)
        #print yvals
        ax.bar(xvals,
               yvals,
               width=bwidth,
               label = fids[idx],
               color = colors[idx],
               yerr=yerr,
               error_kw=dict(ecolor='black'),
               #align="center"
        )
        idx += 1


    fig.suptitle(metric_expl.get(metric,metric) + "(%s)" % metric_units.get(metric,""), fontsize=9)
    #plt.title("for all clients", fontsize=7)

    ax.set_xlabel("Flows", fontsize=7)
    ax.set_ylabel(metric, fontsize=7)
    xticks_ = index + (bwidth)
    ax.legend(loc=1,prop={'size':7})


    ax2 = plt.subplot(gs[1])

    ax2.boxplot([xresults[fid] for fid in fids])
    ax2.tick_params(axis='both', which='major', labelsize=7)
    ax2.set_xticklabels(fids)
    ax2.set_xlabel("all flows", fontsize=7)
    ax2.set_ylabel(metric, fontsize=7)

    plt.tight_layout()

    for outputFile in outputFileList:
        # Checking if the parent directory exists or not.
        parentDir = os.path.dirname(outputFile)
        #print "the parentdir for output file [%s] is %s" % (outputFile, parentDir)
        if parentDir != '' :
            if not os.path.isdir(parentDir):
                os.makedirs(parentDir)
        print "Saving plot in a file %s" % (outputFile)
        plt.savefig(outputFile, bbox_inches='tight')

def plot_echoserver():
    resultLocation = "./dpdk_fdir_results/"
    fresults_echoServer = {
        "Intel-1k-40-32"     : get_files(resultDir = resultLocation,
                                workload = "priority", testType = "udp_rr",
                                concurrency = 32, pktsize = 1024, nic = "Intel",
                                clients = 40),
            }

    def plot_echo(pprefix, fids, metric):
        fnames = [fresults_echoServer[fid] for fid in fids]
        plot_per_client(fnames, fids, pprefix, "netperf", metric)

    for metric in ("MEAN_LATENCY", "THROUGHPUT"):
        plot_echo("Intel-1k-40-32", ["Intel-1k-40-32"], metric)


def plot_memcached():

    resultLocation = "./nsdi_data_local_copy/"
    resultLocation = "./dpdk_test_big_memcached/DUMMY/"
    fresults_memcached = {
        "Intel-1k-40-32"     : get_files(resultDir = resultLocation,
                                workload = "", testType = "memcached_rr",
                                concurrency = 32, pktsize = 1024, nic = "Intel",
                                clients = 40),
        "Intel-1k-8-100"     : get_files(resultDir = resultLocation,
                                workload = "", testType = "memcached_rr",
                                concurrency = 100, pktsize = 1024, nic = "Intel",
                                clients = 8),
    }



    def plot_memcached(pprefix, fids, metric):
        fnames = [fresults_memcached[fid] for fid in fids]
        plot_per_client(fnames, fids, pprefix, "memaslap", metric)

    for metric in ("TPS", "Total_Avg"):
        plot_memcached("Intel-1k-40-32", ["Intel-1k-40-32"], metric)
        plot_memcached("Intel-1k-8-100", ["Intel-1k-8-100"], metric)

if __name__ == "__main__":
    plot_echoserver()
    plot_memcached()
