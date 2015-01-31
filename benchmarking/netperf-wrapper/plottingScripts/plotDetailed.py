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

pp = PP.PrettyPrinter(indent=4)

home = os.path.expanduser("~")
plot_save_location = "./memcachedLoadPlots/"
DEBUG = True


def get_files(resultDir, workload, testType, concurrency, pktsize, nic,
        clients, srvtype="*"):

    rlocation = "%s/%s/" % (resultDir, workload)
    pattern = "*%s*Test_%s_CONCUR_%s_PKT_%s_*%s_SRV_%s_CLC_%s*json*" % (
            workload.title(), testType, concurrency, pktsize, nic, srvtype,
            clients)

    grep_pattern = ""
    if not os.path.isdir(rlocation):
        print "WARNING: invalid location [%s] (needed for pattern [%s])" % (
                rlocation, pattern)
        return []

    try:
        flist =  SP.check_output(["find", rlocation,
                "-name", pattern]).decode("utf-8").split()

        #flist =  SP.check_output(["./findGrep.sh", rlocation, pattern,
        #   grep_pattern]).decode("utf-8").split()

    except:
        print "WARNING: No files found for ./findGrep.sh %s '%s' '%s'" % (
                rlocation, pattern, grep_pattern)
        raise
        return []

    if DEBUG:
        print "files used for [find %s -name '%s'] are bellow" % (
                rlocation, pattern)
        PP.pprint(flist)
    if len(flist) != 1:
        print "ERROR: There is more than 1 matching file found!"
        PP.pprint(flist)
        sys.exit(1)
    return flist[0]

metric_expl = {
    "Total_Avg" : "Mean latency",
    "THROUGHPUT" : "Throughput",
    "MEAN_LATENCY" : "Mean latency",
    "TPS" : "TPS",
    "kTPS" : "Throughput"
}

metric_units = {
    "THROUGHPUT" : "Gbit/s",
    "MEAN_LATENCY" : "usecs",
    "Total_Avg" : "usecs",
    "kTPS" : "kTx/sec"
}

metric_error = {
    #"Total_Avg" : "Total_Std",
    #"MEAN_LATENCY" : "STDDEV_LATENCY",
}

def xticks_map(x):
    if x.startswith("dnet"):
        if "onlypriority" in x:
            return "Load Balance\n(Only HP)"
        if "prio" in x:
            return "Priority"
            #return "Conque:Prio"
        else:
            return "Load Balance\n(ALL)"
            #return "Conque:Lb"
    elif x.startswith("linuxOnload"):
        return "Onload"
    elif x.startswith("linux"):
        return "Linux"

def is_only_one_column(x):
    if "onlypriority" in x:
        return True
    if "balance" in x:
        return True
    return False

def clientName(c):
    return (c[6:])


def clientId(c):
    if len(c.split('-')) > 1 :
        return int(c.split('-')[1])
    return int(c[6:])

def get_result(results, app, client, res):
    cid = clientId(client)
    clientName = client[6:]
    return results[client][app + clientName]["CMD_OUTPUT"][res]

def xcmp(x,y):
    xi = clientId(x)
    yi = clientId(y)
    return cmp(xi,yi)


def plot_per_client(fnames, fids, pid, app, metric, client_class=None):

    outputFileList = [
            ( plot_save_location + pid + "-%s-%s.png" % (app,metric)),
        ]


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
    #ax.tick_params(axis='both', which='major', labelsize=)

    idx = 0
    xresults = {}
    box = ax.get_position()
    for fname in fnames:
        f = gzip.open(fname, 'r')
        j = json.load(f)
        results = j["results"]["1"]

        clients =  filter(lambda x : x.startswith("client"), results.keys())
        clients.sort(cmp=xcmp)
        print "sorted clients are "
        PP.pprint(clients)

        client_ids = [clientId(c) for c in clients]

        yvals = [float(get_result(results,  app, c,metric)) for c in clients ]
        yerr  = None
        if metric in metric_error:
            error = metric_error[metric]
            errs = [float(get_result(results,  app, c,error)) for c in clients]
            yerr = [errs, errs]
        xresults[fids[idx]] = yvals
        #plt.figure(figsize=(10,2))
        xvals = index + (idx*bwidth)
        #xvals = xvals[:len(yvals)]
        #print xvals
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


    if metric in metric_units:
        u = "(%s)" % metric_units[metric]
    else:
        u =""
    fig.suptitle(metric_expl.get(metric,metric) + u, fontsize=9)
    #plt.title("for all clients", fontsize=7)

    ax.set_xlabel("Flows", fontsize=7)
    ax.set_ylabel(metric, fontsize=7)
    xticks_ = index + (bwidth)
    ax.legend(loc=1,prop={'size':7})

    ax2 = plt.subplot(gs[1])

    ax2.boxplot([xresults[fid] for fid in fids])
    ax2.tick_params(axis='both', which='major', labelsize=10)
    ax2.set_xticklabels(fids)
    #ax2.set_xlabel("all flows", fontsize=7)
    ax2.set_ylabel(metric, fontsize=7)

    for outputFile in outputFileList:
        # Checking if the parent directory exists or not.
        parentDir = os.path.dirname(outputFile)
        #print "the parentdir for output file [%s] is %s" % (outputFile, parentDir)
        if parentDir != '' :
            if not os.path.isdir(parentDir):
                os.makedirs(parentDir)
        print "Saving plot in a file %s" % (outputFile)
        plt.savefig(outputFile, bbox_inches='tight')
        # To create cropped files
        if outputFile[-4:] == ".pdf":
            SP.check_output(["pdfcrop", outputFile])
    plt.close(fig)

def just_boxplot(fnames, fids, pid, app, metric, client_class=None, y_log_scale=None,
                 hp_clients=None):
    outputFileList = [
            ( plot_save_location + pid + "-%s-%s.png" % (app,metric)),
            ( plot_save_location + pid + "-%s-%s.pdf" % (app,metric)),
        ]

    print "DOING: " + pid

    colors = ["r", "b", "g"]
    bwidth = .4

    print "files with results are %s" % (fnames)
#    f = gzip.open(fnames[0], 'r')
#    j = json.load(f)
#    clients = filter(lambda x : x.startswith("client"), j["results"]["1"].keys())
#    nclients = len(clients)
#    index = np.arange(nclients)
#
    fig = plt.figure(figsize=(4,2))
    pidx = 0
    idx = 0
    kmetric = False
    if metric == "TPS":
        kmetric = True
    xresults = {}
    xresults_hp = {}
    xresults_lp = {}
    hp_clients_fid = {}
    hp_clients_x = hp_clients

    for fname in fnames:
        f = gzip.open(fname, 'r')
        j = json.load(f)
        results = j["results"]["1"]

        clients =  filter(lambda x : x.startswith("client"), results.keys())
        clients.sort(cmp=xcmp)
        #print "sorted clients are "
        #PP.pprint(clients)

        client_ids = [clientId(c) for c in clients]

        yvals = [float(get_result(results,  app, c,metric)) for c in clients ]
        if kmetric:
                yvals_hp = [y/1000.0 for y in yvals]

        if hp_clients_x is not None:
            yvals_hp = [ v for (c,v) in zip(client_ids, yvals) if c in hp_clients_x ]
            yvals_lp = [ v for (c,v) in zip(client_ids, yvals) if c not in hp_clients_x ]
            print "HP:", yvals_hp
            print "LP:", yvals_lp
            if kmetric:
                yvals_hp = [y/1000.0 for y in yvals_hp]
                yvals_lp = [y/1000.0 for y in yvals_lp]

            if (len(yvals_lp) > 0):

                xresults_hp[fids[idx]] = yvals_hp
                xresults_lp[fids[idx]] = yvals_lp
                hp_clients_fid[fids[idx]] = hp_clients_x
            else :
                xresults_hp[fids[idx]] = yvals_hp
                xresults_lp[fids[idx]] = yvals_lp
                xresults[fids[idx]] = yvals_hp
                hp_clients_fid[fids[idx]] = None

        xresults[fids[idx]] = yvals

        yerr  = None
        if metric in metric_error:
            error = metric_error[metric]
            errs = [float(get_result(results,  app, c,error)) for c in clients]
            yerr = [errs, errs]

        #xvals = index + (idx*bwidth)
        #print xvals
        idx += 1

    #xticks_ = index + (bwidth)
    ax2 = plt.subplot()
    if hp_clients is not None:
        r = []
        for fid in fids:
            #if hp_clients_fid[fid] is not None:
            if is_only_one_column(fid) == False:
                r.append(xresults_hp[fid])
                r.append(xresults_lp[fid])
            else:
                r.append((xresults_hp[fid] + xresults_lp[fid]))
                #r.append(xresults[fid])
        ax2.boxplot(r)
        #my_ys = []
        #my_xs = []
        #idx = 1
        #for fid in fids:
        #    my_ys += xresults_hp[fid]
        #    my_xs += [idx for _ in xresults_hp[fid]]
        #    my_ys += xresults_lp[fid]
        #    my_xs += [idx+1 for _ in xresults_lp[fid]]
        #    idx += 2
        #ax2.plot(my_xs, my_ys, 'x')
    else:
        ax2.boxplot([xresults[fid] for fid in fids])

    if y_log_scale is not None:
        ax2.set_yscale('log')

    ax2.tick_params(axis='both', which='major', labelsize=8)

    if hp_clients_x is not None:
        xlbls = []
        for fid in fids:
            #if hp_clients_fid[fid] is not None or is_only_one_column(fid) == False:
            if is_only_one_column(fid) == False:
                print "using labels as %s HP, %s LP " % ( xticks_map(fid),
                    xticks_map(fid))
                xlbls.append("%s" % (xticks_map(fid)) + "\n(HP)")
                xlbls.append("%s" % (xticks_map(fid)) + "\n(BE)")
                #xlbls.append("%s" % (xticks_map(fid)) )
                #xlbls.append("%s" % (xticks_map(fid)))
                ax2.set_xticklabels(xlbls)
            else:
                xlbls.append("%s" % (xticks_map(fid)))
                ax2.set_xticklabels(xlbls)

    else:
        ax2.set_xticklabels(map(xticks_map, fids))

    #ax2.set_xlabel("all flows", fontsize=3)
    if kmetric:
        metric = "k" + metric

    if metric in metric_units:
        u = "(%s)" % metric_units[metric]
    else:
        u =""
    ylabel = metric_expl.get(metric,metric) + u
    ax2.set_ylabel(ylabel, fontsize=11)

    plt.grid()
    #plt.tight_layout()
    #plt.savefig("nsdiplots/" + pid + "-%s-%s.pdf" % (app,metric), bbox_inches='tight')
    #plt.savefig(output_dir_real + pid + "-%s-%s.pdf" % (app,metric), bbox_inches='tight')
    for outputFile in outputFileList:
        # Checking if the parent directory exists or not.
        parentDir = os.path.dirname(outputFile)
        #print "the parentdir for output file [%s] is %s" % (outputFile, parentDir)
        if parentDir != '' :
            if not os.path.isdir(parentDir):
                os.makedirs(parentDir)
        print "Saving plot in a file %s" % (outputFile)
        #plt.savefig(outputFile, bbox_inches='tight')
        plt.savefig(outputFile, bbox_inches='tight', dpi=300)
        # To create cropped files
        if outputFile[-4:] == ".pdf":
            SP.check_output(["pdfcrop", outputFile])
    plt.close(fig)






def plot_memcached():

    resultLocation = "./nsdi_data_local_copy/"
    resultLocation = "./dpdk_test_deleteme/DUMMY/"
    resultLocation = "./dpdk_test_memcached/DUMMY/priority/"
    resultLocation = "./dpdk_test_memcached_test/STARTSTACK/priority/"
    resultLocation = "./dpdk_test_memcached/priority/"
    fresults_memcached = {
        "Intel-1k-40-32"     : get_files(resultDir = resultLocation,
                                workload = "", testType = "memcached_rr",
                                concurrency = 32, pktsize = 1024, nic = "Intel",
                                clients = 40),
        "SF-1k-40-32"     : get_files(resultDir = resultLocation,
                                workload = "", testType = "memcached_rr",
                                concurrency = 32, pktsize = 1024, nic = "SF",
                                clients = 40),
    }

#    fresults_memcached = {
#        "SF-1k-12-2"     : get_files(resultDir = resultLocation,
#                                workload = "", testType = "memcached_rr",
#                                concurrency = 2, pktsize = 1024, nic = "SF",
#                                clients = 12),
#    }
#


    def plot_memcached(pprefix, fids, metric):
        fnames = [fresults_memcached[fid] for fid in fids]
        plot_per_client(fnames, fids, pprefix, "memaslap", metric)

    for metric in ("TPS", "Total_Avg"):
        plot_memcached("Intel-1k-40-32", ["Intel-1k-40-32"], metric)
        plot_memcached("SF-1k-40-32", ["SF-1k-40-32"], metric)
#        plot_memcached("SF-1k-20-32", ["SF-1k-20-32"], metric)
#        plot_memcached("SF-1k-12-2", ["SF-1k-12-2"], metric)


def plot_memcached_test(clients, concurrency, nic, resultLocation, pktsize, prefix=''):

    title =  "dnet-prio-%s-%s-1k-%d-%d-pkt-%d" % (prefix, nic, clients, concurrency, pktsize)
    fresults_memcached = {
        title     : get_files(resultDir = resultLocation,
                                workload = "", testType = "memcached_rr",
                                concurrency = concurrency, pktsize = pktsize,
                                nic = nic,
                                clients = clients),
    }


    def plot_memcached(pprefix, fids, metric):
        fnames = [fresults_memcached[fid] for fid in fids]
        #plot_per_client(fnames, fids, pprefix, "memaslap", metric)
        just_boxplot(fnames, fids, pprefix, "memaslap", metric,
                #y_log_scale=True,
                hp_clients=[0,1])

    for metric in ("TPS", "Total_Avg"):
#        plot_memcached("SF-1k-12-2", ["SF-1k-12-2"], metric)
        plot_memcached(title, [title], metric)

only_show_files = False


#        def plot_memcached_gen(clients, concurrency, nic,
#                    config,
def plot_memcached_priority_balance(clients, concurrency, nic,
                priorityOnly,
                priorityPath,
                balancePath,
                pktsize,
                prefix=''):

    titlePriorityOnly =  "dnet-onlypriority-%s-%s-1k-%d-%d-pkt-%d" % (
            prefix, nic, 2, concurrency, pktsize)
    titlePriority =  "dnet-priority-%s-%s-1k-%d-%d-pkt-%d" % (
            prefix, nic, clients, concurrency, pktsize)
    titleBalance =  "dnet-balance-%s-%s-1k-%d-%d-pkt-%d" % (
            prefix, nic, clients, concurrency, pktsize)
    fresults_memcached = {
        titleBalance : get_files(resultDir = balancePath,
                                workload = "", testType = "memcached_rr",
                                concurrency = concurrency, pktsize = pktsize,
                                nic = nic,
                                clients = clients),
        titlePriority : get_files(resultDir = priorityPath,
                                workload = "", testType = "memcached_rr",
                                concurrency = concurrency, pktsize = pktsize,
                                nic = nic,
                                clients = clients),
        titlePriorityOnly : get_files(resultDir = priorityOnly,
                                workload = "", testType = "memcached_rr",
                                concurrency = concurrency, pktsize = pktsize,
                                nic = nic,
                                clients = 2),

    }

    def show_used_files(cmdPrefix, fids):
        fnames = [fresults_memcached[fid] for fid in fids]
        for f in fnames:
            print "%s %s" % (cmdPrefix, f)

    def plot_memcached(pprefix, fids, metric):
        #show_used_files(cmdPrefix = "git add ", fids = fids)
        #if only_show_files:
        #    return

        fnames = [fresults_memcached[fid] for fid in fids]
        #plot_per_client(fnames, fids, pprefix, "memaslap", metric)
        just_boxplot(fnames, fids, pprefix, "memaslap", metric,
                #y_log_scale=True,
                hp_clients=[0,1])

    for metric in ("TPS", "Total_Avg"):
#        plot_memcached("SF-1k-12-2", ["SF-1k-12-2"], metric)
        #plot_memcached(title, [title], metric)

        plot_memcached("memcached-%s-pkt-%d-cl-%d-load-%d" % (nic, pktsize,
            clients, concurrency),
                [titlePriorityOnly, titleBalance, titlePriority], metric)


def memcachedPriorityResultsPaper():
    #plot_echoserver()
    #plot_memcached()
    #plot_memcached_test(12, 32, "Intel")
    #plot_memcached_test(20, 32, "Intel")
    #plot_memcached_test(20, 32, "SF")
    #plot_memcached_test(40, 32, "SF")
    #plot_memcached_test(20, 64, "SF")
    #plot_memcached_test(40, 16, "SF")
    plot_memcached_test(40, 32, "SF", "./data/output_runs_test/DUMMY/", pktsize = 1024)
    plot_memcached_test(40, 32, "Intel", "./data/output_runs_test2/priority/", pktsize = 1024)
    #plot_memcached_test(20, 32, "SF", "./data/output_runs_test/DUMMY/", pktsize = 1024, prefix='Q5')
    plot_memcached_test(20, 32, "Intel", "./data/dpdk_test_memcached_test_5Q/priority/", pktsize = 1024, prefix='Q5')

    #plot_memcached_test(20, 32, "Intel", "./data/output_runs_test2/priority/",  pktsize = 1024, prefix='TestQ5')
    plot_memcached_test(20, 32, "SF", "./data/output_runs_test2/priority/",  pktsize = 1024, prefix='Q5')

    plot_memcached_test(40, 32, "Intel", "./data/output_runs_test2/priority/", pktsize = 64)
    plot_memcached_test(40, 32, "SF", "./data/output_runs_test2/priority/", pktsize = 64)

    plot_memcached_test(20, 32, "SF", "./data/output_runs_test2/priority/", pktsize = 64, prefix='Q5')
    plot_memcached_test(20, 32, "Intel", "./data/output_runs_test2/priority/", pktsize = 64, prefix='Q5')


def plotBalanceResults():
    plot_memcached_test(20, 32, "SF", "./data/output_runs_balance/priority/", pktsize = 1024, prefix='Q5')
    plot_memcached_test(20, 32, "Intel", "./data/output_runs_balance/priority/", pktsize = 1024, prefix='Q5')

def forPoster():
    plot_memcached_priority_balance(20, 32, "SF",
            #priorityOnly = "./data/output_runs_posterV3/priBal/",
            priorityOnly = "./data/output_runs_posterV3/priSmall/",
            priorityPath = "./data/output_runs_posterV3/priority/",
            balancePath = "./data/output_runs_posterV3/balance/",
            pktsize = 1024, prefix='Q5' )
    plot_memcached_priority_balance(20, 32, "Intel",
            #priorityOnly = "./data/output_runs_posterV3/priBal/",
            priorityOnly = "./data/output_runs_posterV3/priSmall/",
            priorityPath = "./data/output_runs_posterV3/priority/",
            balancePath = "./data/output_runs_posterV3/balance/",
            pktsize = 1024, prefix='Q5' )



def plot_boxplot_gen(fids, pid, metric, config, y_log_scale = None):

    outputFileList = [
            #( plot_save_location + pid + "-%s-%s.png" % (app,metric)),
            ( plot_save_location + pid + "-%s.pdf" % (metric)),
        ]

    print "DOING: " + pid

    colors = ["r", "b", "g"]
    bwidth = .4

#    f = gzip.open(fnames[0], 'r')
#    j = json.load(f)
#    clients = filter(lambda x : x.startswith("client"), j["results"]["1"].keys())
#    nclients = len(clients)
#    index = np.arange(nclients)
#
    fig = plt.figure(figsize=(4,2))
    pidx = 0
    idx = 0
    kmetric = False
    if metric == "TPS":
        kmetric = True
    xresults = {}
    xresults_hp = {}
    xresults_lp = {}

    r = []
    xlbls = []
    for fid in fids:
        fnames = config[fid]['flist']
        print "for type %s, using following files:" % (fid)
        PP.pprint(config[fid]['flist'])
        fname = config[fid]['flist']

        app = config[fid]['app']
        hp_clients = config[fid]['highPrio']
        title = config[fid]['title']

        f = gzip.open(fname, 'r')
        j = json.load(f)
        results = j["results"]["1"]

        clients =  filter(lambda x : x.startswith("client"), results.keys())
        clients.sort(cmp=xcmp)
        #print "sorted clients are "
        #PP.pprint(clients)

        client_ids = [clientId(c) for c in clients]

        yvals = [float(get_result(results,  app, c,metric)) for c in clients ]
        if kmetric:
            yvals = [y/1000.0 for y in yvals]

        xresults[fid] = yvals

        if hp_clients is None:
            r.append(yvals)

            xlbls.append("%s" % (config[fid]['title']))

        else:
            yvals_hp = [ v for (c,v) in zip(client_ids, yvals) if c in hp_clients]
            yvals_lp = [ v for (c,v) in zip(client_ids, yvals) if c not in hp_clients]
            xresults_hp[fid] = yvals_hp
            xresults_lp[fid] = yvals_lp
            print "HP:", yvals_hp
            print "LP:", yvals_lp

            r.append(yvals_hp)
            xlbls.append("%s\n(HP)" % (config[fid]['title']))

            r.append(yvals_lp)
            xlbls.append("%s\n(BE)" % (config[fid]['title']))

        yerr  = None
        if metric in metric_error:
            error = metric_error[metric]
            errs = [float(get_result(results,  app, c,error)) for c in clients]
            yerr = [errs, errs]


    ax2 = plt.subplot()
    ax2.boxplot(r)
#    if y_log_scale is not None:
#        ax2.set_yscale('log')

    ax2.tick_params(axis='both', which='major', labelsize=8)
    ax2.set_xticklabels(xlbls)

    #ax2.set_xlabel("all flows", fontsize=3)
    if kmetric:
        metric = "k" + metric

    if metric in metric_units:
        u = "(%s)" % metric_units[metric]
    else:
        u =""
    ylabel = metric_expl.get(metric,metric) + u
    ax2.set_ylabel(ylabel, fontsize=11)

    plt.grid()

    for outputFile in outputFileList:
        # Checking if the parent directory exists or not.
        parentDir = os.path.dirname(outputFile)
        #print "the parentdir for output file [%s] is %s" % (outputFile, parentDir)
        if parentDir != '' :
            if not os.path.isdir(parentDir):
                os.makedirs(parentDir)
        print "Saving plot in a file %s" % (outputFile)
        #plt.savefig(outputFile, bbox_inches='tight')
        plt.savefig(outputFile, bbox_inches='tight', dpi=300)
        # To create cropped files
        if outputFile[-4:] == ".pdf":
            SP.check_output(["pdfcrop", outputFile])
    plt.close(fig)


def linuxNumbers():

    #nic = "Intel"
    nic = "SF"
    pktsize = 1024
    clients = 20
    concurrency = 32

    confsToPlot = {
        "linux" : {
            'title': "Linux",
            'flist': get_files(
                    resultDir = "./data/output_runs_linuxTest/balance/HWQ_5/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = clients,
                    srvtype="memcached_linux"),
            'app' : 'memaslap',
            'highPrio' : None,
           },

        "onload" : {
            'title': "UNet",
            'flist': get_files(
                    resultDir = "./data/output_runs_linuxTest/balance/HWQ_5/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = clients,
                    srvtype="memcached_onload"),
            'app' : 'memaslap',
            'highPrio' : None,
           },

        "dnet-pr" : {
            'title': "dnet-pr",
            'flist': get_files(
                    resultDir = "./data/output_runs_posterV3/priority/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = clients,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : [0, 1],
           },


        "dnet-pronly" : {
            'title': "dnet-prOnly",
            'flist': get_files(
                    resultDir = "./data/output_runs_posterV3/priSmall/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = 2,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : None,
           },

        "dnet-bal" : {
            'title': "Dragonet",
            'flist': get_files(
                    resultDir = "./data/output_runs_posterV3/balance/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = clients,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : None,
           },
    }





    def plot_memcached(pprefix, metric, config):
        #fids = sorted(config.keys())
        #fids = ["linux", "onload", "dnet-pr", "dnet-pronly", "dnet-bal" ]
        fids = ["linux", "onload", "dnet-bal"]

        plot_boxplot_gen(fids, pprefix, metric, config, y_log_scale=False)

    for metric in ("TPS", "Total_Avg"):
        plot_memcached(
            ("memcached-%s-pkt-%d-cl-%d-load-%d" % (nic, pktsize, clients, concurrency)),
                metric, confsToPlot)

    print "done with everything"


def mixedWorkload():

    nic = "Intel"
    pktsize = 1024
    clients = 20
    concurrency = 16

    confsToPlot = {
        "stable" : {
            'title': "stable",
            'flist': get_files(
                    resultDir =  "./data/output_debugging_online_automated_v2/MixedRun/LONG/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = clients,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : [0, 1],
           },

        "dynamicHP" : {
            'title': "DynamicHP",
            'flist': get_files(
                    resultDir =  "./data/output_debugging_online_automated_v2/MixedRun/SHORTHP/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = 1,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : None,
           },

        "dynamicLP" : {
            'title': "DynamicLP",
            'flist': get_files(
                    resultDir =  "./data/output_debugging_online_automated_v2/MixedRun/SHORTLP/",
                    workload = "",
                    testType = "memcached_rr",
                    concurrency = concurrency,
                    pktsize = pktsize,
                    nic = nic,
                    clients = 1,
                    srvtype="*"),
            'app' : 'memaslap',
            'highPrio' : None,
           },

    }





    def plot_memcached(pprefix, metric, config):
        #fids = sorted(config.keys())
        #fids = ["linux", "onload", "dnet-pr", "dnet-pronly", "dnet-bal" ]
        fids = ["stable", "dynamicHP", "dynamicLP"]

        plot_boxplot_gen(fids, pprefix, metric, config, y_log_scale=False)

    for metric in ("TPS", "Total_Avg"):
        plot_memcached(
            ("memcached-%s-pkt-%d-cl-%d-load-%d" % (nic, pktsize, clients, concurrency)),
                metric, confsToPlot)

    print "done with everything"


if __name__ == "__main__":
    global plot_save_location
    #plot_save_location = "./memcachedPlots/"
    #memcachedPriorityResultsPaper()
    #plot_save_location = "./memcachedLoadPlotsTest/"
    #forPoster()
    #plot_save_location = "./memcachedLoadPlotsLinux/"
    #linuxNumbers()
    #memcachedPriorityResultsPaper()
    plot_save_location = "./memcachedLoadPlotsOnline/"
    mixedWorkload()

