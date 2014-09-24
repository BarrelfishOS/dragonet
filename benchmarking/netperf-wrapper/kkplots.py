import gzip
import json
from pprint import pprint

import pylab
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec

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
    "MEAN_LATENCY" : "STDDEV_LATENCY",
}

def clientId(c):
    return int(c[6:])

def plot_per_client(fnames, fids, pid, app, metric, client_class=None):
    def get_result(client, res):
        cid = clientId(c)
        return results[client][app + str(cid)]["CMD_OUTPUT"][res]

    def xcmp(x,y):
        xi = int(x[6:])
        yi = int(y[6:])
        return cmp(xi,yi)

    colors = ["r", "b", "g"]
    bwidth = .4
    nclients = 40
    index = np.arange(40)

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
        clients.sort(cmp=xcmp)
        client_ids = [clientId(c) for c in clients]

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
    plt.savefig("nsdiplots/" + pid + "-%s-%s.pdf" % (app,metric), bbox_inches='tight')

if __name__ == "__main__":
    fresults_echo = {
        "dnet-64b-40"             : "nsdi_data/Test_udp_rr/CONCUR_1/PKT_64/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T031116.210807.llvmE10k_CLC_40_PKT_64.json.gz",
        "linux-64b-40"            : "nsdi_data/Test_udp_rr/CONCUR_1/PKT_64/NIC_Intel/SRV_fancyEchoLinuxPoll/udp_rr-2014-09-24T054326.587080.Test_udp_rr_CONCUR_1_PKT_64_NIC_Intel_SRV_fancyEchoLinuxPoll_CLC_40.json.gz",
        "dnet-1k-40"              : "nsdi_data/Test_udp_rr/CONCUR_1/PKT_1024/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T031514.597472.llvmE10k_CLC_40_PKT_1024.json.gz",
        "linux-1k-40"             : "nsdi_data/Test_udp_rr/CONCUR_1/PKT_1024/NIC_Intel/SRV_fancyEchoLinuxPoll/udp_rr-2014-09-24T054831.626261.Test_udp_rr_CONCUR_1_PKT_1024_NIC_Intel_SRV_fancyEchoLinuxPoll_CLC_40.json.gz",
        "dnet-64b-40-conc32"      : "nsdi_data/Test_udp_rr/CONCUR_32/PKT_64/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T011154.065430.llvmE10k_CLC_40_PKT_64.json.gz",
        "dnet-64b-40-conc32-prio" : "nsdi_data/priority/Test_udp_rr/CONCUR_32/PKT_64/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T212422.113155.Priority_llvmE10k_CLC_40_PKT_64.json.gz",
        "dnet-1k-40-conc32"       : "nsdi_data/Test_udp_rr/CONCUR_32/PKT_1024/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T011553.951088.llvmE10k_CLC_40_PKT_1024.json.gz",
        "dnet-1k-40-conc32-prio"  : "nsdi_data/priority/Test_udp_rr/CONCUR_32/PKT_1024/NIC_Intel/SRV_llvmE10k/udp_rr-2014-09-24T212818.324440.Priority_llvmE10k_CLC_40_PKT_1024.json.gz",
    }

    def plot_echo(pprefix, fids, metric):
        fnames = [fresults_echo[fid] for fid in fids]
        plot_per_client(fnames, fids, pprefix, "netperf", metric)

    fresults_memcached = {
        "linux-64b-40"     : "./nsdi_data/Test_memcached_rr/CONCUR_1/PKT_64/NIC_Intel/SRV_memcached_poll/memcached_rr-2014-09-24T081503.361100.Test_memcached_rr_CONCUR_1_PKT_64_NIC_Intel_SRV_memcached_poll_CLC_40.json.gz",
        "linux-1k-40"      : "./nsdi_data/Test_memcached_rr/CONCUR_1/PKT_1024/NIC_Intel/SRV_memcached_poll/memcached_rr-2014-09-24T082012.779390.Test_memcached_rr_CONCUR_1_PKT_1024_NIC_Intel_SRV_memcached_poll_CLC_40.json.gz",
        "dnet-64b-40"      : "./nsdi_data/Test_memcached_rr/CONCUR_1/PKT_64/NIC_Intel/SRV_llvmE10k/memcached_rr-2014-09-23T222302.120085.llvmE10k_CLC_40_PKT_64.json.gz",
        "dnet-1k-40"       : "./nsdi_data/Test_memcached_rr/CONCUR_1/PKT_1024/NIC_Intel/SRV_llvmE10k/memcached_rr-2014-09-23T214501.564801.llvmE10k_CLC_40_PKT_1024.json.gz",
        "dnet-64b-40-prio" : "./nsdi_data/priority/Test_memcached_rr/PKT_64/NIC_Intel/SRV_llvmE10k/memcached_rr-2014-09-24T153858.799258.Priority_llvmE10k_CLC_40_PKT_64.json.gz",
        "dnet-1k-40-prio"  : "./nsdi_data/priority/Test_memcached_rr/PKT_1024/NIC_Intel/SRV_llvmE10k/memcached_rr-2014-09-24T154256.146873.Priority_llvmE10k_CLC_40_PKT_1024.json.gz"
    }

    def plot_memcached(pprefix, fids, metric):
        fnames = [fresults_memcached[fid] for fid in fids]
        plot_per_client(fnames, fids, pprefix, "memaslap", metric)

    for metric in ("MEAN_LATENCY", "THROUGHPUT"):
        plot_echo("echo-64b-40", ["dnet-64b-40", "linux-64b-40"], metric)
        plot_echo("echo-1k-40", ["dnet-1k-40", "linux-1k-40"], metric)
        plot_echo("echo-64b-40-conc32-prio", ["dnet-64b-40-conc32", "dnet-64b-40-conc32-prio"], metric)
        plot_echo("echo-1k-40-conc32-prio", ["dnet-1k-40-conc32", "dnet-1k-40-conc32-prio"], metric)

    for metric in ("TPS", "Total_Avg"):
        plot_memcached("memcached-64b-40", ["dnet-64b-40", "linux-64b-40"], metric)
        plot_memcached("memcached-1k-40", ["dnet-1k-40", "linux-1k-40"], metric)
        plot_memcached("prio-memcached-64b-40", ["dnet-64b-40-prio","dnet-64b-40",], metric)
        plot_memcached("prio-memcached-1k-40", ["dnet-1k-40-prio","dnet-1k-40"], metric)
