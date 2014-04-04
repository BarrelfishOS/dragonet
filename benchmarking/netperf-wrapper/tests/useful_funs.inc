## -*- mode: python; coding: utf-8 -*-


def get_server_bw(allResults, meta, ifname1=None, takeAvg=False):
    """should return value series to plot from whole result"""
#        "metadata"
#         "MINFO_SERVER": {
#            "asiago": {
#                "LOCAL_HOST": "asiago",
#                "KERNEL_RELEASE": "3.13.0+",
#                "EGRESS_INFO": {
#                    "src": "10.22.4.11",
#                    "iface": "eth6",

    res = []
    tmp = allResults['1']['server-1']['dstat']['dstat_outlist']
    for k in tmp.keys():
        if "net/" in k:
            res = tmp[k]['recv'][2:-1]
            break
#    res = allResults['1']['server-1']['dstat']['dstat_outlist']['net/%s' % SERVERS_IF[SERVERS[0]]]['recv'][2:-1]
    ans = map ((lambda x: (float)(int(x) * 8)/ (10 ** 9) ), res)
    if (not takeAvg) :
        return ans
    else :
        ll = myavg(ans)
        return [ll]

def get_cpu(allResults, meta, mname, coreid, takeAvg=False):
    """should return idle CPU time for given machine on given core"""
    res = allResults['1'][mname]['dstat']['dstat_outlist']['cpu%1d usage' % coreid]['idl'][2:-1]
    ans = map ((lambda x: 100 - float(x)), res)
    if (not takeAvg) :
        return ans
    else :
        ll = myavg(ans)
        return [ll]

def get_server_cpu(allResults, meta, takeAvg=False):
    return get_cpu(allResults, meta, 'server-1', int(server_core), takeAvg)

def get_client_cpu(allResults, meta, takeAvg=False):
    return get_cpu(allResults, meta, 'client-1', int(client_core), takeAvg)

def get_netperf_attr(allResults, meta, attribute):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            for tn, t in m.items():
                if (tn == 'netperf'):
                    out = t["CMD_OUTPUT"]
                    per_run.append(float(out[attribute]))

        assert(len(per_run) > 0)
        ans.append(sum(per_run))
    return ans

def get_netperf_str(allResults, meta, attribute):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            for tn, t in m.items():
                if (tn == 'netperf'):
                    out = t["CMD_OUTPUT"]
                    per_run.append((out[attribute]))

        assert(len(per_run) > 0)
        return per_run
        #ans.append(per_run)
        #return ans


def myavg(l):
    if (l == None or l == [])  :
        return 0
    return (sum(l)/len(l))


def netperf_attr_wrapper(p, l=None, func=get_netperf_attr):
    if l == None or l == "":
       l = p
    res =  {
        'data': func,
        'args' : {'attribute': p},
        'label': l
       }
    return res



