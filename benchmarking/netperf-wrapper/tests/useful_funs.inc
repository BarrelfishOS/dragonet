## -*- mode: python; coding: utf-8 -*-



def get_server_bw(allResults, meta, ifname1=None, takeAvg=False, rx_tx='recv'):
    """should return value series to plot from whole result"""
    res = []
    tmp = allResults[allResults.keys()[0]]['server-0']['dstat']['dstat_outlist']
    for k in tmp.keys():
        if "net/" in k:
            res = tmp[k][rx_tx][dstat_ignore_initial_x_vals:-1]
            break
    ans = map ((lambda x: (float)(int(x) * 8)/ (10 ** 9) ), res)
    if (not takeAvg) :
        return ans
    else :
        ll = myavg(ans)
        return [ll]


def get_latency_nums(allResults, meta, mname, func=None, takeAvg=False):
    """get all the data points related to latency"""
    res = allResults[allResults.keys()[0]][mname]['latencybm']['RESULT']
    res = res[dstat_ignore_initial_x_vals:-1]
    if func:
        return func(res)
    return res

def get_interrupts(allResults, meta, mname, takeAvg=False):
    """should total interrupts core"""
    res = allResults[allResults.keys()[0]][mname]['dstat']['dstat_outlist']['interrupts']
    allSum = []
    for k in res.keys():
        curr = res[k][dstat_ignore_initial_x_vals:-1]
        if allSum == [] :
            allSum = curr
        else:
            allSum =[x+y for x,y in zip(allSum,curr)]
    if (not takeAvg) :
        return allSum
    else :
        ll = int(myavg(allSum))
        return [ll]


def get_cpu2(allResults, meta, mname, coreidlist, takeAvg=False):
    """should return idle CPU time for given machine on given core"""
    ans = []
    if coreidlist == [] :
        return []
    for rn, r in allResults.items():
        per_run = {}
        per_run['total'] = []
        per_core_avg = []
        data = r[mname]['dstat']['dstat_outlist']
        for c in coreidlist:
            res = data['cpu%d usage' % c]['idl'][dstat_ignore_initial_x_vals:-1]
            values = map ((lambda x: 100 - float(x)), res)
            per_run[c] = values
            per_core_avg.append(myavg(per_run[c]))
            if per_run['total'] == [] :
               per_run['total'] = values
            else:
               per_run['total'] = [ x + y for x, y in zip(per_run['total'], per_run[c])]
        ll = int(myavg(per_core_avg))
        ans.append(ll)
    return ans

def get_cpu(allResults, meta, mname, coreid, takeAvg=False):
    """should return idle CPU time for given machine on given core"""
    res = allResults[allResults.keys()[0]][mname]['dstat']['dstat_outlist']['cpu%d usage' % coreid]['idl'][dstat_ignore_initial_x_vals:-1]
    ans = map ((lambda x: 100 - float(x)), res)
    if (not takeAvg) :
        return ans
    else :
        ll = int(myavg(ans))
        return [ll]



def get_server_cpu(allResults, meta, takeAvg=False):
    return get_cpu(allResults, meta, 'server-0', int(server_core), takeAvg)

def get_client_cpu(allResults, meta, takeAvg=False):
    return get_cpu(allResults, meta, 'client-0', int(client_core), takeAvg)

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

        if not (len(per_run) == 1):
            print "There are more than expected (1) answers fo for attribute %s in run %s" % (
                attribute, rn )

        assert(len(per_run) == 1)
        #ans.append(sum(per_run))
        #ans.append(myavg(per_run))
        ans.append((per_run[0]))
    return (ans)

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


def get_metadata_attr(allResults, meta, attribute, agg=None):
    """should return the metadata attribute"""
    if attribute in meta.keys() :
        if agg != None and agg != False:
            return agg (meta[attribute])
        else :
            return meta[attribute]
    else:
        return ""


def meta_attr_wrapper(p, l=None, func=get_metadata_attr, agg=None):
    if l == None or l == "":
       l = p
    res =  {
        'data': func,
        'args' : {'attribute': p,
                   'agg': agg
                 },
        'label': l
       }
    return res

################################################

def get_dstat_attr_cpu(allResults, meta, client, attribute, aggregator):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            if (mn.startswith(client)):
                for tn, tt in m.items():
                    if (tn.startswith("dstat")):
                        for k in tt['dstat_outlist'].keys():
                            if k.startswith(attribute):
                                stats = tt['dstat_outlist'][k]['idl'][dstat_ignore_initial_x_vals:-1]
                                values = map ((lambda x: 100 - float(x)), stats)
                                per_run = per_run + values
        ans.append(aggregator(per_run))
    return ans

def get_dstat_attr_cpu_detailed(allResults, meta, client, attribute, attr2, aggregator):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            if (mn.startswith(client)):
                for tn, tt in m.items():
                    if (tn.startswith("dstat")):
                        for k in tt['dstat_outlist'].keys():
                            if k.startswith(attribute):
                                stats = tt['dstat_outlist'][k][attr2][dstat_ignore_initial_x_vals:-1]
                                values = map ((lambda x: float(x)), stats)
                                per_run = per_run + values
        ans.append(aggregator(per_run))
    return ans




def get_result_attr(allResults, meta, client, attribute, aggregator):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            for tn, t in m.items():
                if (tn.startswith(client)):
                    out = t["CMD_OUTPUT"]
                    if attribute in out.keys():
                        per_run.append(float(out[attribute]))

        ans.append(aggregator(per_run))
        #ans.append(myavg(per_run))

        #if not (len(per_run) == 1):
        #   print "There are more than expected (1) answers fo for attribute %s in run %s" % (
        #       attribute, rn )
        #assert(len(per_run) == 1)
        #ans.append((per_run[0]))
    return (ans)

def find_uniq(vals):
    if vals == []:
        return ""
    return vals[0]

def get_result_str(allResults, meta, client, attribute, aggregator=find_uniq):
    """should return value series to plot from whole result"""
    ans = []
    for rn, r in allResults.items():
        per_run = []
        for mn, m in r.items():
            for tn, t in m.items():
                if (tn.startswith(client)):
                    out = t["CMD_OUTPUT"]
                    per_run.append((out[attribute]))
        ans.append(aggregator(per_run))
    return (ans)


def result_attr_wrapper(p, l=None, c="memaslap0", func=get_result_attr, agg=sum):
    if l == None or l == "":
       l = p
    if func == None or func == "":
        func = get_result_attr

    res =  {
        'data': func,
        'args' : {
            'client': c,
            'attribute': p,
            'aggregator': agg,
                },
        'label': l
       }
    return res

