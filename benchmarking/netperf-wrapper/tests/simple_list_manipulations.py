## -*- mode: python; coding: utf-8 -*-

# Assumed access to global access to
#   server_names,
#    PIN_THREADS, SERVER_CORESHIFT, SERVERS_INSTANCES, SERVER_CORES


def uniqify(seq):
   """ order preserving method to create list of unique elements"""
   checked = []
   for e in seq:
       if e not in checked:
           checked.append(e)
   return checked




def chunks(l, n):
    """ Yield successive n-sized chunks from l.  """
    for i in xrange(0, len(l), n):
        yield l[i:i+n]

def balanced_chunks(l, n):
    """ break given arrary into almost equisized n arrarys """
    bsize = len(l) / n
    remainder = len(l) % n
    ans = []

    startingPoint = 0
    for i in xrange(0, n):
        chunk = bsize
        if i < remainder :
            chunk = chunk + 1
        narray = l[startingPoint: startingPoint + chunk]
        ans.append(narray)
        startingPoint = startingPoint + chunk
    return ans


def create_flows(srv_ip_list, srv_start_port,
        client_ip_list, client_start_port,
        flows_per_client):
    """ Given src ip, src port, client ip, client src port, create x flows """

    allFlows = []

    for si in range(0, len(srv_ip_list)):
        # find if same server name has arrived before
        sid = len(filter((lambda x: x == srv_ip_list[si]), srv_ip_list[:si]))
        srv_p = srv_start_port + sid
        for ci in range(0, len(client_ip_list)):
            # find if same client name has arrived before
            cid = len(filter((lambda x: x == client_ip_list[ci]), client_ip_list[:ci]))
            cli_p_s = client_start_port + (cid * flows_per_client)

            for cl_p in range(cli_p_s, cli_p_s + flows_per_client):
                allFlows.append((srv_ip_list[si], srv_p, client_ip_list[ci], cl_p, sid, cid))

    return allFlows


def flows_group_by(flist, groupbyKeyFn):
    """group the flows together based on the given function to convert
        elem into the key for group-by """
    allKeys = map (groupbyKeyFn, flist)
    keys = uniqify(allKeys)
    group = {}
    for k in keys:
        group[k] = filter((lambda x: groupbyKeyFn(x) == k), flist)
    return group



#############################################################
# old functions
#############################################################
def get_n_elems_from_list(n, inlist):
    """get first n elements of the list.
        If list does not have enough elements, then repeat the list. """
    outlist = []
    listlen = len(inlist)
    if listlen >= n:
        outlist = inlist[:n]
        return outlist
    for i in range(0, n) :
        outlist.append(inlist[i % listlen])
    return outlist

def repeat_list_elem_n_times(inlist, n):
    """ Repeat each element in the list n times """
    nlist = []
    for e in inlist:
        for i in range(0, n):
            nlist.append(e)
    return nlist

def expand_client_list(sn, cn):
    cllist = []
    for s in sn:
        for c in cn:
            cllist.append(c)
    return cllist


def toCoreList2(clist, separator=",", prefix=""):
    ret = ""
    for c in clist:
        if ret == "" :
            ret = "%s%d" % (prefix, c)
        else :
            ret = "%s%s%s%d" % (ret, separator, prefix, c)
    #print "toCoreList2 %s" % (ret)
    return ret


def toCoreList(clist):
    ret = ""
    for c in clist:
        if ret == "" :
            ret = "%d" % (c)
        else :
            ret = "%s,%d" % (ret, c)
    return ret

def toCoreListDstat(clist):
    ret = "%d-%d" % (min(clist), max(clist))
    return ret

    port_list_for_clients = expand_client_list(server_names, port_list)

def get_client_real_name(m):
    isMultiNICMachine =  m.find("--")
    if isMultiNICMachine  == -1 :
        return m
    else :
        return m[:isMultiNICMachine]

def is_same_machine(m1, m2):
    m1_real = get_client_real_name(m1)
    m2_real = get_client_real_name(m2)
    return (m1_real == m2_real)

# generates cores to use list while avoiding assigning same core to two
# instances on same machine.
# Essentially it allows having same client multiple times in list of clients
def gen_core_list(mlist, cores_to_alloc, cores_per_machine, starting_core=2):
    cores_to_use = []
    for i in range(0,len(mlist)):
        m = mlist[i]
        existing_duplicates = [x for x in mlist[0:i] if is_same_machine(x, m)]
        init_core = starting_core + (len(existing_duplicates) * cores_to_alloc)
        # rounding the core id's with number of cores available
        core_use_order = map(
                                (lambda x: x % int(cores_per_machine[m])),
                                range(init_core, (init_core + cores_to_alloc))
                            )
        cores_to_use.append(core_use_order)
#        print "Cores used by machine %s: %s" % (m, core_use_order)
    return cores_to_use



def get_isolation_container(is_server):
    if not PIN_THREADS:
        return ""

    if is_server == True:
        return " taskset -c %s " % (
             toCoreList2(range(
                 (0                 ),
                 ((SERVERS_INSTANCES*SERVER_CORES))
               )))
    else :
        return " taskset -c %s " % (
             toCoreList2(range(
                 MAX_CORES - (HWQUEUES),
                 MAX_CORES
               )))



