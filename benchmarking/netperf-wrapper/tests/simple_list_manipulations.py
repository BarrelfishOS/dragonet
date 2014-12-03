## -*- mode: python; coding: utf-8 -*-

# Assumed access to global access to


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

def repeat_list_elem_n_times(elemList, n):
    nlist = []
    for e in elemList:
        for i in range(0, n):
            nlist.append(e)
    return nlist


def expand_client_list(sn, cn):
    cllist = []
    for s in sn:
        for c in cn:
            cllist.append(c)
    return cllist


def chunks(l, n):
    """ Yield successive n-sized chunks from l.  """
    for i in xrange(0, len(l), n):
        yield l[i:i+n]

# generates cores to use list while avoiding assigning same core to two
# instances on same machine.
# Essentially it allows having same client multiple times in list of clients
def gen_core_list(mlist, cores_to_alloc, cores_per_machine, starting_core=2):
    cores_to_use = []
    for i in range(0,len(mlist)):
        m = mlist[i]
        existing_duplicates = [x for x in mlist[0:i] if x == m]
        init_core = starting_core + (len(existing_duplicates) * cores_to_alloc)
        # rounding the core id's with number of cores available
        core_use_order = map(
                                (lambda x: x % int(cores_per_machine[m])),
                                range(init_core, (init_core + cores_to_alloc))
                            )
        cores_to_use.append(core_use_order)
#        print "Cores used by machine %s: %s" % (m, core_use_order)
    return cores_to_use


