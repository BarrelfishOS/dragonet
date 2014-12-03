## -*- mode: python; coding: utf-8 -*-

# Assumed access to global access to
#  - RESULT_LOCATION_BASE
#  - TOTAL_LENGTH

def getInterruptLines(minfo):
    netdev = minfo["EGRESS_INFO"]["iface"]
    if netdev == None or netdev == 'lo' or netdev == "" :
        return ""
    if not 'INTERRUPTS' in  minfo["EGRESS_INFO"].keys():
        return ""
    ints = minfo["EGRESS_INFO"]['INTERRUPTS']
    if ints == None or ints == 'lo' or ints == "" :
        return ""
    return "-I %s" % (ints)


def dstatCmd(mname, outBase=RESULT_LOCATION_BASE, outfname='dstat_out.dstat',
            outjson='dstat_out.json',
            netdev='lo', cpus='3,5',
            runtime=TOTAL_LENGTH,
            interrupts=""
            ):

    if outfname == None or outfname == "" or outfname == False :
        outfname = '/dev/null'
    else :
        outfilename = ('%s_%s/%s' % (RESULT_LOCATION_BASE, mname, outfname))
    outfname = '/dev/null'

    outjsonname = ('%s_%s/%s' % (RESULT_LOCATION_BASE, mname, outjson))
    cmd = '../dstat/dstat --nocolor -J %s -ciny -C%s -N %s %s 1 %d > %s ; cat %s' % (
                   outjsonname, cpus, netdev, interrupts, runtime, outfilename, outjsonname)
    return cmd



#def ethtoolCmd(mname, outBase=RESULT_LOCATION_BASE, netdev, isEnd=False):
#
#    extension="start"
#    if isEnd:
#        extension="end"
#    outfilename = ('%s_%s/ethtoolout.%s' % (RESULT_LOCATION_BASE, mname, extension))
#    cmd="ethtool -S %s > %s " % (netdev, outfilename)
#    return cmd


