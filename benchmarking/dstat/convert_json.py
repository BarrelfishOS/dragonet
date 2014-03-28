#!/usr/bin/env python2

try:
    import sys, os
    import types as tp
    import json as js
except KeyboardInterrupt:
    pass

JSON_INDENT = 4

# map ( ( lambda x: x['total cpu usage']['idl'] ), myd[PARENT_DATATYPE])
if __name__ == '__main__':
    if (len(sys.argv) == 1) :
        print """
ERROR: Missing input file.
        Please provide input file, and optionally output files,
        and permission to update output file"
USAGE: %s <INPUTFILE> [<OUTPUTFILE> <u>] %
""" % (inputfile)
        sys.exit(1)


    inputfile = sys.argv[1]
    ff = open(inputfile, 'r')
    data = ff.read()
    ff.close()

    if ( data[-2] == ',' and data[-1] == '\n' ) :
        ndata = data[0:-2] + "]}"
    else:
        ndata = data

    myd = js.loads(ndata)

    PARENT_DATATYPE = 'dstat_data'
    PARENT_DATATYPE_OUT = 'dstat_outlist'
    newd = {}
    newd[PARENT_DATATYPE_OUT] = {}
    for pkey in myd[PARENT_DATATYPE][0].keys() :
        if  isinstance(myd[PARENT_DATATYPE][0][pkey], tp.DictType) :
            newd[PARENT_DATATYPE_OUT][pkey] =  map ((lambda x: x[pkey]),
                    myd[PARENT_DATATYPE])

        newd[PARENT_DATATYPE_OUT][pkey] = {}
        for ckey in myd[PARENT_DATATYPE][0][pkey].keys() :
            newd[PARENT_DATATYPE_OUT][pkey][ckey] = map ((lambda x: x[pkey][ckey] ),
                            myd[PARENT_DATATYPE])


    # TODO: let user decide if he wants to fix his file
    if (len(sys.argv) > 3) :
        if ( data[-2] == ',' and data[-1] == '\n' ) :
            print "Updating %s with fixed ending for valid JSON" % (inputfile)
        ff = open(inputfile, 'w')
        ff.write(ndata)
        ff.close()

    if (len(sys.argv) > 2) :
        print "sys.argv is %d, --> %s" % ( len(sys.argv), str(sys.argv))
        outputfile = sys.argv[2]
        print "Writing transformed result into %s" % (outputfile)
        ff = open(outputfile, 'w')
        ff.write(js.dumps(newd,indent=JSON_INDENT))
        ff.close()
    else :
        print js.dumps(newd,indent=JSON_INDENT)

