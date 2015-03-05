#!/usr/bin/env python2

import sys
import re

def diffFiles(f1, f2, cutoff=0):

    f1_lines = open(f1).readlines()
    f2_lines = open(f2).readlines()

    assert(len(f1_lines) == len(f2_lines))

    for i in range(0, len(f1_lines)):
        l1 = f1_lines[i].strip().split(':')
        l2 = f2_lines[i].strip().split(':')

        h1 = l1[0].strip()
        h2 = l2[0].strip()

        if (len(l1) != len(l2)) or ( h1 != h2) :
            print "Entry mismatch found: %s != %s" % (l1, l2)
            continue

        if len(l1) <= 1 :
            print "Ignoring small line: %s" % (l1)
            continue

        vs1 = l1[1].strip()
        vs2 = l2[1].strip()

        if ( vs1 == vs2) :
            #print "Ignoring same values: %s: %s == %s" % (h1, vs1, vs2)
            continue

        v1 = int(vs1)
        v2 = int(vs2)

        diff = v2 - v1
        if diff > cutoff or -diff > cutoff :
            print "%s: %d" % (h1, diff)

if __name__ == "__main__":
    cutoff = 0
    if len(sys.argv) < 3 :
        print "ERROR: Invalid missing cmdline parameters"
        print "USAGE: %s <fileBefore> <fileAfter> [cutoff]" % (sys.argv[0])
        sys.exit(1)
    f1 = sys.argv[1]
    f2 = sys.argv[2]
    if len(sys.argv) > 3 :
        cutoff = int(sys.argv[3])

    diffFiles(f1, f2, cutoff)


