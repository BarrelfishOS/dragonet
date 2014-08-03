#!/usr/bin/env python

import sys

#sys.path.append('/home/skaestle/bin/')
#from skstat import do_stat

import matplotlib.pyplot as plt
from matplotlib import rcParams
import fileinput
import re
import os, sys
import argparse
from matplotlib.lines import Line2D

ax2 = None


linestyles = ['-', '--', ':', '-.' ]
markers = []
#for m in Line2D.markers:
#    try:
#        if len(m) == 1 and m != ' ':
#            markers.append(m)
#    except TypeError:
#        pass
#
styles = markers + [
    r'$\lambda$',
    r'$\bowtie$',
    r'$\circlearrowleft$',
    r'$\clubsuit$',
    r'$\checkmark$']


class SkData:

    def __init__(self, args=None):

        self.x = []
        self.v = []
        self.v2 = []
        self.err = []
        self.err2 = []
        self.err_min_1 = []
        self.err_max_1 = []
        self.err_min_2 = []
        self.err_max_2 = []
        self.sk_m_title = ''
        self.data_max = None
        self.args = args
        self.plot_label = None
        self.data_max_val = []

        self.d = dict()


    def set_plot_label(self, n):
        self.plot_label = n

    def add_v(self, value):
        """
        Add given value to the the data elements.
        Value will be type-casted to float.

        """
        if not self.args or \
                not self.args.ymax or \
                float(value)<float(self.args.ymax):

            self.v.append(float(value))


    def set_dict(self, d):
        """
        Read plot data from given dict

        Dict is x-value -> skstat.py

        """
        self.d = d

    def read_stdin(self):
        """
        Read input data from stdin

        """
        self.read_input(sys.stdin.readlines())

    def read_file(self, fname):
        """
        Read input from file

        """
        self.plot_label = os.path.basename(fname).split('.')[0].replace('_', ' ')
        with open(fname) as f:
            content = f.readlines()
            self.read_input(content)

    def read_input(self, lines):
        """
        Parse input

        """
        trailing_marker = ' #####'
        title_marker = 'title:'
        max_val_marker = 'max:'
        count = 0
        for l in lines:

            l = l.strip()
            if not l.endswith(trailing_marker) :
                print "skipping line %s as it does not have proper ending" % (l)
                continue
            l = l[:-len(trailing_marker)]

            if l.startswith(title_marker) :
                #self.plot_label = l[len(title_marker):]
                continue

            if l.startswith(max_val_marker) :
                self.data_max_val.append(float(l[len(max_val_marker):]))
                continue


            # Remove actual data part, which is enclosed in [].
            if l.find('[') > 0:
                l = l[:l.find('[')]

            # Only numbers and spaces on line
            m = re.match('[0-9\.\s ]+.?$', l)
            if m:
                if self.args.format == 'x_y_stderr':
                    e = l.split()
                    assert len(e)>=3
                    self.x.append(int(e[0]))
                    self.v.append(float(e[1]))
                    self.err.append(float(e[2]))

                elif self.args.format == 'x_y':
                    e = l.split()
                    assert len(e)>=2
                    self.x.append(int(e[0]))
                    self.add_v(e[1])

                elif self.args.format == 'y':
                    e = l.split()
                    assert len(e)>=1
                    self.add_v(e[0])

                else:
                    try:
                        e = l.split()
                        if len(e)==1:
                            self.add_v(e[0])
                        elif len(e)==2:
                            self.v.append(float(e[0]))
                            self.err.append(float(e[1]))
                        elif len(e)==3:
                            self.x.append(int(e[0]))
                            self.v.append(float(e[1]))
                            self.err.append(float(e[2]))
                        elif len(e)==5:
                            self.x.append(int(e[0]))
                            self.v.append(float(e[1]))
                            self.err.append(float(e[2]))
                            self.v2.append(float(e[3]))
                            self.err2.append(float(e[4]))
                    except:
                        pass
            else:
                print "Nothing matched for line %s" % (l)


    def output(self, idx):
        """
        Output plot

        """
        if self.d:
            print 'Not supported'
            sys.exit(1)
#            print 'Parsing Dictionary data'
#            for key, data in self.d.iteritems():
#                print key
#                print data
#                self.x.append(key)
#
#                if isinstance(data, list):
#                    (mean, std, _, _, _) = do_stat(data)
#                else:
#                    (mean, std, _, _, _) = data
#
#                self.v.append(mean)
#                self.err.append(std)
#
#                print "Adding %d with value %f" % (key, mean)

        if not self.v:
            print 'No data to plot, exiting'
            sys.exit(1)

        if self.x:
            if self.err:
                print 'Plotting with x-coordinates given (%s)' % self.plot_label
                print self.x
                print self.v
                print self.err
                if idx < len(linestyles):
                    plt.errorbar(self.x, self.v, linestyle=linestyles[idx], yerr=self.err, label=self.plot_label)
                else:
                    style = styles[(idx - len(linestyles)) % len(styles)]
                    plt.errorbar(self.x, self.v, linestyle='--', marker=style, yerr=self.err, label=self.plot_label)

            else:
                plt.plot(self.x, self.v, 'yo-')
                # plt.bar(self.x, self.v)
                # val = [
                #     "NORMAL",
                #     "RANDOM",
                #     "SEQUENTIAL",
                #     "WILLNEED",
                #     "DONTNEED",
                #     "FREE",
                #     "ACCESS_DEFAULT",
                #     "ACCESS_LWP",
                #     "ACCESS_MANY",
                #     "ACCESS_MANY_PSET",
                #     ]
                # plt.xticks(range(len(val)), val)
                # locs, labels = plt.xticks()
                # plt.setp(labels, rotation=45)
        else:
            if self.err:
                print 'Plotting with implicit x-coordinates and error bars'
                plt.errorbar([x for x in range(len(self.v)) ],
                             self.v, yerr=self.err)
            else:
                print 'Plotting with implicit x-coordinates'
                plt.plot([x for x in range(len(self.v)) ], self.v, 'yo-')

        if self.v2 and self.x:
            print 'Plotting second plot'
            plt.errorbar(self.x, self.v2, yerr=self.err2)


class SkPlot:

    def __init__(self):

        parser = argparse.ArgumentParser(description='Hepler for plots')
        parser.add_argument('--title', default="fixme",
                            help="Title of the plot")
        parser.add_argument('--measurement', default=None,
                            help="Name of measurement to extract from input")
        parser.add_argument('--x', default="fixme",
                            help="Label for x-axis")
        parser.add_argument('--y', default="fixme",
                            help="Label for y-axis")
        parser.add_argument('--ymax', default=None,
                            help="Drop values above indicated max")
        parser.add_argument('--format',
                            help="Either one of x_y_stderr, x_y, y. sk_m should be detected automatically.")
        parser.add_argument('files', nargs='*',
                            help="Files to print plots from rather than stdin")
        parser.add_argument('--save',
                            help="Save image as JPG with given name")
        parser.add_argument('--psize', default=1024,
                            help="Packet size of each echo request")
        self.args = parser.parse_args()

        self.do_plot(self.args.files)


    @staticmethod
    def plot_header(title='', x='', y='', y2='', y1_max=None, y2_max=None):
        plt.title(title)
        if y1_max == None:
            plt.ylim(ymin=0)
        else :
            plt.ylim(ymin=0, ymax=y1_max)
        plt.xlabel(x)
        plt.ylabel(y)
        plt.legend(loc=1,prop={'size':10})
        #plt.legend()

        if y2_max != None:
            ax2 = plt.twinx()
            plt.ylim(ymin=0, ymax=y2_max)
            plt.ylabel(y2)
            #ax2.ylim(0, ymax=y2_max)
            #ax2.ylabel(y2)
#        print "the labels are %s" % (plt.axis().get_xticklabels())


    def do_plot(self, files):
        """
        Do plots

        """
        max_y_axis_val = []
        idx = 0
        if files:
            for f in files:
                i = SkData(self.args)
                i.read_file(f)
                i.output(idx)
                max_y_axis_val.append(max(i.data_max_val))
                idx = idx + 1
        else:
            i = SkData(self.args)
            i.read_stdin()
            i.output(idx)

        pkt_size = int(self.args.psize)
        max_val =  max(max_y_axis_val) * 1.1
        max_val_y2 = max_val * (pkt_size * 8)
        print "max value %f: 110 percent  =  %f, BW = %f, %f" % (max(max_y_axis_val),
                max_val, max_val_y2, max_val_y2/ (10 ** 9))
        self.args.title = self.args.title + ", Request size: %dB" % (pkt_size)
        SkPlot.plot_header(self.args.title, self.args.x, self.args.y, "Gbps",
                max_val, (max_val_y2/ (10 ** 9))
                )

        if self.args.save:
            rcParams.update({'figure.autolayout': True})
            plt.gcf().subplots_adjust(bottom=0.22)
            plt.savefig(self.args.save, dpi=300)
        else:
            plt.show()


def main():
    p = SkPlot()

if __name__ == "__main__":
    main()
