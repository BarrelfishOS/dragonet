## -*- coding: utf-8 -*-
##
## formatters.py
##
## Author:   Toke Høiland-Jørgensen (toke@toke.dk)
## Date:     16 October 2012
## Copyright (c) 2012, Toke Høiland-Jørgensen
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

import json, sys, csv, math, inspect, os
#import settings as settings

from itertools import cycle

import random

import matplotlib as mpl
## agg backend is used to create plot as a .png file
mpl.use('agg')
import matplotlib.pyplot as plt

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cbook as cbook


boxprops = dict(linestyle='--', linewidth=3, color='darkgoldenrod')
flierprops = dict(marker='o', markerfacecolor='green', markersize=12,
                  linestyle='none')
medianprops = dict(linestyle='-.', linewidth=2.5, color='firebrick')
meanpointprops = dict(marker='D', markeredgecolor='black',
                      markerfacecolor='firebrick')
meanlineprops = dict(linestyle='--', linewidth=2.5, color='purple')


from .util import cum_prob, frange
from functools import reduce

PLOT_KWARGS = (
    'alpha',
    'antialiased',
    'color',
    'dash_capstyle',
    'dash_joinstyle',
    'drawstyle',
    'fillstyle',
    'label',
    'linestyle',
    'linewidth',
    'lod',
    'marker',
    'markeredgecolor',
    'markeredgewidth',
    'markerfacecolor',
    'markerfacecoloralt',
    'markersize',
    'markevery',
    'pickradius',
    'solid_capstyle',
    'solid_joinstyle',
    'visible',
    'zorder'
    )


def target_lookup (addr):
    if (addr.startswith("127.")) :
        return "localhost"
    if (addr == "10.23.4.21") :
        return "SFD"
    if (addr == "10.22.4.11") :
        return "IntelD"
    if (addr == "192.168.123.1") :
        return "Tuntap"
    return addr


def plot_attr_details(trow, sorder, infod):
    nr = len(infod[trow[0]])
    data_to_plot = []
    axis_to_plot = []
    for i in range(0, nr):
        data_to_plot.append(infod["THROUGHPUT"][i])
        title="%s_%s_%d" % (
                    infod["ECHO_SERVER"][i],
                    target_lookup(infod["TARGET"][i]),
                    infod["REQUEST_SIZE"][i][0],
                    )
        title2= infod["TITLE"][i]
        axis_to_plot.append(title2)
    # Create a figure instance
    fig = plt.figure(1, figsize=(9, 6))
    # Create an axes instance
    ax = fig.add_subplot(111)
    # Create the boxplot
    bp = ax.boxplot(data_to_plot)

    ## Custom x-axis labels
    ax.set_xticklabels(axis_to_plot, fontsize=8, rotation=90)
    #ax.set_ylim(ymin=0, ymax=10)
    ax.set_ylim(ymin=0)
    # Save the figure
    fig.savefig('fig1.png', bbox_inches='tight')


def collect_attributes(result, config, titles, values):
    #for s in config['series']:
    for s in config['attrs']['attrlist']:
        args = None
        if 'value' in s.keys():
            ans = s['value']
        elif 'args' in s.keys():
            args = s['args']
            ans = s['data'](result._results, result.metadata, **args)
        else:
            ans = s['data'](result._results, result.metadata)

        #print "%s: %s " % (s['label'], str(ans))
        print "%s: %s: %s" % (s['label'], str(ans)[:100],
               result.metadata['TITLE'])
        if (s['label'] not in values.keys()) :
            titles.append(s['label'])
            values[s['label']] = [ans]
        else :
            values[s['label']].append(ans)


def mystr(obj):
    return str(obj)[:9]

def sort_attr_details(skey, trow, infod):
    if not skey in infod.keys():
        print "Can't sort results as given key [%s] is missing" % (key)
        return

    nr = len(infod[trow[0]])
    ll = []
    for i in range(0, nr):
        #ll.append((infod[skey][i], i))

        ll.append((
                infod[trow[0]][i],
                infod[trow[1]][i],
                infod[trow[2]][i],
                infod[trow[3]][i],
                infod[trow[4]][i],
                i))
    print ll
    sll = sorted(ll)
    sorder = map (lambda x: x[-1], sll)
    #print "sorted list = %s" % (sll)
    #print "sorted order = %s" % (sorder)
    return sorder


def show_attr_details(trow, sorder, infod):

#    trow = self.nresults_titles
#    infod = self.nresults

    for k in trow:
        print "|%10s " % (mystr(k)),
    print "|\n",

    nr = len(infod[trow[0]])
    if sorder == None or sorder == []:
        order = range(0, nr)
    else :
        order = sorder

    for i in order:
#        if infod['BURST_SIZE'][i][0] != 1.0 :
#            continue
#        if infod['TARGET'][i] != "10.23.4.21" :
#            continue

        for k in trow:
            data = infod[k][i]
            if k == "TITLE":
                print "|%30s " % (str(data)),
            else :
                print "|%10s " % (mystr(data)),
        print "|\n",


class Formatter(object):

    open_mode = "w"

    def __init__(self, settings):
        self.settings = settings
        self.check_output(self.settings.OUTPUT)
        self.extra_msg = ""
        self.nresults = {}
        self.nresults_titles = []



    def check_output(self, output):
        if hasattr(output, 'read') or output == "-":
            self.output = output
        else:
            # This logic is there to ensure that:
            # 1. If there is no write access, fail before running the tests.
            # 2. If the file exists, do not open (and hence overwrite it) until after the
            #    tests have run.
            if os.path.exists(output):
                # os.access doesn't work on non-existant files on FreeBSD; so only do the
                # access check on existing files (to avoid overwriting them before the tests
                # have completed).
                if not os.access(output, os.W_OK):
                    raise RuntimeError("No write permission for output file '%s'" % output)
                else:
                    self.output = output
            else:
                # If the file doesn't exist, just try to open it immediately; that'll error out
                # if access is denied.
                try:
                    self.output = open(output, self.open_mode)
                except IOError as e:
                    raise RuntimeError("Unable to open output file: '%s'" % e)

    def open_output(self):
        output = self.output
        if hasattr(output, 'read'):
            return
        if output == "-":
            self.output = sys.stdout
        else:
            try:
                self.output = open(output, self.open_mode)
            except IOError as e:
                raise RuntimeError("Unable to output data: %s" % e)

    def format(self, results):
        if results[0].dump_file is not None:
            sys.stderr.write("No output formatter selected.\nTest data is in %s (use with -i to format).\n" % results[0].dump_file)

    def format2(self, results, trow=None, sorder=None, infod=None):
        self.format(results)

DefaultFormatter = Formatter

class TableFormatter(Formatter):

    def get_header(self, results):
        name = results[0].meta("NAME")
        keys = list(self.settings.DATA_SETS.keys())
        header_row = [name]

        if len(results) > 1:
            for r in results:
                header_row += [k + ' - ' + r.label() for k in keys]
        else:
            header_row += keys
        return header_row

    def combine_results(self, results):
        """Generator to combine several result sets into one list of rows, by
        concatenating them."""
        keys = list(self.settings.DATA_SETS.keys())
        for row in list(zip(*[list(r.zipped(keys)) for r in results])):
            out_row = [row[0][0]]
            for r in row:
                if r[0] != out_row[0]:
                    raise RuntimeError("x-value mismatch: %s/%s. Incompatible data sets?" % (out_row[0], r[0]))
                out_row += r[1:]
            yield out_row


class OrgTableFormatter(TableFormatter):
    """Format the output for an Org mode table. The formatter is pretty crude
    and does not align the table properly, but it should be sufficient to create
    something that Org mode can correctly realign."""

    def format(self, results):
        self.open_output()
        name = results[0].meta("NAME")

        if not results[0]:
            self.output.write(str(name) + " -- empty\n")
            return
        header_row = self.get_header(results)
        self.output.write("| " + " | ".join(header_row) + " |\n")
        self.output.write("|-" + "-+-".join(["-"*len(i) for i in header_row]) + "-|\n")

        def format_item(item):
            if isinstance(item, float):
                return "%.2f" % item
            return str(item)

        for row in self.combine_results(results):
            self.output.write("| ")
            self.output.write(" | ".join(map(format_item, row)))
            self.output.write(" |\n")



class CsvFormatter(TableFormatter):
    """Format the output as csv."""

    def format(self, results):
        self.open_output()
        if not results[0]:
            return

        writer = csv.writer(self.output)
        header_row = self.get_header(results)
        writer.writerow(header_row)

        def format_item(item):
            if item is None:
                return ""
            return str(item)

        for row in self.combine_results(results):
            writer.writerow(list(map(format_item, row)))

class StatsFormatter(Formatter):

    def __init__(self, settings):
        Formatter.__init__(self, settings)
        try:
            import numpy
            self.np = numpy
        except ImportError:
            raise RuntimeError("Stats formatter requires numpy, which seems to be missing. Please install it and try again.")

    def format(self, results):
        self.open_output()
        self.output.write("Warning: Totals are computed as cumulative sum * step size,\n"
                          "so spurious values wreck havoc with the results.\n")
        for r in results:
            self.output.write("Results %s" % r.meta('TIME'))
            if r.meta('TITLE'):
                self.output.write(" - %s" % r.meta('TITLE'))
            self.output.write(":\n")

            for s in sorted(r.series_names):
                self.output.write(" %s:\n" % s)
                d = [i for i in r.series(s) if i]
                if not d:
                    self.output.write("  No data.\n")
                    continue
                cs = self.np.cumsum(d)
                units = self.settings.DATA_SETS[s]['units']
                self.output.write("  Data points: %d\n" % len(d))
                if units != "ms":
                    self.output.write("  Total:       %f %s\n" % (cs[-1]*r.meta('STEP_SIZE'),
                                                               units.replace("/s", "")))
                self.output.write("  Mean:        %f %s\n" % (self.np.mean(d), units))
                self.output.write("  Median:      %f %s\n" % (self.np.median(d), units))
                self.output.write("  Min:         %f %s\n" % (self.np.min(d), units))
                self.output.write("  Max:         %f %s\n" % (self.np.max(d), units))
                self.output.write("  Std dev:     %f\n" % (self.np.std(d)))
                self.output.write("  Variance:    %f\n" % (self.np.var(d)))


class PlotFormatter(Formatter):

    open_mode = "wb"

    def __init__(self, settings):
        Formatter.__init__(self, settings)
        try:
            import matplotlib, numpy
            # If saving to file, try our best to set a proper backend for
            # matplotlib according to the output file name. This helps with
            # running matplotlib without an X server.
            output = self.settings.OUTPUT
            if output != "-":
                if output.endswith('.svg') or output.endswith('.svgz'):
                    matplotlib.use('svg')
                elif output.endswith('.ps') or output.endswith('.eps'):
                    matplotlib.use('ps')
                elif output.endswith('.pdf'):
                    matplotlib.use('pdf')
                elif output.endswith('.png'):
                    matplotlib.use('agg')
                else:
                    raise RuntimeError("Unrecognised file format for output '%s'" % output)
            import matplotlib.pyplot as plt
            self.plt = plt
            self.np = numpy
            self.figure = self.plt.figure()
            self.init_plots()

            font = {'family' : 'normal',
                    'weight' : 'bold',
                    'size'   : 22}
            matplotlib.rc('font', **font)

        except ImportError:
            raise
            raise RuntimeError("Unable to plot -- matplotlib is missing! Please install it if you want plots.")


    def _load_plotconfig(self, plot):
        if not plot in self.settings.PLOTS:
            raise RuntimeError("Unable to find plot configuration '%s'" % plot)
        config = self.settings.PLOTS[plot].copy()
        if 'parent' in config:
            parent_config = self.settings.PLOTS[config['parent']].copy()
            parent_config.update(config)
            return parent_config
        return config

    def init_plots(self):
        self.figure.clear()
        self.config = self._load_plotconfig(self.settings.PLOT)
        self.configs = [self.config]
        getattr(self, '_init_%s_plot' % self.config['type'])()

    def _init_timeseries_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        if 'dual_axes' in config and config['dual_axes']:
            second_axis = self.figure.add_axes(axis.get_position(), sharex=axis, frameon=False)
            second_axis.yaxis.tick_right()
            axis.yaxis.tick_left()
            second_axis.yaxis.set_label_position('right')
            second_axis.yaxis.set_offset_position('right')
            second_axis.xaxis.set_visible(False)
            config['axes'] = [axis,second_axis]
        else:
            config['axes'] = [axis]

        unit = [None]*len(config['axes'])
        for s in config['series']:
            if 'axis' in s and s['axis'] == 2:
                a = 1
            else:
                a = 0

           #s_unit = self.settings.DATA_SETS[s['data']]['units']
            #if unit[a] is not None and s_unit != unit[a]:
            #    raise RuntimeError("Plot axis unit mismatch: %s/%s" % (unit[a], s_unit))
            #unit[a] = s_unit
            #unit[a] = "Gbits/s"

        axis.set_xlabel('Time')
        for i,u in enumerate(unit):
            if 'axis_labels' in config and config['axis_labels'][i]:
                config['axes'][i].set_ylabel(config['axis_labels'][i])
            else:
                config['axes'][i].set_ylabel(unit[i])


    def _init_box_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        self._init_timeseries_plot(config, axis)
        axis.set_xlabel('')

        self.start_position = 1

    def _init_box2_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        self._init_timeseries_plot(config, axis)
        axis.set_xlabel('')

        self.start_position = 1

    def _init_boxc_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        self._init_timeseries_plot(config, axis)
        axis.set_xlabel('')

        self.start_position = 1

    def _init_cdf2_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        self._init_timeseries_plot(config, axis)
        axis.set_xlabel('')

        self.start_position = 1



    def _init_cdf_plot(self, config=None, axis=None):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        unit = None
        for s in config['series']:
            s_unit = self.settings.DATA_SETS[s['data']]['units']
            if unit is not None and s_unit != unit:
                raise RuntimeError("Plot axis unit mismatch: %s/%s" % (unit, s_unit))
            unit = s_unit

        if 'axis_labels' in config and config['axis_labels'][0]:
            axis.set_xlabel(config['axis_labels'][0])
        else:
            axis.set_xlabel(unit)
        axis.set_ylabel('Cumulative probability')
        axis.set_ylim(0,1)
        config['axes'] = [axis]
        self.medians = []
        self.min_vals = []


    def _init_meta_plot(self):
        self.configs = []
        ax = self.figure.gca()
        ax.set_axis_off()
        for i,subplot in enumerate(self.config['subplots']):
            axis = self.figure.add_subplot(len(self.config['subplots']),1,i+1, sharex=self.figure.gca())
            config = self._load_plotconfig(subplot)
            self.configs.append(config)
            getattr(self, '_init_%s_plot' % config['type'])(config=config, axis=axis)
            if i < len(self.config['subplots'])-1:
                axis.set_xlabel("")


    def do_timeseries_plot(self, results, config=None, axis=None):
        if len(results) > 1:
            for r in results:
                #self.settings.update(r.meta())
                self._do_timeseries_plot(r, config=config, axis=axis, postfix=" - "+r.label())
        else:
            self._do_timeseries_plot(results[0], config=config, axis=axis)


    def _gen_table(result, config=None):
        for s in config['series']:
            args = None
            if 'args' in s.keys():
                args = s['args']
                ans = s['data'](result._results, result.metadata, **args)
            else:
                ans = s['data'](result._results, result.metadata)

            print "%s: %s: %s" % (s['label'], str(ans),
                    result.metadata['TITLE'])


    def _do_timeseries_plot(self, results, config=None, axis=None, postfix=""):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        #axis.set_xlim(0, max(results.x_values+[self.settings.TOTAL_LENGTH]))
        data = []
        for i in range(len(config['axes'])):
            data.append([])

        for s in config['series']:
            if not s['data'] in results.series_names:
                args = None
                if 'args' in s.keys():
                    args = s['args']
                    ans = s['data'](results._results, results.metadata, **args)
                else:
                    ans = s['data'](results._results, results.metadata)

                #print "%s: %s " % (s['label'], str(ans))
                print "%s: %s: %s" % (s['label'], str(ans),
                       results.metadata['TITLE'])
                if (s['label'] not in self.nresults.keys()) :

                    self.nresults_titles.append(s['label'])
                    self.nresults[s['label']] = [ans]
                else :
                    self.nresults[s['label']].append(ans)

                y_values = ans
                x_values = range(1,len(y_values)+1)
                continue

            if 'smoothing' in s:
                smooth=s['smoothing']
            else:
                smooth = False
            kwargs = {}
            for k in PLOT_KWARGS:
                if k in s:
                    kwargs[k] = s[k]

            if 'label' in kwargs:
                kwargs['label']+=postfix

            #y_values = results.series(s['data'], smooth)
            if (not y_values):
                y_values = results.series(s['data'], smooth)
            if 'axis' in s and s['axis'] == 2:
                a = 1
            else:
                a = 0
            data[a] += y_values
            for r in self.settings.SCALE_DATA:
                data[a] += r.series(s['data'], smooth)
            #config['axes'][a].plot(results.x_values,
            config['axes'][a].plot(x_values,
                   y_values,
                   **kwargs)

        if 'scaling' in config:
            btm,top = config['scaling']
        else:
            btm,top = 0,100

        for a in range(len(config['axes'])):
            if data[a]:
                self._do_scaling(config['axes'][a], data[a], btm, top)



    def do_cdf2_plot(self, results, config=None, axis=None):
        if config is None:
            config = self.config
        axis = config['axes'][0]

        nr = len(self.infod[self.trow[0]])
        data_to_plot = []
        axis_to_plot = []

        group_size = nr # len(results)
        ticklabels = []
        ticks = []
        pos = 1

        for i in range(0, nr):
            title="%s_%s" % (
                    self.infod["ECHO_SERVER"][i],
                    target_lookup(self.infod["TARGET"][i]),
                    )
            title2 = self.infod["TITLE"][i]
            axis_to_plot.append(title2)
            ticklabels.append(title2)
            #positions = range(i,pos+group_size)
            #ticks.append(self.np.mean(positions))

        colours = ['b', 'g', 'c', 'm', 'k']
        while len(colours) < len(results):
            colours = colours *2



        kk = 0
        for i,s in enumerate(config['series']):
            if 'axis' in s and s['axis'] == 2:
                a = 1
            else:
                a = 0

            print i
            data = []
            data_labels = []

            if 'label' in s:
                for i in range(0, nr):
                    data_to_plot.append(self.infod[s['label']][i])
                    dd = map ((lambda x: int(x)), self.infod[s['label']][i])
                    title="%s_%s" % (
                            self.infod["ECHO_SERVER"][i],
                            target_lookup(self.infod["TARGET"][i]),
                            )
                    title2 = self.infod["TITLE"][i][:-11]
                    title2 = title2.replace('_', ' ')
                    title2 = title2.replace('Dpdk', ' Dragonet')
                    title2 = title2.replace('llvmE10k', 'llvm E10k Dragonet')
                    title2 = title2.replace('CImplOnload', 'CImpl Dragonet')
                    title2 = title2.replace('CImpl', 'C')
                    data.append((dd, title2))

            lines = ["-","--","-.",":"]
            linecycler = cycle(lines)
            print "The length of data is %d"  % (len(data))
            for dd,t in data:
                num_bins = 100
                randIndex = random.sample(range(len(dd)), 10000)
                randIndex.sort()
                rand = [dd[i] for i in randIndex]
                samples = rand # dd
                counts, bin_edges = np.histogram(samples, bins=num_bins, normed=False)
                cdf = np.cumsum(counts)
                #pylab.plot(bin_edges[1:], cdf)
                bp = config['axes'][a].plot( bin_edges[1:], cdf, next(linecycler) ,label=t) #   boxplot(data)
                                           #positions=positions)
            #self.plt.setp(bp['boxes'][a], color=colours[kk])
            #kk = kk + 1
#                if i == 0 and group_size > 1:
#                    bp['caps'][j*2].set_label(r.label())
#                for k in 'caps','whiskers','fliers':
#                    if bp[k]:
#                        self.plt.setp(bp[k][j*2], color=colours[j])
#                        self.plt.setp(bp[k][j*2+1], color=colours[j])
#
#            pos += group_size+1

        #axis.set_xticks(ticks)
        #axis.set_xticklabels(ticklabels, fontsize=9, rotation=90)


        #axis.set_xlim(0,pos-1)
        #axis.set_ylim(ymin=0, ymax=5000)

        #axis.set_yscale('log')

        #axis.set_xscale('log')

        axis.set_ylim(ymin=0, ymax=11000)
        axis.set_xlim(xmin=10, xmax=80)
        axis.set_xlabel("RTT Latency (usec)")


    def do_boxc_plot(self, results, config=None, axis=None):
        if config is None:
            config = self.config
        axis = config['axes'][0]

        nr = len(self.infod[self.trow[0]])
        data_to_plot = []
        axis_to_plot = []

        stats = []
        ticklabels = []
        ymax = 0
        #myselector = np.mean
        myselector = (lambda x: x[0])
        for i in range(0, nr):
            s1 = {}
            title="%s_%s" % (
                    self.infod["ECHO_SERVER"][i],
                    target_lookup(self.infod["TARGET"][i]),
                    )

            ticklabels.append(title)
            s1['label'] = title
            s1['q1'] = myselector(self.infod["P50_LATENCY"][i])
            s1['med'] = myselector(self.infod["P90_LATENCY"][i])
            s1['q3'] = myselector(self.infod["P99_LATENCY"][i])
            s1['whislo'] = myselector(self.infod["MIN_LATENCY"][i])
            s1['whishi'] = myselector(self.infod["MAX_LATENCY"][i])
            s1['fliers'] = []
            if s1['q3'] > ymax :
                ymax = s1['q3']
            stats.append(s1)
            #positions = range(i,pos+group_size)
            #ticks.append(self.myselector(positions))

        colours = ['b', 'g', 'c', 'm', 'k']

        #axis.bxp(stats, boxprops=boxprops)
        axis.bxp(stats, showfliers=False)

        #axis.set_xticks(ticks)
        axis.set_xticklabels(ticklabels, fontsize=9, rotation=90)


        #axis.set_xlim(0,pos-1)
        #axis.set_ylim(ymin=0, ymax=10)
        axis.set_ylim(ymin=0, ymax=(ymax * 1.1))

    def do_box2_plot(self, results, config=None, axis=None):
        if config is None:
            config = self.config
        axis = config['axes'][0]

        nr = len(self.infod[self.trow[0]])
        data_to_plot = []
        axis_to_plot = []

        group_size = nr # len(results)
        ticklabels = []
        ticks = []
        pos = 1

        boxprops = dict(linestyle='--', linewidth=3, color='darkgoldenrod')
        flierprops = dict(marker='o', markerfacecolor='green', markersize=12,
                  linestyle='none')
        medianprops = dict(linestyle='-.', linewidth=2.5, color='firebrick')
        meanpointprops = dict(marker='D', markeredgecolor='black',
                      markerfacecolor='firebrick')
        meanlineprops = dict(linestyle='--', linewidth=2.5, color='purple')


        if self.sorder == None or self.sorder == []:
            order = range(0, nr)
        else :
            order = self.sorder

        for i in order:
            #print self.infod
            title="%s_%s" % (
                    #self.infod["ECHO_SERVER"][i],
                    self.infod["Server"][i],
                    self.infod["USE_TCP"][i],
                    #target_lookup(self.infod["TARGET"][i]),
                    )
            title1 = self.infod["TITLE"][i]
            title1 = title1.replace('_', ' ')
            t3 = title1.split()
            title2 = ""
            for t in t3:
                if t == "PKT":
                    break
                title2 = " %s %s" % (title2, t)
            title2 = title2.replace('Dpdk', ' Dragonet')
            title2 = title2.replace('llvmE10k', 'llvm E10k Dragonet')
            title2 = title2.replace('CImplOnload', 'CImpl Dragonet')
            title2 = title2.replace('CImpl', 'C')

            title3 = "%d,%d (%d)" % (self.infod['SERVERS_INSTANCES'][i],
                    self.infod['SERVER_CORES'][i],
                    self.infod['TCONCURRENCY'][i])

            axis_to_plot.append(title3)
            ticklabels.append(title3)
            #positions = range(i,pos+group_size)
            #ticks.append(self.np.mean(positions))

        colours = ['b', 'g', 'c', 'm', 'k']
        while len(colours) < len(results):
            colours = colours *2

        kk = 0
        for i,s in enumerate(config['series']):
            if 'axis' in s and s['axis'] == 2:
                a = 1
            else:
                a = 0

            print i
            data = []

            if 'label' in s:
                for i in order:
                    data_to_plot.append(self.infod[s['label']][i])
                    data.append(self.infod[s['label']][i])

            bp = config['axes'][a].boxplot(data)
                                           #positions=positions)
            #self.plt.setp(bp['boxes'][a], color=colours[kk])
            #kk = kk + 1
#                if i == 0 and group_size > 1:
#                    bp['caps'][j*2].set_label(r.label())
#                for k in 'caps','whiskers','fliers':
#                    if bp[k]:
#                        self.plt.setp(bp[k][j*2], color=colours[j])
#                        self.plt.setp(bp[k][j*2+1], color=colours[j])
#
#            pos += group_size+1

        #axis.set_xticks(ticks)
        axis.set_xticklabels(ticklabels, fontsize=15, rotation=90)
        #axis.set_xticklabels(ticklabels, rotation=90)



        #axis.set_xlim(0,pos-1)
        #axis.set_ylim(ymin=0, ymax=110)
        axis.set_ylim(ymin=0 )


    def do_box_plot(self, results, config=None, axis=None):
        if config is None:
            config = self.config
        axis = config['axes'][0]

        group_size = len(results)
        ticklabels = []
        ticks = []
        pos = 1

        colours = ['b', 'g', 'c', 'm', 'k']
        while len(colours) < len(results):
            colours = colours *2

        for i,s in enumerate(config['series']):
            if 'axis' in s and s['axis'] == 2:
                a = 1
            else:
                a = 0

            data = []
            for r in results:
                if 'args' in s.keys():
                    args = s['args']
                    val = s['data'](r._results, r.metadata, **args)
                else:
                    val = s['data'](r._results, r.metadata)

                # val =  s['data'](r._results, r.metadata)

                data.append(val)
                #data.append([i for i in r.series(s['data']) if i is not None])

            if 'label' in s:
                ticklabels.append(s['label'])
                print "appending %s" % (s['label'])
            else:
                ticklabels.append(i)
                print "appending-2 %s" % (s['label'])

            positions = range(pos,pos+group_size)
            ticks.append(self.np.mean(positions))

            bp = config['axes'][a].boxplot(data,
                                           positions=positions)

            for j,r in enumerate(results):
                self.plt.setp(bp['boxes'][j], color=colours[j])
                if i == 0 and group_size > 1:
                    bp['caps'][j*2].set_label(r.label())
                for k in 'caps','whiskers','fliers':
                    if bp[k]:
                        self.plt.setp(bp[k][j*2], color=colours[j])
                        self.plt.setp(bp[k][j*2+1], color=colours[j])

            pos += group_size+1


        axis.set_xticks(ticks)
        axis.set_xticklabels(ticklabels)
        axis.set_xlim(0,pos-1)

        if 'scaling' in config:
            btm,top = config['scaling']
        else:
            btm,top = 0,100

        for a in range(len(config['axes'])):
            if data[a]:
                self._do_scaling(config['axes'][a], data[a], btm, top)



    def do_cdf_plot(self, results, config=None, axis=None):
        if len(results) > 1:
            for r in results:
                self._do_cdf_plot(r, config=config, axis=axis, postfix=" - "+r.label())
        else:
            self._do_cdf_plot(results[0], config=config, axis=axis)

    def _do_cdf_plot(self, results, config=None, axis=None, postfix=""):
        if axis is None:
            axis = self.figure.gca()
        if config is None:
            config = self.config

        data = []
        sizes = []
        max_value = 0.0
        for s in config['series']:
            if not s['data'] in results.series_names:
                data.append([])
                continue
            s_data = results.series(s['data'])
            if 'cutoff' in config:
                # cut off values from the beginning and end before doing the
                # plot; for e.g. pings that run long than the streams, we don't
                # want the unloaded ping values
                start,end = config['cutoff']
                end = -int(end/self.settings.STEP_SIZE)
                if end == 0:
                    end = None
                s_data = s_data[int(start/self.settings.STEP_SIZE):end]
            sizes.append(float(len(s_data)))
            d = sorted([x for x in s_data if x is not None])
            data.append(d)
            if d:
                self.medians.append(self.np.median(d))
                self.min_vals.append(min(d))
                max_value = max([max_value]+d)

                for r in self.settings.SCALE_DATA:
                    d_s = [x for x in r.series(s['data']) if x is not None]
                    if d_s:
                        max_value = max([max_value]+d_s)


        x_values = list(frange(0, max_value, 0.1))


        for i,s in enumerate(config['series']):
            kwargs = {}
            for k in PLOT_KWARGS:
                if k in s:
                    kwargs[k] = s[k]
            if 'label' in kwargs:
                kwargs['label']+=postfix
            axis.plot(x_values,
                      [cum_prob(data[i], point, sizes[i]) for point in x_values],
                      **kwargs)

        if self.medians and max(self.medians)/min(self.medians) > 10.0:
            # More than an order of magnitude difference; switch to log scale
            axis.set_xscale('log')
            min_val = min(self.min_vals)
            if min_val > 10:
                min_val -= min_val%10 # nearest value divisible by 10
            axis.set_xlim(left=min_val)

    def do_meta_plot(self, results):
        for i,config in enumerate(self.configs):
            getattr(self, 'do_%s_plot' % config['type'])(results, config=config)

    def format2(self, results, trow, sorder, infod):
        if not trow[0]:
            return
        self.trow = trow
        self.sorder = sorder
        self.infod = infod

        getattr(self, 'do_%s_plot' % self.config['type'])(results)
        skip_title = len(results) > 1

        artists = []
        legend_exists = False
        for c in self.configs:
            legends = self._do_legend(c)
            if legends:
                artists += legends
                legend_exists = True

        artists += self._annotate_plot(skip_title)

        # Since outputting image data to stdout does not make sense, we launch
        # the interactive matplotlib viewer if stdout is set for output.
        # Otherwise, the filename is passed to matplotlib, which selects an
        # appropriate output format based on the file name.
        if self.output == "-":
            # For the interactive viewer there's no bbox_extra_artists, so we
            # need to reduce the axis sizes to make room for the legend (which
            # might still be slightly cut off).
            if self.settings.PRINT_LEGEND and legend_exists:
                for a in reduce(lambda x,y:x+y, [i['axes'] for i in self.configs]):
                    box = a.get_position()
                    a.set_position([box.x0, box.y0, box.width * 0.8, box.height])
            if not self.settings.GUI:
                self.plt.show()
        else:
            try:
                self.figure.savefig(self.output, bbox_extra_artists=artists, bbox_inches='tight')
            except IOError as e:
                raise RuntimeError("Unable to save output plot: %s" % e)



    def format(self, results):
        if not results[0]:
            return

        getattr(self, 'do_%s_plot' % self.config['type'])(results)
        skip_title = len(results) > 1

        artists = []
        legend_exists = False
        for c in self.configs:
            legends = self._do_legend(c)
            if legends:
                artists += legends
                legend_exists = True

        artists += self._annotate_plot(skip_title)

        # Since outputting image data to stdout does not make sense, we launch
        # the interactive matplotlib viewer if stdout is set for output.
        # Otherwise, the filename is passed to matplotlib, which selects an
        # appropriate output format based on the file name.
        if self.output == "-":
            # For the interactive viewer there's no bbox_extra_artists, so we
            # need to reduce the axis sizes to make room for the legend (which
            # might still be slightly cut off).
            if self.settings.PRINT_LEGEND and legend_exists:
                for a in reduce(lambda x,y:x+y, [i['axes'] for i in self.configs]):
                    box = a.get_position()
                    a.set_position([box.x0, box.y0, box.width * 0.8, box.height])
            if not self.settings.GUI:
                self.plt.show()
        else:
            try:
                self.figure.savefig(self.output, bbox_extra_artists=artists, bbox_inches='tight')
            except IOError as e:
                raise RuntimeError("Unable to save output plot: %s" % e)


    def _annotate_plot(self, skip_title=False):
        titles = []
        if self.settings.PRINT_TITLE:
            plot_title = self.settings.DESCRIPTION
            y=0.98
            if 'description' in self.config:
                plot_title += "\n" + self.config['description']
            if self.settings.TITLE and not skip_title:
                plot_title += "\n" + self.settings.TITLE
            if 'description' in self.config and self.settings.TITLE and not skip_title:
                y=1.00
            titles.append(self.figure.suptitle(plot_title, fontsize=14, y=y))

        if self.settings.ANNOTATE:
            annotation_string = "client/remote: %s/%s - Target: %s - Time: %s" % (
                str(self.settings.CLIENTS), self.settings.SERVERS,
                self.settings.TARGET,
                self.settings.TIME)
            titles.append(self.figure.text(0.5, 0.0, annotation_string,
                                            horizontalalignment='center',
                                            verticalalignment='bottom',
                                            fontsize=8))
        return titles

    def _do_legend(self, config, postfix=""):
        print "called _do_legend"
        if not self.settings.PRINT_LEGEND:
            return []

        axes = config['axes']

        # Each axis has a set of handles/labels for the legend; combine them
        # into one list of handles/labels for displaying one legend that holds
        # all plot lines
        handles, labels = reduce(lambda x,y:(x[0]+y[0], x[1]+y[1]),
                                 [a.get_legend_handles_labels() for a in axes])

        if not labels:
            print "no labels"
            return []

        kwargs = {}
        if 'legend_title' in config:
            kwargs['title'] = config['legend_title']


        if len(axes) > 1:
            offset_x = 1.09
        else:
            offset_x = 1.02

        legends = []
        l = axes[0].legend(handles, labels,
                                bbox_to_anchor=(offset_x, 1.0),
                                loc='upper left', borderaxespad=0.,
                                prop={'size':'small'},
                                **kwargs)

        # Work around a bug in older versions of matplotlib where the
        # legend.get_window_extent method does not take any arguments, leading
        # to a crash when using bbox_extra_artists when saving the figure
        #
        # Simply check for either the right number of args, or a vararg
        # specification, and if they are not present, attempt to monkey-patch
        # the method if it does not accept any arguments.
        a,v,_,_ = inspect.getargspec(l.get_window_extent)
        if len(a) < 2 or v is None:
            def get_window_extent(*args, **kwargs):
                return l.legendPatch.get_window_extent(*args, **kwargs)
            l.get_window_extent = get_window_extent
        legends.append(l)
        return legends

    def _do_scaling(self, axis, data, btm, top):
        """Scale the axis to the selected bottom/top percentile"""
        data = [x for x in data if x is not None]
        if not data:
            return
        top_percentile = self.np.percentile(data, top)*1.05
        btm_percentile = self.np.percentile(data, btm)*0.95
        if self.settings.ZERO_Y:
            axis.set_ylim(ymin=0, ymax=top_percentile)
        else:
            axis.set_ylim(ymin=btm_percentile, ymax=top_percentile)
            if top_percentile/btm_percentile > 20.0 and self.settings.LOG_SCALE:
                axis.set_yscale('log')



    def show_nresults(self):
        show_attr_details(self.nresults_titles, self.nresults)
        return

        trow = self.nresults_titles
        infod = self.nresults
        for k in trow:
            print "|%10s " % (mystr(k)),
        print "|\n",

        nr = len(infod[trow[0]])
        for i in range(0, nr):
#            if infod['BURST_SIZE'][i][0] != 1.0 :
#                continue
#            if infod['TARGET'][i] != "10.23.4.21" :
#                continue

            for k in trow:
                data = infod[k][i]
                if k == "TITLE":
                    print "|%30s " % (str(data)),
                else :
                    print "|%10s " % (mystr(data)),
            print "|\n",

def mystr(obj):
    return str(obj)[:9]


class MetadataFormatter(Formatter):

    def format(self, results):
        self.open_output()
        self.output.write(json.dumps([r.serialise_metadata() for r in results], indent=4) + "\n")
