## -*- coding: utf-8 -*-
##
## settings.py
##
## Author:   Toke Høiland-Jørgensen (toke@toke.dk)
## Date:     25 November 2012
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

import sys, os, optparse, socket, subprocess, time

from datetime import datetime
from fnmatch import fnmatch

try:
    from configparser import RawConfigParser
except ImportError:
    from ConfigParser import RawConfigParser

try:
    from collections import OrderedDict
except ImportError:
    from netperf_wrapper.ordereddict import OrderedDict
from netperf_wrapper.resultset import ResultSet
from netperf_wrapper.build_info import DATA_DIR, VERSION
from netperf_wrapper.metadata import record_extended_metadata, record_machine_metadata
from netperf_wrapper import util


LOGFILE_HANDLE = None

def writeLog(msg):
    #global LOGFILE_HANDLE
    if LOGFILE_HANDLE:
        LOGFILE_HANDLE.write("%s\n" % (msg))

def pwLog(msg):
    #global LOGFILE_HANDLE
    if LOGFILE_HANDLE:
        LOGFILE_HANDLE.write("%s\n" % (msg))
    print ("%s\n" % (msg))


def timeStamped(t=None, fmt='%Y%m%d%H%M%S'):
    if t :
        return t.strftime(fmt).format()
    else:
       return datetime.now().strftime(fmt).format()

DEFAULT_SETTINGS = {
    'NAME': None,
    'HOST': None,
    'HOSTS': [],
    'TARGET': None,
    'USE_TCP': False,
    'SERVERS': [],
    'SERVERS_IF': {},
    'SERVERS_DRV': {},
    'RUN_CONF': {},
    'SERVERS_CORECOUNT': {},
    'SERVERS_INSTANCES': 1,
    'SERVER_CORESHIFT' : 0,
    'CLIENTS': [],
    'CLIENTS_IF': {},
    'CLIENTS_CORECOUNT': {},
    'SERVER_CORES': 1,
    'CLIENT_CORES': 1,
    'CLIENT_PORTS_SHIFT': 0,
    'HWQUEUES': 1,
    'TOTAL_CLIENTS': 0,
    'BRUST_SIZE': 1,
    'CONCURRENCY' : 1,
    'TCONCURRENCY' : 1,
    'SPECIAL_CLIENTS_COUNT' : 0,
    'SPECIAL_SERVERS_COUNT' : 0,
    'PKT_SIZE': 1024,
    'LOCAL_HOST': socket.gethostname(),
    'STEP_SIZE': 0.2,
    'LENGTH': 10,
    'OUTPUT': '-',
    'FORMAT': 'default',
    'TITLE': '',
    'NOTE': '',
    'RCFILE': os.path.expanduser("~/.netperf-wrapperrc"),
    'LOG_FILE': None,
    'INPUT': [],
    'DESCRIPTION': 'No description',
    'PLOTS': {},
    'IP_VERSION': None,
    'DELAY': 2,
    'SOCKET_TIMEOUT': 2,
    'TIME': datetime.now(),
    'SCALE_DATA': [],
    'SCALE_MODE': False,
#    'ANNOTATE': True,
    'ANNOTATE': False,
    'PRINT_TITLE': True,
    'PRINT_LEGEND': True,
    'ZERO_Y': True,
    'LOG_SCALE': True,
    'EXTENDED_METADATA': False,
    'REMOTE_METADATA': [],
    'MINFO_SERVER': {},
    'MINFO_CLIENT': {},
    'MINFO': {},
    'RESULT_LOCATION_BASE2': [],
    'TOOLS_LOCATION': '${HOME}/dragonet/benchmarking/netperf-wrapper/',
    'GUI': False,
    'ITERATIONS': 1,
#   'ECHO_SERVER': 'netserver',
#    'ECHO_SERVER': 'netcat',
    'ECHO_SERVER': 'HImplOnload',
    'SERVER_ONLOAD_CMDLINE' : "",
    'SERVER_ONLOAD_ENABLED' : False,
    'SERVER_ONLOAD_LATENCY' : False,
    'REPORT_START_RUN': '',
    'DRAGONET_COST_FUNCTION' : '',
    #sudo onload --profile=latency --preload=/usr/lib64/libonload.so socat PIPE UDP-LISTEN:7,fork
    }

CONFIG_TYPES = {
    'HOSTS': 'list',
    'CLIENTS': 'list',
    'TARGER': 'str',
    'MINFO': 'list',
    'RESULT_LOCATION_BASE2': 'str',
    'TOOLS_LOCATION': 'str',
    'STEP_SIZE': 'float',
    'LENGTH': 'int',
    'OUTPUT': 'str',
    'FORMAT': 'str',
    'TITLE': 'str',
    'NOTE': 'str',
    'LOG_FILE': 'str',
    'IP_VERSION': 'int',
    'DELAY': 'int',
    'SOCKET_TIMEOUT': 'int',
    'SCALE_MODE': 'bool',
    'ANNOTATE': 'bool',
    'USE_TCP': 'bool',
    'PRINT_TITLE': 'bool',
    'PRINT_LEGEND': 'bool',
    'ZERO_Y': 'bool',
    'LOG_SCALE': 'bool',
    'EXTENDED_METADATA': 'bool',
    'ITERATIONS': 'int',
    'ECHO_SERVER': 'str',
    'SERVER_ONLOAD_CMDLINE' : 'str',
    }

TEST_PATH = os.path.join(DATA_DIR, 'tests')
DICT_SETTINGS = ('DATA_SETS', 'PLOTS', 'ATTRIBUTES' )

def version(*args):
    print("Netperf-wrapper v%s.\nRunning on Python %s." %(VERSION, sys.version.replace("\n", " ")))
    try:
        import matplotlib, numpy
        print("Using matplotlib version %s on numpy %s." % (matplotlib.__version__, numpy.__version__))
    except ImportError:
        print("No matplotlib found. Plots won't be available.")
    sys.exit(0)


class Glob(object):
    """Object for storing glob patterns in matches"""

    def __init__(self, pattern, exclude=None):
        if exclude is None:
            self.exclude = []
        else:
            self.exclude = exclude
        self.pattern = pattern

    def filter(self, values, exclude):
        exclude += self.exclude
        return [x for x in values if fnmatch(x, self.pattern) and x not in exclude]

    def __iter__(self):
        return iter((self,)) # allow list(g) to return [g]

    @classmethod
    def filter_dict(cls, d):
        # Expand glob patterns in parameters. Go through all items in the
        # dictionary looking for subkeys that is a Glob instance or a list
        # that has a Glob instance in it.
        for k,v in list(d.items()):
            for g_k in list(v.keys()):
                try:
                    v[g_k] = cls.expand_list(v[g_k], list(d.keys()), [k])
                except TypeError:
                    continue
        return d

    @classmethod
    def expand_list(cls, l, values, exclude=None):
        l = list(l) # copy list, turns lone Glob objects into [obj]
        if exclude is None:
            exclude = []
        # Expand glob patterns in list. Go through all items in the
        # list  looking for Glob instances and expanding them.
        for i in range(len(l)):
            pattern = l[i]
            if isinstance(pattern, cls):
                l[i:i+1] = pattern.filter(values, exclude)
        return l

class TestEnvironment(object):

    def __init__(self, env={}, informational=False):
        self.env = dict(env)
        self.env.update({
            'glob': Glob,
            'o': OrderedDict,
            'include': self.include_test,
            'min_host_count': self.require_host_count,
            'find_ping': self.find_ping,
            'find_netperf': self.find_netperf,
            })
        self.informational = informational
        self.netperf = None

    def execute(self, filename):
        try:
            #print "Compiling %s" % (filename)
            comp = compile(open(filename).read(), filename, 'exec')
            #print "evaluationg %s" % (filename)
            exec(comp, self.env)
            return self.env
        except (IOError, SyntaxError):
            t, value, traceback = sys.exc_info()
            print('Error opening %s: %s: %s' % (t, value, traceback))
            raise RuntimeError("Unable to read test config file: '%s'" % filename)

    def include_test(self, name, env=None):
        self.execute(os.path.join(TEST_PATH, name))

    def find_ping(self, ip_version, interval, length, host):
        """Find a suitable ping executable, looking first for a compatible
        `fping`, then falling back to the `ping` binary. Binaries are checked
        for the required capabilities."""

        # This can take a while, so skip if the tests are only loaded for informational
        # purposes (e.g. for --list-tests)
        if self.informational:
            return ""

        if ip_version == 6:
            suffix = "6"
        else:
            suffix = ""

        fping = util.which('fping'+suffix)
        ping = util.which('ping'+suffix)

        if fping is not None:
            proc = subprocess.Popen([fping, '-h'],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            out,err = proc.communicate()
            if "print timestamp before each output line" in str(out):
                # fping has timestamp option, use it
                # there's no timeout parameter for fping, calculate a total number
                # of pings to send
                count = length // interval + 1
                interval = int(interval * 1000)

                return "%s -D -p %d -c %d %s" % (fping, interval, count, host)

        if ping is not None:
            # No checks atm; should check for presence of -D parameter
            return "%s -n -D -i %.2f -w %d %s" % (ping, max(0.2, interval), length, host)

        raise RuntimeError("No suitable ping tool found.")


    def find_netperf(self, test, length, host, ip_version=None, marking=None, interval=None, extra_args=None):
        """Find a suitable netperf executable, and test for the required capabilities."""
        # This can take a while, so skip if the tests are only loaded for informational
        # purposes (e.g. for --list-tests)
        print "Called function find_netperf"
        if self.informational:
            return ""
        if ip_version is None:
            ip_version = self.env['IP_VERSION']

        if interval is None:
            interval = self.env['STEP_SIZE']

        args = "-P 0 -v 0 -D -%.1f" % interval
        if ip_version == 4:
            args += " -4"
        elif ip_version == 6:
            args += " -6"

        if marking is not None:
            args += " -Y %s" % marking

        args += " -H %s -t %s -l %d" % (host, test, length)

        if self.netperf is None:
            netperf = util.which('netperf')
            if netperf is None:
                raise RuntimeError("No netperf binary found in PATH.")

            # Try to figure out whether this version of netperf supports the -e
            # option for socket timeout on UDP_RR tests, and whether it has been
            # compiled with --enable-demo. Unfortunately, the --help message is
            # not very helpful for this, so the only way to find out is try to
            # invoke it and check for an error message. This has the side-effect
            # of having netperf attempt a connection to localhost, which can
            # stall, so we kill the process almost immediately.

            proc = subprocess.Popen([netperf, '-l', '1', '-D', '-0.2', '--', '-e', '1'],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            time.sleep(0.1) # should be enough time for netperf to output any error messages
            proc.kill()
            out,err = proc.communicate()
            if "Demo Mode not configured" in str(out):
                raise RuntimeError("%s does not support demo mode." % netperf)

            if "invalid option -- '0'" in str(err):
                raise RuntimeError("%s does not support accurate intermediate time reporting. You need netperf v2.6.0 or newer." % netperf)

            self.netperf = {'executable': netperf, "-e": False}

            if not "netperf: invalid option -- 'e'" in str(err):
                self.netperf['-e'] = True

        if extra_args is not None:
            args += " " + extra_args

        if test == "UDP_RR" and self.netperf["-e"]:
            # -- might have been passed as extra_args
            if not "--" in args:
                args += " --"
            args += " -e %d" % self.env['SOCKET_TIMEOUT']
        elif test in ("TCP_STREAM", "TCP_MAERTS", "omni"):
            args += " -f m"

        return "%s %s" % (self.netperf['executable'], args)


    def require_host_count(self, count):
        if len(self.env['HOSTS']) < count:
            if self.informational:
                self.env['HOSTS'] = ['dummy']*count
            elif 'DEFAULTS' in self.env and 'HOSTS' in self.env['DEFAULTS'] and self.env['DEFAULTS']['HOSTS']:
                # If a default HOSTS list is set, populate the HOSTS list with
                # values from this list, repeating as necessary up to count
                def_hosts = self.env['DEFAULTS']['HOSTS']
                host_c = len(self.env['HOSTS'])
                missing_c = count-host_c
                self.env['HOSTS'].extend((def_hosts * (missing_c//len(def_hosts)+1))[:missing_c])
                if not self.env['HOST']:
                    self.env['HOST'] = self.env['HOSTS'][0]
            else:
                raise RuntimeError("Need %d hosts, only %d specified" % (count, len(self.env['HOSTS'])))

parser = optparse.OptionParser(description='Wrapper to run concurrent netperf-style tests.',
                               usage="Usage: %prog [options] <host|test|input file ...> ")

parser.add_option("-o", "--output", action="store", type="string", dest="OUTPUT",
                  help="File to write processed output to (default standard out). The JSON "
                  "data file is written to the same directory as this file, if provided. "
                  "Otherwise, the data file is written to the current directory.")
parser.add_option("-i", "--input", action="append", type="string", dest="INPUT",
                  help="File to read input from (instead of running tests). Input files "
                  "can also be specified as unqualified arguments without using the -i switch.")
parser.add_option("-f", "--format", action="store", type="string", dest="FORMAT",
                  help="Select output format (plot, csv, org_table, stats, metadata). Default is "
                  "no processed output (just writes the JSON data file).")
parser.add_option("-p", "--plot", action="store", type="string", dest="PLOT",
                  help="Select which plot to output for the given test (implies -f plot). "
                  "Use the --list-plots option to see available plots.")
parser.add_option("-t", "--title-extra", action="store", type="string", dest="TITLE",
                  help="Text to add to plot title and data file name.")
#parser.add_option("-b", "--brust", action="store", type="int", dest="BRUST_SIZE",
#                  help="Number of concurrent transactions")

parser.add_option("--report-start-run", action="store", type="string", dest="REPORT_START_RUN",
                  help="Writes the exact start of benchmark time in given file")
parser.add_option("--dragonet-cost-function", action="store", type="string", dest="DRAGONET_COST_FUNCTION",
                  help="Cost function to be used by dragonet for starting stack. (balance, static, priority) ")
#
#
parser.add_option("-q", "--servercores", action="store", type="int", dest="SERVER_CORES",
                  help="Number of cores to use in server")
parser.add_option("--hwqueues", action="store", type="int", dest="HWQUEUES",
                  help="Number of hardware queues to be used (for Dragonet stack)")

parser.add_option("-Q", "--clientcores", action="store", type="int", dest="CLIENT_CORES",
                  help="Number of cores to use in client")

parser.add_option("--clientPortShift", action="store", type="int", dest="CLIENT_PORTS_SHIFT",
                  help="Shift in initial port number to be used by client")


parser.add_option("--serverInstances", action="store", type="int", dest="SERVERS_INSTANCES",
                  help="Number of server instances")
parser.add_option("--serverCoreShift", action="store", type="int", dest="SERVER_CORESHIFT",
                  help="Server coreshift for NUMA effect.  Core allocation will start from this value (default 2)")

parser.add_option("--spClients", action="store", type="int", dest="SPECIAL_CLIENTS_COUNT",
                  help="Number of clients to be used as 'special' clients")

parser.add_option("--totalClients", action="store", type="int", dest="TOTAL_CLIENTS",
                  help="Number total clients to simulate from given clients (0 for whatever are on commandline -C)")

parser.add_option("--concurrency", action="store", type="int", dest="CONCURRENCY",
                  help="Number concurrencies per client thread")


parser.add_option("-e", "--onload", action="store_true", dest="SERVER_ONLOAD_ENABLED",
                  help="Solarflare openonload library will be loaded for Linux apps")
parser.add_option("-E", "--onloadLatency", action="store_true", dest="SERVER_ONLOAD_LATENCY",
                  help="Solarflare openonload library with latency profile will be loaded for Linux apps")

parser.add_option("-c", "--echoServer", action="store", type="string", dest="ECHO_SERVER",
                  help="Which Echo server to use")



parser.add_option("-P", "--packet", action="store", type="int", dest="PKT_SIZE",
                  help="Size of outgoing packets")

parser.add_option("-n", "--note", action="store", type="string", dest="NOTE",
                  help="Add arbitrary text as a note to be stored in the JSON data file "
                  "(under the NOTE key in the metadata object).")
parser.add_option("-r", "--rcfile", action="store", type="string", dest="RCFILE",
                  help="Load configuration data from RCFILE (default ~/.netperf-wrapperrc).")
parser.add_option("-x", "--extended-metadata", action="store_true", dest="EXTENDED_METADATA",
                  help="Collect extended metadata and store it with the data file. "
                  "May include details of your machine you don't want to distribute; see man page.")
parser.add_option("--remote-metadata", action="append", type="string", dest="REMOTE_METADATA",
                  metavar="HOSTNAME",
                  help="Collect extended metadata from a remote host. HOSTNAME is passed "
                  "verbatim to ssh, so can include hosts specified in ~/.ssh/config. This "
                  "option can be specified multiple times. Note that gathering the data can "
                  "take some time, since it involves executing several remote commands.")
parser.add_option("--gui", action="store_true", dest="GUI",
                  help="Run the netperf-wrapper GUI. All other options are used as defaults "
                  "in the GUI, but can be changed once it is running.")


test_group = optparse.OptionGroup(parser, "Test configuration",
                                  "These options affect the behaviour of the test being run "
                                  "and have no effect when parsing input files.")
test_group.add_option("-H", "--host", action="append", type="string", dest="HOSTS", metavar='HOST',
                  help="Host to connect to for tests. For tests that support it, multiple hosts "
                  "can be specified by supplying this option multiple times. Hosts can also be "
                  "specified as unqualified arguments; this parameter guarantees that the "
                  "argument be interpreted as a host name (rather than being subject to "
                  "auto-detection between input files, hostnames and test names).")
test_group.add_option("-T", "--target", action="store", type="string", dest="TARGET",
                  help="Target IP to use in clients for generating traffic"),
test_group.add_option("-C", "--client", action="append", type="string", dest="CLIENTS",
                  help="Host to connect to for for running the clients. Multiple clients "
                  "can be specified by supplying this option multiple times.")
test_group.add_option("-l", "--length", action="store", type="int", dest="LENGTH",
                  help="Base test length (some tests may add some time to this).")
test_group.add_option("-s", "--step-size", action="store", type="float", dest="STEP_SIZE",
                  help="Measurement data point step size.")
test_group.add_option("-I", "--iterations", action="store", type="int", dest="ITERATIONS",
                  help="How many iterations to run")
test_group.add_option("-d", "--delay", action="store", type="int", dest="DELAY",
                  help="Number of seconds to delay parts of test (such as bandwidth "
                  "loaders).")
test_group.add_option("-4", "--ipv4", action="store_const", const=4, dest="IP_VERSION",
                  help="Use IPv4 for tests (some tests may ignore this).")
test_group.add_option("-6", "--ipv6", action="store_const", const=6, dest="IP_VERSION",
                  help="Use IPv6 for tests (some tests may ignore this).")
test_group.add_option("--socket-timeout", action="store", type=int, dest="SOCKET_TIMEOUT",
                  help="Socket timeout (in seconds) used for UDP delay measurement, to prevent "
                  "stalls on packet loss. Only enabled if the installed netperf version is "
                  "detected to support this (requires SVN version of netperf). "
                  "Default value: %d seconds. Set to 0 to disable." % DEFAULT_SETTINGS['SOCKET_TIMEOUT'])
parser.add_option_group(test_group)

plot_group = optparse.OptionGroup(parser, "Plot configuration",
                                  "These options are used to configure the appearance of "
                                  "plot output and only make sense combined with -f plot.")

plot_group.add_option("-z", "--zero-y", action="store_true", dest="ZERO_Y",
                  help="Always start y axis of plot at zero, instead of auto-scaling the "
                  "axis (also disables log scales). Auto-scaling is still enabled for the "
                  "upper bound.")
plot_group.add_option("--disable-log", action="store_false", dest="LOG_SCALE",
                  help="Disable log scales on plots.")
plot_group.add_option("--scale-data", action="append", type="string", dest="SCALE_DATA",
                  help="Additional data files to consider when scaling the plot axes "
                  "(for plotting several plots with identical axes). Note, this displays "
                  "only the first data set, but with axis scaling taking into account the "
                  "additional data sets. Can be supplied multiple times; see also --scale-mode.")
plot_group.add_option("-S", "--scale-mode", action="store_true", dest="SCALE_MODE",
                  help="Treat file names (except for the first one) passed as unqualified "
                  "arguments as if passed as --scale-data (default as if passed as --input).")
plot_group.add_option("--no-annotation", action="store_false", dest="ANNOTATE",
                  help="Exclude annotation with hostnames, time and test length from plots.")
plot_group.add_option("--use-tcp", action="store_true", dest="USE_TCP",
                  help="use TCP protocol instead of UDP for benchmarking")
plot_group.add_option("--tcp", action="store_true", dest="USE_TCP",
                  help="use TCP protocol instead of UDP for benchmarking")
plot_group.add_option("--udp", action="store_false", dest="USE_TCP",
                  help="use UDP for benchmarking (default)")
plot_group.add_option("--no-legend", action="store_false", dest="PRINT_LEGEND",
                  help="Exclude legend from plots.")
plot_group.add_option("--no-title", action="store_false", dest="PRINT_TITLE",
                  help="Exclude title from plots.")
parser.add_option_group(plot_group)


misc_group = optparse.OptionGroup(parser, "Misc and debugging options")
misc_group.add_option("-L", "--log-file", action="store", type="string", dest="LOG_FILE",
                  help="Write debug log (test program output) to log file.")
misc_group.add_option('--list-tests', action='store_true', dest="LIST_TESTS",
                  help="List available tests and exit.")
misc_group.add_option('--list-plots', action='store_true', dest="LIST_PLOTS",
                  help="List available plots for selected test and exit.")
misc_group.add_option("-V", "--version", action="callback", callback=version,
                  help="Show netperf-wrapper version information and exit.")
parser.add_option_group(misc_group)

class Settings(optparse.Values, object):

    def load_test_or_host(self, test_name):
        filename = os.path.join(TEST_PATH, test_name + ".conf")

        if not os.path.exists(filename):
            # Test not found, assume it's a hostname
            self.HOSTS.append(test_name)
        elif self.NAME is not None and self.NAME != test_name:
            raise RuntimeError("Multiple test names specified.")
        else:
            self.NAME = test_name

    def load_rcfile(self):
        if os.path.exists(self.RCFILE):

            config = RawConfigParser()
            config.optionxform = lambda x: x.upper()
            config.read(self.RCFILE)

            items = []

            if config.has_section('global'):
                items.extend(config.items('global'))
            if self.NAME is not None and config.has_section(self.NAME):
                items.extend(config.items(self.NAME))

            for k,v in items:
                if k in CONFIG_TYPES and getattr(self,k) == DEFAULT_SETTINGS[k]:
                    if CONFIG_TYPES[k] == 'str':
                        setattr(self, k, v)
                    elif CONFIG_TYPES[k] == 'int':
                        setattr(self, k, int(v))
                    elif CONFIG_TYPES[k] == 'float':
                        setattr(self, k, float(v))
                    elif CONFIG_TYPES[k] == 'list':
                        setattr(self, k, [i.strip() for i in v.split(",")])
                    elif CONFIG_TYPES[k] == 'bool':
                        if v.lower() in ('1', 'yes', 'true', 'on'):
                            setattr(self, k, True)
                        elif v.lower() in ('0', 'no', 'false', 'off'):
                            setattr(self, k, False)
                        else:
                            raise ValueError("Not a boolean: %s" % v)

    def load_test(self, test_name=None, informational=False):
        if test_name is not None:
            self.NAME=test_name
        if self.NAME is None:
            raise RuntimeError("Missing test name.")
        if self.HOSTS:
            self.HOST = self.HOSTS[0]

        if not self.INPUT and not self.GUI:
            self.lookup_hosts()

        test_env = TestEnvironment(self.__dict__, informational)
        filename = os.path.join(TEST_PATH, self.NAME + ".conf")
        s = test_env.execute(filename)

        for k,v in list(s.items()):
            if k == k.upper():
                setattr(self, k, v)

        if 'DEFAULTS' in s:
            for k,v in list(s['DEFAULTS'].items()):
                if not hasattr(self, k):
                    setattr(self, k, v)

        if not 'TOTAL_LENGTH' in s:
            self.TOTAL_LENGTH = self.LENGTH + self.DELAY

    def lookup_hosts(self):
        """If no explicit IP version is set, do a hostname lookup and try to"""
        version = 4
        for h in self.HOSTS:
            try:
                hostname = util.lookup_host(h)
                if hostname[0] == socket.AF_INET6:
                    version = 6
            except socket.gaierror as e:
                raise RuntimeError("Hostname lookup failed for host %s: %s" % (h,e))

        if self.IP_VERSION is None:
            self.IP_VERSION = version

    def __setattr__(self, k, v):
        if k in DICT_SETTINGS and isinstance(v, list):
            v = OrderedDict(v)

        object.__setattr__(self, k, v)

    def update(self, values):
        for k,v in list(values.items()):
            setattr(self, k, v)

    def items(self):
        return self.__dict__.items()

    def copy(self):
        return Settings(self)

settings = Settings(DEFAULT_SETTINGS)

def load_gui(settings):
    from netperf_wrapper import gui
    gui.run_gui(settings) # does not return

def load():
    global LOGFILE_HANDLE
    (dummy,args) = parser.parse_args(values=settings)

    if settings.LOG_FILE:
        LOGFILE_HANDLE = open(settings.LOG_FILE, "a")
    writeLog("logfile opened")

    # If run with no args and no controlling TTY, launch the GUI by default
    if not sys.stdin.isatty() and not sys.stdout.isatty() and not sys.stderr.isatty() \
        and len(sys.argv) < 2:
        settings.GUI = True

    if hasattr(settings, 'LIST_TESTS') and settings.LIST_TESTS:
        list_tests()

    if hasattr(settings, 'PLOT'):
        settings.FORMAT = 'plot'

    for a in args:
        if os.path.exists(a):
            if settings.SCALE_MODE and settings.INPUT:
                settings.SCALE_DATA.append(a)
            else:
                settings.INPUT.append(a)
        else:
            settings.load_test_or_host(a)

    settings.load_rcfile()

    if settings.INPUT:
        results = []
        test_name = None
        for filename in settings.INPUT:
            r = ResultSet.load_file(filename)
            if test_name is not None and test_name != r.meta("NAME"):
                raise RuntimeError("Result sets must be from same test (found %s/%s)" % (test_name, r.meta("NAME")))
            test_name = r.meta("NAME")
            #print "Appending results for test %s" % (test_name)
            results.append(r)

        if settings.GUI:
            load_gui(settings)

        settings.update(results[0].meta())

        settings.load_test()
    elif settings.GUI:
        load_gui(settings)
    else:
        settings.RESULT_LOCATION_BASE2 = '${HOME}/tempResult%s' % (timeStamped(t=settings.TIME).strip().replace(' ', ''))
        settings.SERVERS = settings.HOSTS

        if settings.TOTAL_CLIENTS > 0 :
            if len(settings.CLIENTS) >= settings.TOTAL_CLIENTS:
                settings.CLIENTS = settings.CLIENTS[:settings.TOTAL_CLIENTS]
            else:
                client_count = len(settings.CLIENTS)
                for i in range(0, (settings.TOTAL_CLIENTS - client_count)) :
                    settings.CLIENTS.append(settings.CLIENTS[i % client_count])
        else :
            settings.TOTAL_CLIENTS = len(settings.CLIENTS)

        print "Final client list is %s" % (settings.CLIENTS)
        for m in settings.SERVERS:
            settings.MINFO_SERVER[m] = record_machine_metadata(m, settings.TARGET)
            settings.SERVERS_IF[m] = settings.MINFO_SERVER[m]["EGRESS_INFO"]["iface"]
            settings.SERVERS_DRV[m] = settings.MINFO_SERVER[m]["EGRESS_INFO"]["driver"]
            settings.SERVERS_CORECOUNT[m] = int(settings.MINFO_SERVER[m]['CORECOUNT'])

        for m in settings.CLIENTS:
            settings.MINFO_CLIENT[m] = record_machine_metadata(m, settings.TARGET)
            settings.CLIENTS_IF[m] = settings.MINFO_CLIENT[m]["EGRESS_INFO"]["iface"]
            settings.CLIENTS_CORECOUNT[m] = int(settings.MINFO_CLIENT[m]['CORECOUNT'])

        if settings.SERVER_ONLOAD_LATENCY:
            settings.SERVER_ONLOAD_CMDLINE = "onload --profile=latency --preload=/usr/lib64/libonload.so"
        elif settings.SERVER_ONLOAD_ENABLED :
            settings.SERVER_ONLOAD_CMDLINE = "onload  --preload=/usr/lib64/libonload.so"
        else :
            settings.SERVER_ONLOAD_CMDLINE = ""

        if len(settings.SERVERS) > 1 :
            print "ERROR: More than one server specified.  This is not supporte d yet"
            sys.exit(1)

        srvname = settings.SERVERS[0]
        for i in range(0, settings.SERVERS_INSTANCES):
            settings.RUN_CONF[0] = {}
            settings.RUN_CONF[0]['SERVER'] = {}

        settings.BRUST_SIZE = settings.CONCURRENCY
        settings.TCONCURRENCY =  (settings.CONCURRENCY * settings.CLIENT_CORES
                        * (len(settings.CLIENTS) - settings.SPECIAL_CLIENTS_COUNT))


        settings.load_test()
        results = [ResultSet(NAME=settings.NAME,
                            HOST=settings.HOST,
                            HOSTS=settings.HOSTS,
                            SERVERS=settings.SERVERS,
                            MINFO_SERVER=settings.MINFO_SERVER,
                            MINFO_CLIENT=settings.MINFO_CLIENT,
                            RESULT_LOCATION_BASE2=settings.RESULT_LOCATION_BASE2,
                            TOOLS_LOCATION=settings.TOOLS_LOCATION,
                            PKT_SIZE=settings.PKT_SIZE,
                            TARGET=settings.TARGET,
                            USE_TCP=settings.USE_TCP,
                            SERVER_CORES=settings.SERVER_CORES,
                            HWQUEUES=settings.HWQUEUES,
                            SPECIAL_CLIENTS_COUNT = settings.SPECIAL_CLIENTS_COUNT,
                            SPECIAL_SERVERS_COUNT = settings.SPECIAL_SERVERS_COUNT,
                            BRUST_SIZE=settings.BRUST_SIZE,
                            CONCURRENCY = settings.CONCURRENCY,
                            TCONCURRENCY = settings.TCONCURRENCY,
                            CLIENT_CORES=settings.CLIENT_CORES,
                            CLIENT_PORTS_SHIFT=settings.CLIENT_PORTS_SHIFT,
                            CLIENTS=settings.CLIENTS,
                            TOTAL_CLIENTS=settings.TOTAL_CLIENTS,
                            CLIENTS_IF=settings.CLIENTS_IF,
                            CLIENTS_CORECOUNT=settings.CLIENTS_CORECOUNT,
                            SERVERS_IF=settings.SERVERS_IF,
                            SERVERS_DRV=settings.SERVERS_DRV,
                            SERVERS_CORECOUNT=settings.SERVERS_CORECOUNT,
                            SERVERS_INSTANCES=settings.SERVERS_INSTANCES,
                            SERVER_CORESHIFT=settings.SERVER_CORESHIFT,
                            TIME=settings.TIME,
                            LOCAL_HOST=settings.LOCAL_HOST,
                            TITLE=settings.TITLE,
                            NOTE=settings.NOTE,
                            SERVER_ONLOAD_LATENCY=settings.SERVER_ONLOAD_LATENCY,
                            DRAGONET_COST_FUNCTION=settings.DRAGONET_COST_FUNCTION,
                            SERVER_ONLOAD_ENABLED=settings.SERVER_ONLOAD_ENABLED,
                            SERVER_ONLOAD_CMDLINE=settings.SERVER_ONLOAD_CMDLINE,
                            LENGTH=settings.LENGTH,
                            TOTAL_LENGTH=settings.TOTAL_LENGTH,
                            DELAY=settings.DELAY,
                            STEP_SIZE=settings.STEP_SIZE,
                            ITERATIONS=settings.ITERATIONS,
                            ECHO_SERVER=settings.ECHO_SERVER,
                            NETPERF_WRAPPER_VERSION=VERSION,
                            IP_VERSION=settings.IP_VERSION)]
        if settings.EXTENDED_METADATA:
            record_extended_metadata(results[0], settings.REMOTE_METADATA)

    if settings.SCALE_DATA:
        scale_data = []
        for filename in settings.SCALE_DATA:
            if filename == settings.INPUT:
                # Do not load input file twice - makes it easier to select a set
                # of files for plot scaling and supply each one to -i without
                # having to change the other command line options each time.
                continue
            r = ResultSet.load_file(filename)
            if r.meta("NAME") != settings.NAME:
                raise RuntimeError("Setting name mismatch between test "
                                   "data and scale file %s" % filename)
            scale_data.append(r)
        settings.SCALE_DATA = scale_data


    if hasattr(settings, 'LIST_PLOTS') and settings.LIST_PLOTS:
        list_plots()

    if not settings.HOSTS and not results[0]:
        raise RuntimeError("Must specify host (-H option).")

    print "Running experiment for time of %d" % (settings.TOTAL_LENGTH)
    return settings, results

def list_tests():
    tests = sorted([os.path.splitext(i)[0] for i in os.listdir(TEST_PATH) if i.endswith('.conf')])
    sys.stderr.write('Available tests:\n')
    max_len = max([len(t) for t in tests])
    for t in tests:
        settings.update(DEFAULT_SETTINGS)
        settings.load_test(t, informational=True)
        desc = settings.DESCRIPTION.replace("\n", "\n"+" "*(max_len+6))
        sys.stderr.write(("  %-"+str(max_len)+"s :  %s\n") % (t, desc))
    sys.exit(0)

def list_plots():
    plots = list(settings.PLOTS.keys())
    if not plots:
        sys.stderr.write("No plots available for test '%s'.\n" % settings.NAME)
        sys.exit(0)

    sys.stderr.write("Available plots for test '%s':\n" % settings.NAME)
    max_len = str(max([len(p) for p in plots]))
    for p in plots:
        sys.stderr.write(("  %-"+max_len+"s :  %s\n") % (p, settings.PLOTS[p]['description']))
    sys.exit(0)
