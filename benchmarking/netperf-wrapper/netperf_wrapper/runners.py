## -*- coding: utf-8 -*-
##
## runners.py
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

import threading, time, shlex, subprocess, re, time, sys, math, os, tempfile, signal

from datetime import datetime

from .settings import settings, Glob, writeLog
from collections import OrderedDict

import json as js
from .util import gzip_open

import metadata as md

# Controls pretty-printing of json dumps
JSON_INDENT=4
#JSON_INDENT=None


class MachineRunner(object):
    """Machine abstraction to capture machines involved in experiment"""
    def __init__(self, m_name, deployment_host,
            result_location, tools_location, is_server):
        self.tool_instances = OrderedDict()
        self.threads = OrderedDict()
        self.is_server = is_server
        self.postprocessors = []
        self.machine_metadata = None
        self.m_name = m_name
        self.deployment_host = deployment_host
        self.result = None
        self.result_location = result_location
        self.tools_location = '${HOME}/dragonet/benchmarking/netperf-wrapper/'
        self.tools_to_run = []
        self.temp_work_location = None
#        print "Setting up machine %s ==> %s" % (m_name, deployment_host)

#        if self.deployment_host and self.deployment_host != 'localhost':
#            self.tools_location = '${HOME}/dragonet/benchmarking/netperf-wrapper/'
#            self.command = "ssh %s 'cd %s ; %s'" % (self.deployment_host,
#                    self.tools_location, command)
#        else :
#            self.tools_location = '${HOME}/git/dragonet/benchmarking/netperf-wrapper/'
#            self.command = "bash -c 'cd %s ; %s'" % (self.tools_location, command)

    def _exec_cmd_blocking(self, cmd):
        if cmd == None or cmd == '' :
            return ""
        if self.deployment_host and self.deployment_host != 'localhost':
            cmd = "ssh %s '%s'" % (self.deployment_host, cmd)
            #cmd = "ssh -t %s '%s'" % (self.deployment_host, cmd)

        try:
            res = subprocess.check_output(cmd, universal_newlines=True, shell=True,
                    stderr=subprocess.STDOUT)
            return res.strip()
        except:
            raise


    def _create_work_location(self):
        """Create location where we can keep output files without any conflicts"""
        # Clear old results
        # FIXME: this can be risky if the user gave path to user directory
        #cmd = "rm --rf %s" % (self.result_location)

        # Making sure that parent directory is there
        cmd = "mkdir -p %s" % (self.result_location)
        self._exec_cmd_blocking(cmd)
#        cmd = "mktemp -d --tmpdir=%s %s_%sXXXXXX" % (self.result_location,
#                self.m_name.strip().replace(' ', ''),
#                self.deployment_host.strip().replace(' ', ''),
#                )
#        self.temp_work_location = self._exec_cmd_blocking(cmd)

    def read_machine_metadata(self):
        return self.machine_metadata

    def setup_machine(self):
        """Sets up machine to run the benchmark"""
        self._create_work_location()

class ProcessRunner(threading.Thread):
    """Default process runner for any process."""

    def __init__(self, machine, name, command, wait_for,
            init_cmd, kill_cmd, out_cmd, is_catastrophic,
            delay, *args, **kwargs
            ):
        threading.Thread.__init__(self)
        self.name = name
        self.delay = delay
        self.result = None
        self.killed = False
        self.machine_ref = machine
        self.wait_for = wait_for
        self.init_cmd = init_cmd
        self.kill_cmd = kill_cmd
        self.out_cmd = out_cmd
        self.is_catastrophic = is_catastrophic
        self.pid = None
        self.returncode = None
#        self.deployment_host = machine.deployment_host
#        self.result_location = machine.result_location
#        self.tools_location = '${HOME}/dragonet/benchmarking/netperf-wrapper/'
#        self.temp_work_location = None
        if self.machine_ref.deployment_host and self.machine_ref.deployment_host != 'localhost':
            self.command = "ssh %s 'cd %s ; %s'" % (self.machine_ref.deployment_host,
                    self.machine_ref.tools_location, command)
        else :
            self.command = "bash -c 'cd %s ; %s'" % (self.machine_ref.tools_location, command)

        self.logMsg ("The runner %s:  [%s]" % (self.machine_ref, self.command))
        self.args = shlex.split(self.command)

    def logMsg(self, msg):
        writeLog("%s: %s\n" % (self.name, msg))

    def fork(self):
        # doing initial setup
        if self.init_cmd:
            for cmd in self.init_cmd:
                self.logMsg ("INIT: Machine %s: Executing init command [%s]" % (
                        self.machine_ref, cmd))
                ans = self.machine_ref._exec_cmd_blocking(cmd)
                self.logMsg ("INIT: Machine %s: the response of executing init command is \n%s\n" %( self.machine_ref, ans))

        # Use named temporary files to avoid errors on double-delete when
        # running on Windows/cygwin.
        self.stdout = tempfile.NamedTemporaryFile(delete=False)
        self.stderr = tempfile.NamedTemporaryFile(delete=False)

        pid = os.fork()

        if pid == 0:
            os.dup2(self.stdout.fileno(), 1)
            os.dup2(self.stderr.fileno(), 2)
            self.stdout.close()
            self.stderr.close()

            time.sleep(self.delay)

            prog = self.args[0]
            os.execvp(prog, self.args)
        else:
            self.pid = pid


    def should_wait(self):
        return self.wait_for

    def kill_explicit(self):
        if self.kill_cmd:
            try:
                for cmd in self.kill_cmd:
                    ans = self.machine_ref._exec_cmd_blocking(cmd)
                    self.logMsg ("KILL_EXPLICIT: Machine %s: the response of executing kill command[%s] is \n%s\n" %( self.machine_ref, cmd, ans))
            except OSError:
                pass


    def kill(self):
        #print "T4rying to kill the process"
        if self.killed:
            return
        if self.pid is not None:
            try:
                self.kill_explicit()
                os.kill(self.pid, signal.SIGINT)
            except OSError:
                pass
        self.killed = True

    # helper function from subprocess module
    def _handle_exitstatus(self, sts, _WIFSIGNALED=os.WIFSIGNALED,
                           _WTERMSIG=os.WTERMSIG, _WIFEXITED=os.WIFEXITED,
                           _WEXITSTATUS=os.WEXITSTATUS):
        # This method is called (indirectly) by __del__, so it cannot
        # refer to anything outside of its local scope."""
        if _WIFSIGNALED(sts):
            self.returncode = -_WTERMSIG(sts)
        elif _WIFEXITED(sts):
            self.returncode = _WEXITSTATUS(sts)
        else:
            # Should never happen
            raise RuntimeError("Unknown child exit status!")

    def start(self):
        #print "Starting the process"
        self.fork()
        threading.Thread.start(self)

    def run(self):
        """Runs the configured job. If a delay is set, wait for that many
        seconds, then open the subprocess, wait for it to finish, and collect
        the last word of the output (whitespace-separated)."""


#        print "Waiting for process id %d " % (self.pid)
        pid, sts = os.waitpid(self.pid, 0)
        self.logMsg ("process id %d died/finished with status %s" % (pid, sts))
        self._handle_exitstatus(sts)

        self.stdout.seek(0)
        self.out = self.stdout.read().decode()
        try:
            # Close and remove the temporary file. This might fail, but we're going
            # to assume that is okay.
            filename = self.stdout.name
            self.stdout.close()
            os.unlink(filename)
        except OSError:
            pass

        self.stderr.seek(0)
        self.err = self.stderr.read().decode()
        try:
            filename = self.stderr.name
            self.stderr.close()
            os.unlink(filename)
        except OSError:
            pass

        if self.killed:
            return

        if self.returncode:
            if self.is_catastrophic:
                sys.stderr.write("Warning: Program exited non-zero.\nCommand: %s\n" % self.command)
                sys.stderr.write("Program output:\n")
                sys.stderr.write("  " + "\n  ".join(self.err.splitlines()) + "\n")
                sys.stderr.write("  " + "\n  ".join(self.out.splitlines()) + "\n")
                self.result = None
                raise RuntimeError("Warning: Program exited non-zero.\nCommand: %s\n" % self.command)
                return
            sys.stderr.write("NOTE: (Noncatastrophic) Program exited non-zero.\nCommand: %s\n" % self.command)
            #sys.exit(1) # FIXME: should I really call esit here?
#       else:
        self.result = self.parse(self.out)
        if not self.result:
            sys.stderr.write("Warning: Command produced no valid data.\n"
                             "Data series: %s\n"
                             "Runner: %s\n"
                             "Command: %s\n"
                             "Standard error output:\n" % (self.name, self.__class__.__name__, self.command)
                             )
            sys.stderr.write("  " + "\n  ".join(self.err.splitlines()) + "\n")

    def parse(self, output):
        """Default parser returns the last (whitespace-separated) word of
        output."""
        #print "Prasing the output..."
        if output == None or output == [] :
            return ""
        words = output.split()
        if len(words) == 0:
            return ""
        return words[-1].strip()

DefaultRunner = ProcessRunner

class DstatJsonRunner(ProcessRunner):
    """Runner dstat monitering """

    def parse(self, output):
        """Parses the final results and gives (key,value) pairs."""
        # Assuming that json output is in stdout
        result = js.loads(output)
        return result

class NetperfSumaryRunner(ProcessRunner):
    """Runner for netperf summary """

    def parse(self, output):
        """Parses the final results and gives (key,value) pairs."""

        result = {}
        cmd_output = {}
        lines = output.split("\n")
        for line in lines:
            if (len(line.strip()) == 0):
                continue
            parts = line.split("=")
            if (len(parts) == 2):
                cmd_output[parts[0]] = parts[1]
            else :
                self.logMsg("Could not parse line [%s] == [%s]" %
                        (line, str(parts)))
            #result.append(parts[0], parts[1])
        # FIXME: Verify that the numbers are proper and there are no errors
        bytes_sent = int(cmd_output["LOCAL_BYTES_XFERD"])
        bytes_recvd = int(cmd_output['REMOTE_BYTES_RECVD'])
        missing_bytes = bytes_sent - bytes_recvd
        if (missing_bytes != 0) :
        #    print ("Bytes lost = %d\n", missing_bytes)
            self.logMsg ("Bytes lost = %d\n" % missing_bytes)
        assert(cmd_output['THROUGHPUT_UNITS'] == "10^9bits/s")
        result['RESULT'] = cmd_output['REMOTE_RECV_THROUGHPUT']
        result['CMD_OUTPUT'] = cmd_output

        return result

class MemaslapSumaryRunner(ProcessRunner):
    """Runner for memaslap summary """

    def parse(self, output):
        """Parses the final results and gives (key,value) pairs."""
        result_keys = ["cmd_get", "cmd_set", "get_misses",
                        "written_bytes", "read_bytes", "object_bytes",
                        "packet_disorder", "packet_drop", "udp_timeout"
                      ]
        stats_keys = ["Min", "Max", "Avg", "Geo", "Std"]
        summary_keys = ["Ops", "TPS", "Net_rate"]
        cmd_keys = ["servers", "threads count", "concurrency",
                    "run time", "windows size", "set proportion",
                    "get proportion"
                   ]

        result = {}
        cmd_output = {}
        prefix = ""

        try:
            lines = output.split("\n")
            for line in lines:
                parts = line.split(":")
                key = parts[0].strip()
                #print "Key is [%s] and parts are [%s]" % (str(key), str(parts))
                if (key in cmd_keys):
                    cmd_output[key] = parts[1].strip()

                if (key in result_keys):
                    cmd_output[key] = float(parts[1].strip())
                elif (key.startswith('Get Statistics (')) :
                    prefix = "Get"
                elif (key.startswith('Total Statistics (')) :
                    prefix = "Total"
                elif (key in stats_keys):
                    nkey = "%s_%s" % (prefix, key)
                    cmd_output[nkey] = float(parts[1].strip())
                elif (key == "Run time") :
                    for i in range(1, (len(parts)-1)):
                        p2 = parts[i].strip().split(" ")
                        k2 = p2[1].strip()
                        if (k2 in summary_keys):
                            v2 = parts[i+1].strip().split(" ")[0]
                            cmd_output[k2] = v2.strip()

            if ("run time" in cmd_output.keys()) :
                cmd_output['RT'] = float(cmd_output["run time"][:-1])

            if ("TPS" in cmd_output.keys()) :
                result['RESULT'] = float(cmd_output["TPS"])
                cmd_output['ORESULT'] = float(cmd_output["TPS"])
            else :
                print "Run imcomplete as attribute 'TPS' is not present in output"
                print "Existing keys: %s" % (str(cmd_output.keys()))
                result['RESULT'] = 0
#            if (cmd_output['get_misses'] > (100 * cmd_output['RT']) ) :
#                result['RESULT'] = 0
#                cmd_output['ORESULT'] = 0
#            elif (cmd_output['get_misses'] > 0) :
            if (cmd_output['get_misses'] > 0) :
                result['RESULT'] = result['RESULT'] - (cmd_output['get_misses'] / cmd_output['RT'])
                cmd_output['ORESULT'] = 0

            if ('packet_drop' in cmd_output.keys() and cmd_output['packet_drop'] > 0 ):
                result['RESULT'] = result['RESULT'] - (cmd_output['packet_drop'] / cmd_output['RT'])
                cmd_output['ORESULT'] = 0

            if ('udp_timeout' in cmd_output.keys() and cmd_output['udp_timeout'] > 0):
                result['RESULT'] = result['RESULT'] - (cmd_output['udp_timeout'] / cmd_output['RT'])
                cmd_output['ORESULT'] = 0

        finally:
            e = sys.exc_info()
            cmd_output['EXCEPTION'] = str(e)

        if cmd_output['EXCEPTION'] == '(None, None, None)':
            cmd_output['ORESULT'] = 0

        cmd_output['RESULT'] = float(result['RESULT'])
        result['CMD_OUTPUT'] = cmd_output
        return result

class LatencyBmRunner(ProcessRunner):
    """Runner for ../latencyBench/latencybm"""

    def parse(self, output):
        """Parses the interim result lines and returns a list of (time,value)
        pairs."""

        result = {}

        vals = []
        lines = output.split("\n")
        for line in lines:
            if line.startswith("Interim: "):
                parts = line.split()
                vals.append(float(parts[2]))

        result['RESULT'] = vals
        return result


class NetperfDemoRunner(ProcessRunner):
    """Runner for netperf demo mode."""

    def parse(self, output):
        """Parses the interim result lines and returns a list of (time,value)
        pairs."""

        result = []
        lines = output.split("\n")
        for line in lines:
            if line.startswith("Interim"):
                parts = line.split()
                result.append([float(parts[9]), float(parts[2])])

        return result

class PingRunner(ProcessRunner):
    """Runner for ping/ping6 in timestamped (-D) mode."""

    pingline_regex = re.compile(r'^\[([0-9]+\.[0-9]+)\].*time=([0-9]+(?:\.[0-9]+)?) ms$')
    fpingline_regex = re.compile(r'^\[([0-9]+\.[0-9]+)\].*:.*, ([0-9]+(?:\.[0-9]+)?) ms \(.*\)$')

    def parse(self, output):
        result = []
        lines = output.split("\n")
        for line in lines:
            match = self.pingline_regex.match(line)
            if not match:
                match = self.fpingline_regex.match(line)
            if match:
                result.append([float(match.group(1)), float(match.group(2))])

        return result

class IperfCsvRunner(ProcessRunner):
    """Runner for iperf csv output (-y C), possibly with unix timestamp patch."""

    def parse(self, output):
        result = []
        lines = output.strip().split("\n")
        for line in lines[:-1]: # The last line is an average over the whole test
            parts = line.split(",")
            if len(parts) < 8:
                continue

            timestamp = parts[0]
            bandwidth = parts[8]

            # If iperf is patched to emit sub-second resolution unix timestamps,
            # there'll be a dot as the decimal marker; in this case, just parse
            # the time as a float. Otherwise, assume that iperf is unpatched
            # (and so emits YMDHMS timestamps).
            #
            # The patch for iperf (v2.0.5) is in the misc/ directory.
            if "." in timestamp:
                result.append([float(timestamp), float(bandwidth)])
            else:
                dt = datetime.strptime(timestamp, "%Y%m%d%H%M%S")
                result.append([time.mktime(dt.timetuple()), float(bandwidth)])

        return result

class TcRunner(ProcessRunner):
    """Runner for iterated `tc -s qdisc`. Expects iterations to be separated by
    '\n---\n and a timestamp to be present in the form 'Time: xxxxxx.xxx' (e.g.
    the output of `date '+Time: %s.%N'`)."""

    def __init__(tc_parameter, *args, **kwargs):
        ProcessRunner.__init__(self, *args, **kwargs)
        self.tc_parameter = tc_parameter

    time_re   = re.compile(r"^Time: (?P<timestamp>\d+\.\d+)", re.MULTILINE)
    split_re  = re.compile(r"^qdisc ", re.MULTILINE)
    qdisc_res = [
        re.compile(r"Sent (?P<sent_bytes>\d+) bytes (?P<sent_pkts>\d+) pkt "
                   r"\(dropped (?P<dropped>\d+), "
                   r"overlimits (?P<overlimits>\d+) "
                   r"requeues (?P<requeues>\d+)\)"),
        re.compile(r"backlog (?P<backlog_bytes>\d+)b "
                   r"(?P<backlog_pkts>\d+)p "
                   r"requeues (?P<backlog_requeues>\d+)"),
        re.compile(r"maxpacket (?P<maxpacket>\d+) "
                   r"drop_overlimit (?P<drop_overlimit>\d+) "
                   r"new_flow_count (?P<new_flow_count>\d+) "
                   r"ecn_mark (?P<ecn_mark>\d+)"),
        re.compile(r"new_flows_len (?P<new_flows_len>\d+) "
                   r"old_flows_len (?P<old_flows_len>\d+)")
        ]


    def parse(self, output):
        result = []
        parts = output.split("\n---\n")
        for part in parts:
            # Split out individual qdisc entries (in case there are more than
            # one). If so, discard the root qdisc and sum the rest.
            qdiscs = self.split_re.split(part)
            if len(qdiscs) > 2:
                part = "qdisc ".join([i for i in qdiscs if not 'root' in i])

            matches = {}
            timestamp = self.time_re.search(part)
            if timestamp is None:
                continue
            timestamp = float(timestamp.group('timestamp'))

            for r in self.qdisc_res:
                m = r.search(part)
                # Keep searching from the end of the last match until no more
                # matches are found; this should find all qdisc outputs in case
                # there are several qdiscs installed on the interface. The
                # values for the qdiscs are summed for the result (discarding
                # what should be the root qdisc as per above).
                while m is not None:
                    for k,v in list(m.groupdict().items()):
                        if not k in matches:
                            matches[k] = float(v)
                        else:
                            matches[k] += float(v)
                    m = r.search(part, m.end(0))
            key = self.tc_parameter
            if key in matches:
                result.append([timestamp, matches[key]])
            else:
                sys.stderr.write("Warning: Missing value for %s" % key)
        return result

class ComputingRunner(object):
    command = "Computed"
    def __init__(self, name, apply_to=None, *args, **kwargs):
        self.name = name
        if apply_to is None:
            self.keys = []
        else:
            self.keys = apply_to

        # These are use for debug logging
        self.returncode = 0
        self.out = ""
        self.err = ""

    # Emulate threading interface to fit into aggregator usage.
    def start(self):
        pass
    def join(self):
        pass

    def isAlive(self):
        return False

    def kill(self):
        pass

    def result(self, res):
        if not self.keys:
            return res

        new_res = []
        keys = Glob.expand_list(self.keys,res.series_names,[self.name])

        for r in res.zipped(keys):
            values = [v for v in r[1:] if v is not None]
            if not values:
                new_res.append(None)
            else:
                new_res.append(self.compute(values))

        res.add_result(self.name, new_res)
        return res

    def compute(self, values):
        """Compute the function on the values this runner should be applied to.

        Default implementation returns None."""
        return None

class AverageRunner(ComputingRunner):
    command = "Average (computed)"
    def compute(self,values):
        return math.fsum(values)/len(values)

class SmoothAverageRunner(ComputingRunner):
    command = "Smooth average (computed)"
    def __init__(self, smooth_steps=5, *args, **kwargs):
        ComputingRunner.__init__(self, *args, **kwargs)
        self._smooth_steps = smooth_steps
        self._avg_values = []

    def compute(self, values):
        self._avg_values.append(math.fsum(values)/len(values))
        while len(self._avg_values) > self._smooth_steps:
            self._avg_values.pop(0)
        return math.fsum(self._avg_values)/len(self._avg_values)

class SumRunner(ComputingRunner):
    command = "Sum (computed)"
    def compute(self,values):
        return math.fsum(values)
