# Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
#
# Released under a dual BSD 3-clause/GPL 2 license. When using or
# redistributing this file, you may do so under either license.
#
# See LICENCE.Dragonet for details.

#!/bin/bash

SCRIPTDIR="./scripts/pravin/"
ulimit -n 65000
# set the wait time after running the command
waitTime=${1}
shift

# set the directory from where the command will be run
dirToRun=${1}
shift

# set the output file to record the output of command
outputfile=${1}
shift

# the commant itself command
cmd=${1}
shift

# changing the dir
cd ${dirToRun}

# actually executing the command
nohup sudo ${cmd} "$@" > ${outputfile} 2>&1 < /dev/null &
echo "$!" > server.pid

# TODO: maybe I want to record the pid of command

# sleep for specified time
sleep ${waitTime}

# generate the echo statement stating the you are done with the work
echo "Done with lunching command [${cmd}] in background"
exit 0

