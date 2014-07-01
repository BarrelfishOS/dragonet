#!/bin/bash
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

# TODO: maybe I want to record the pid of command

# sleep for specified time
sleep ${waitTime}

# generate the echo statement stating the you are done with the work
echo "Done with lunching command [${cmd}] in background"
exit 0

