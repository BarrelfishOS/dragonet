#!/bin/bash
ulimit -n 65000
cmd=${1}
shift
${cmd} "$@"
