#!/bin/bash
set -x
set -e

SCRIPTDIR="./scripts/pravin/"
${SCRIPTDIR}/deployPrepare.sh

APPNAME="stack-tap"
sudo ./dist/build/${APPNAME}/${APPNAME} $@

