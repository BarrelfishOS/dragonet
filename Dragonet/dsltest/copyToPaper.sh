#!/bin/bash

if [ ! $# == 1 ]; then
    echo "USAGE: $0 <Paper Repository Path>"
    echo "Example: ${HOME}/hg/writing/dragonet/"
    exit 1
fi
PAPERREPO="${1}"


# PAPERREPO="${HOME}/hg/writing/dragonet/"

DIAGRAMSLOCATION="${PAPERREPO}/figs/"


function copyDiagram() {
    fname=$1
    cp "${fname}.dot" "${DIAGRAMSLOCATION}"
    cp "${fname}.pdf" "${DIAGRAMSLOCATION}"
}

set -x
set -e

copyDiagram "LPGpaper"
copyDiagram "PRGUnconfpaper"
copyDiagram "PRGpaper"
copyDiagram "Embeddedpaper"

echo "Done..."

