#!/bin/bash

#
# Change to DPDK directory ( <this-script's-dir>/.. ), and export it as RTE_SDK
#
set -e
export RTE_SDK=$PWD
echo "------------------------------------------------------------------------------"
echo " RTE_SDK exported as $RTE_SDK"
echo "------------------------------------------------------------------------------"

if [ -z "$1" ]; then
    echo "ERROR:: No interface name given."
    echo "Please provide interface name what should be moved to dpdk"
    exit 1
    #echo "Using ${HOME}/minfo/used_if.log to determine which NIC to bind"
    #IFNAME=$(cat ${HOME}/minfo/used_if.log | grep "intel" | grep "switch" | cut -d',' -f1 | head -n1)
fi
IFNAME=${1}

echo "Using ${IFNAME} to bind.  Here are the current details"

ifconfig ${IFNAME}

echo "Bringing down the interface so that we can bind to UIO"
sudo ifconfig ${IFNAME} down

echo "Finding out the PCI address of the NIC for binding"
PCI_PATH=$(sudo ${RTE_SDK}/tools/dpdk_nic_bind.py --status | grep  ${IFNAME} | cut -d' ' -f1)

echo "Found PCI address ${PCI_PATH} for ${IFNAME} "

echo "Binding ${PCI_PATH} (${IFNAME}) with UIO now "
sudo ./tools/dpdk_nic_bind.py --status

sudo ${RTE_SDK}/tools/dpdk_nic_bind.py -b igb_uio $PCI_PATH && echo "OK"

echo "Showing current settings for verification"
sudo ${RTE_SDK}/tools/dpdk_nic_bind.py --status | grep  $PCI_PATH

