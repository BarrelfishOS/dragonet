#!/bin/bash

set -x
set -e

sudo tcpdump -n -vv -XX -i eth3 -U -s 1500 'ether host 00:0f:53:07:51:49'


