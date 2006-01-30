#!/bin/sh

# Parse options:
#   -h <path>     High priority run-time path

while getopts h: c
do
   case $c in
    h)     PATH_H=$OPTARG
   esac
done

# Use relative path so script will work in different people's repositories

PATH_REQ=/_TOOLS_/arch/bin:/_TOOLS_/plat/perl-5.8.0-tx32-64int-latest/bin:/bin:/usr/bin
PATH=$PATH_H:.:../../../../bin:../../../bin:../../../../src:../../../src:/bin:/_TOOLS_/dist/gnu-make-3.80/i686-pc-linux2.4/bin/:/_TOOLS_/dist/moto-gress-/latest/all/bin:$PATH_REQ
export PATH

cd `dirname $0`
