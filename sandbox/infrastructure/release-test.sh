#!/bin/bash

# testing of a release tarball extracted from release.sh
# because if this breaks release.sh exits
# and running tests again 2.4 to 2.7 automatic never worked for me
# and 3.x is not included

# Author: Lea Wiemann
# Contact: grubert@users.sourceforge.net
# Revision: $Revision: 7548 $
# Date: $Date: 2012-12-13 10:08:17 +0100 (Don, 13 Dez 2012) $
# Copyright: This script has been placed in the public domain.

USAGE="USAGE $0 python-version docutils-version"

py_ver=$1

docutils_ver=$2

if [  -z "`which python$py_ver`" ] ; then
    echo "ERROR python$py_ver not found."
    echo $USAGE
    exit 1
fi

tarball=docutils-${docutils_ver}.tar.gz

if [ ! -e $tarball ] ; then
    echo "ERROR $tarball not found."
    echo $USAGE
    exit 1
fi

function print_command()
{
    # Print "$@", quoting parameters containing spaces.
    echo -n $
    for param in "$@"; do
        echo "$param" | grep -Fq ' ' && echo -n " '$param'" || echo -n " $param"
    done
}

function run()
{
    # Print and run "$@".
    print_command "$@"
    echo
    "$@"
}

echo "Testing the release tarball: docutils-${docutils_ver} under python$py_ver."

echo "ATTENTION: some parts must be run as root/sudo to be able to remove/install into site-packages."

START_DIR=`pwd`

test_dir=tarball_test
run rm -rf $test_dir
run mkdir -p $test_dir
cd $test_dir

echo "Deleting and installing Docutils on Python $py_ver."
echo "Press enter."
read

docutils_install_dir=$(python$py_ver -c 'import docutils, os.path; print os.path.dirname(docutils.__file__)')

if [ -z "$docutils_install_dir" ] ; then
    echo "No docutils installation found"
else
    echo "docutils installation found: $docutils_install_dir"
    echo "remove docutils installation (sudo). Ctrl-C to abort"
    read
    sudo rm -rfv $docutils_install_dir
fi

echo "build and install (sudo). Ctrl-C to abort"
read
run tar xzf ../$tarball

cd docutils-"$docutils_ver"

python$py_ver setup.py build
sudo python$py_ver setup.py install
echo
echo "Remove dcoutils code directory from tarball_test to make sure it is not used.."
echo "TODO for python3."
read
rm -rf docutils 

# BUG test-dependecies.py
# * breaks on record.txt access if not run as root
# * fails missing dependencies to files in docutils/docs.

echo "run alltests.py"

python$py_ver -u test/alltests.py

echo "remove test directory Ctrl C to abort"
read
cd $START_DIR
rm -rfv $test_dir

