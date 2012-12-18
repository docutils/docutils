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

set -o errexit

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
run tar xzf ../$tarball

cd docutils-"$docutils_ver"

echo "Deleting and installing Docutils on Python $py_ver."
echo "Press enter."
read

if [ -n "`which python$py_ver | grep local`" ] ; then
    usr_local="local/"
else
    usr_local=""
fi
echo "docutils installation found under: /usr/$usr_local"
site_packages="/usr/${usr_local}lib/python$py_ver/site-packages"
if test ! -d "$site_packages"; then
    echo "Info: \"$site_packages\" does not exist."
fi
if test -e "$site_packages/docutils-test"; then
    echo "Error: \"$site_packages/docutils-test\" exists."
    echo "removing left over from previous release (sudo). Ctrl-C to abort."
    read
    sudo rm -rf $site_packages/docutils-test
fi
echo "remove docutils installation (sudo) build and install. Ctrl-C to abort"
read
sudo rm -rfv ${site-packages}/docutils
echo "TODO for python3 rm local build, but building takes a long time then "
python$py_ver setup.py build
sudo python$py_ver setup.py install
echo
echo "Copying the test suite to the site-packages directory of Python $py_ver (sudo)."
echo "TODO for python3 copy test3. Ennter.to continue."
read
sudo cp -rv test "$site_packages/docutils-test"

# BUG test-dependecies.py
# * breaks on record.txt access if not run as root
# * fails missing dependencies to files in docutils/docs.

echo "run alltests.py"
# run locally so we do not require sudo to create 
# site_packages/docutils-test/alltests.out

# copy to a directory and execute there to avoid usage
# of local docutils source.
test_run_dir=tmp-docutils-test
cp -r test $test_run_dir
mkdir -p $test_run_dir
cd $test_run_dir
python$py_ver -u test/alltests.py

echo "remove test directory Ctrl C to abort"
read
cd $START_DIR
rm -rfv $test_dir

