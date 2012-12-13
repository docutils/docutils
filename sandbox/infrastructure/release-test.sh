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

py_ver=$1

docutils_ver=$2

echo "Testing the release tarball: docutils-${docutils_ver} under python$py_ver."

echo "BUG: must be run as root/sudo to be able to remove/install into site-packages."

test_dir=tarball_test
rm -rfv $test_dir
mkdir -p $test_dir || exit 1
cd $test_dir || exit 1
tar xzvf ../docutils-${docutils_ver}.tar.gz || exit 1

cd docutils-"$docutils_ver" || exit 1

echo "Deleting and installing Docutils on Python $py_ver."
echo "Press enter."
read
site_packages="/usr/local/lib/python$py_ver/site-packages"
echo "BUG prefers /usr/local too /usr"
if test ! -d "$site_packages"; then
    site_packages="/usr/lib/python$py_ver/site-packages"
fi
if test ! -d "$site_packages"; then
    echo "Error: \"$site_packages\" does not exist."
    exit 1
fi
if test -e "$site_packages/docutils-test"; then
    echo "Error: \"$site_packages/docutils-test\" exists."
    echo "removing left over from previous release. Ctrl-C to abort."
    read
    rm -rf $site_packages/docutils-test
fi
rm -rfv /usr/{local,}lib/python$py_ver/site-packages/{docutils'"$extras"'}
echo "TODO for python3 rm local build, but building takes a long time then "
python$py_ver setup.py install
echo
echo "Copying the test suite to the site-packages directory of Python $py_ver."
echo "TODO for python3 copy test3"
echo "Press enter."
read
cp -rv test "$site_packages/docutils-test"

if test ! -d "$site_packages"; then
    echo "Error: \"$site_packages\" does not exist."
    exit 1
fi
# BUG test-dependecies.py
# * breaks on record.txt access if not run as root
# * fails missing dependecies to png.
python$py_ver -u $site_packages/docutils-test/alltests.py

