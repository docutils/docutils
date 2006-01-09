#!/bin/sh

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.

# Usage: dbackport <revision>
#
# Backports <revision> from the trunk to the maintenance branch.

set -e

r="$1"

if test ! "$r"; then
    echo Usage: "`basename "$0"` <revision>"
    exit 1
fi

if test ! "$DOCUTILS_MAINT_BRANCH"; then
    echo '$DOCUTILS_MAINT_BRANCH must point to the directory of the'
    echo 'maintenance branch.'
    exit 1
fi

cd "$DOCUTILS_MAINT_BRANCH"
svn revert . -R
svn up
svn merge -r"$[$r-1]:$r" ../../trunk/docutils .
svn diff
# Python 2.4 is faster and outputs unified diffs for the functional
# tests, so it comes first.
nice python2.4 test/alltests.py
nice python2.1 test/alltests.py
nice python2.2 test/alltests.py
nice python2.3 test/alltests.py
svn ci -m "merged r$r to maintenance branch"
