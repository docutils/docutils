#!/bin/bash

# Author: Lea Wiemann
# Contact: LeWiemann@gmail.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.

set -e

if test -z "$1" -o "$1" == "-h" -o "$1" == "--help"; then
    echo Usage: "`basename "$0"` [-c] <revision> [[-c] <revision> ...]"
    echo
    echo 'Any revision number that is prepended with "-c" is only checked in;'
    echo 'no actual merging is done.  (Useful for resuming aborted backports,'
    echo 'for example after manually resolving conflicts.)'
    exit 1
fi

while test -n "$1"; do
    commit_only=
    if test "$1" == "-c"; then
        commit_only=1
        if test -z "$2"; then
            echo 'Error: Expected revision number after "-c".'
            exit 1
        fi
        shift
    fi
    r="$1"
    if test ! "$DOCUTILS_MAINT_BRANCH"; then
        echo '$DOCUTILS_MAINT_BRANCH must point to the directory of the'
        echo 'maintenance branch.'
        exit 1
    fi

    cd "$DOCUTILS_MAINT_BRANCH"
    if test -z "$commit_only"; then
        svn revert . -R
        svn up
        svn merge -r"$[$r-1]:$r" ../../trunk/docutils .
    fi
    svn diff
    if test "`svn st docutils test *.py -q`"; then
        # Some code has changed.
        # Python 2.5 is faster and outputs unified diffs for the functional
        # tests, so it comes first.
        nice python2.5 -u test/alltests.py
        nice python2.1 -u test/alltests.py
        nice python2.2 -u test/alltests.py
        nice python2.3 -u test/alltests.py
        nice python2.4 -u test/alltests.py
    fi
    echo
    echo Press enter to commit or Ctrl+C to abort.
    read
    svn ci -m "--- MERGE: merged r$r to maintenance branch; original log message:
`svn pg svn:log --revprop -r "$r"`"
    shift
done
