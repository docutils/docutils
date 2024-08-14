#!/bin/sh

# Author: Lea Wiemann
# Contact: LeWiemann@gmail.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.

# This was used to share files in tmp directory on sourceforge.net
# adding a timestamp to the filename.
 
set -e

date="`date --utc --iso-8601`"

if test -z "$1" -o "$1" = -h -o "$1" = --help; then
    uploaddocutils="`basename "$0"`"
    echo This script uploads files to http://docutils.sf.net/tmp/ using scp.
    echo It inserts the current date in the file names and prints the
    echo resulting URLs.
    echo
    echo Usage: "$uploaddocutils" files ...
    echo
    echo For example, \""$uploaddocutils" test.tar.gz\" will upload
    echo test.tar.gz to "<http://docutils.sf.net/tmp/test-$date.tar.gz>".
    exit 1
fi

urllist=
while test -n "$1"; do
    localfile="$1"
    remotefile="`basename $1`"
    # Add date in front of first period or at the end of the file name
    # if there is no period.
    remotefile="`echo "$remotefile" | sed 's/\(\.\|$\)/-'"$date"'\1/'`"
    scp "$localfile" "shell.sourceforge.net:/home/groups/d/do/docutils/htdocs/tmp/$remotefile"
    urllist="$urllist
http://docutils.sf.net/tmp/$remotefile"
    shift
done
echo "$urllist"
