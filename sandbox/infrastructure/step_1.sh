#!/bin/bash 

# set version number and description
    
# Author: engelbert gruber (taken from Lea Wiemann's release.sh)
# Contact: grubert@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.
    
# USAGE see: docutils/docs/dev/release.txt
    
# must be run from docutils trunk/docutils, 
# because HISTORY and RELEASE_NOTES.txt are modified.
    
set -e

echo "Change version number and description"
if [ -z "$1" -o -z "$2" ] ; then
    echo "USAGE"
    echo "    step_1.sh  new-description new-version-number"
    echo ""
    echo "Description might be: repository prerelease release"
    echo ""
    echo "Version number: major.minor.micro"
    echo "    micro is for bug-fix releases and left out on new minor-numbers"
    echo "    prereleases get '(a|b|rc)#' appended"
    echo ""
    echo "Samples"
    echo "    0.14 0.14a0 0.14rc1 0.20.1a0"
    exit 
fi 

# new description: repository prerelease release
new_desc=$1

echo -n 'Detecting current Docutils version... '
old_ver="`python -c 'import docutils; print docutils.__version__'`"

new_ver=$2

function set_ver()
{
    # Parameters: old_version new_version
    shopt -s extglob
    echo Determining list of files to be changed...
    # BUG ls lists directories but does not descend
    files="docutils/__init__.py setup.py `$svn ls test/functional/expected/ | sed 's|^|test/functional/expected/|'`"
    echo "Now I'll change the version number to $2 in the following files:"
    echo $files
    echo
    echo 'Press enter to proceed (or enter anything to skip)...'
    read
    if [ ! "$REPLY" ]; then
        echo 'Modifying files with ed...'
        old_ver_regex="`echo "$1" | sed 's/\./\\\\./g'`"
        # "ed" returns an error code if there has been no substitution, so
        # we temporarily deactivate exit-on-error.
        set +e
        for F in $files; do
            (echo ",s/$old_ver_regex/$2/g"; echo 'wq') | ed "$F"
        done
        set -e
    fi
    echo
    echo 'CAUTION: please look at the diffs carefully, for wrongly'
    echo ' replaced embedded numbers.'
#    checkin "set version number to $2" $files
}

# update __version_details__ string
(echo ",s/^__version_details__ = .*\$/__version_details__ = '${new_desc}'/";
    echo wq) | ed docutils/__init__.py 2> /dev/null
set_ver "$old_ver" "$new_ver"

