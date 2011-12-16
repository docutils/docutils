#!/bin/bash

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This file has been placed in the public domain.

# Options:
#
# -f  Do not print feedback.

set -e

trap "echo; echo Exiting with error.; trap - 0; exit 1" 0 1 2 3 15

test "$1" == -f && printfeedback= || printfeedback=1

function feedback() {
    test "$printfeedback" && echo "$@" || true
}

function do_backup() {
    # If any of the tests fails, the script terminates silently.
    feedback "--- Backing up svn+ssh://$HOST$REMOTEDIR ---"
    feedback "Checking that all necessary variables are set."
    test -n "$BACKUPDIR"
    test -n "$HOST"
    test -n "$REMOTEDIR"
    if test ! -d "$BACKUPDIR"; then
        feedback "Creating backup directory $BACKUPDIR."
        mkdir --parents --verbose "$BACKUPDIR"
    fi
    if test ! -f "$BACKUPDIR/db/fs-type"; then
        feedback
        echo "$BACKUPDIR/db/fs-type not found."
        echo "Please fetch the first copy of the repository database yourself."
        echo "I only assist in doing incremental backups of repository databases."
        exit 1
    fi
    feedback "Checking that repository format is FSFS."
    test "`cat "$BACKUPDIR/db/fs-type"`" == fsfs
    feedback "Changing to database directory $BACKUPDIR/db."
    cd "$BACKUPDIR/db"
    feedback "Getting local current revision number."
    LOCALREVNUM="`cat current | sed 's/ .*//'`"
    feedback "Checking that current.new doesn't exist."
    if test -f current.new; then
        feedback
        echo "Make sure that no other instance of this script"
        echo "is running and delete the following file:"
        echo "$BACKUPDIR/db/current.new"
        exit 2
    fi
    feedback "Getting remote 'current' file."
    ssh "$HOST" "cat '$REMOTEDIR/db/current'" > current.new
    feedback "Getting remote current revision number."
    REMOTEREVNUM="`cat current.new | sed 's/ .*//'`"
    feedback "Checking that we got a response from the server."
    test -n "$REMOTEREVNUM"
    if ! test "$LOCALREVNUM" -le "$REMOTEREVNUM"; then
        feedback
        echo "ERROR: Local revision number ($LOCALREVNUM) greater"
        echo "       than remote revision number ($REMOTEREVNUM)."
        echo "Wrong backup directory or changed repository?"
        exit 2
    fi
    if test "$LOCALREVNUM" -eq "$REMOTEREVNUM"; then
        feedback "No backup needed; at revision $LOCALREVNUM."
        feedback "Removing 'current.new'."
        rm current.new
    else
        LOCALREVNUM="$[$LOCALREVNUM+1]"
        feedback "Backing up from revision $LOCALREVNUM to revision $REMOTEREVNUM."
        test "$printfeedback" && verbose=-v || verbose=
        ssh "$HOST" "
            set -e;
            cd $REMOTEDIR/db/;
            nice -n 10 tar cf - \`seq -f revs/%g $LOCALREVNUM $REMOTEREVNUM\` \`seq -f revprops/%g $LOCALREVNUM $REMOTEREVNUM\` | nice -n 10 bzip2 -c" \
            | tar $verbose -xjf -
        feedback "Renaming 'current.new' to 'current'."
        mv current.new current
    fi
    feedback "Done."
    feedback ""
}

feedback 'Reading ~/.fsfsbackup.'
feedback ''
source ~/.fsfsbackup
feedback 'Finished.'
trap - 0 1 2 3 15
