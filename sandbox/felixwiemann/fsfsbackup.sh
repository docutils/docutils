#!/bin/bash

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This file has been placed in the public domain.

set -e

function do_backup() {
    # If any of the tests fails, the script terminates silently.
    echo "Checking that all necessary variables are set."
    test -n "$BACKUPDIR"
    test -n "$USERNAME"
    test -n "$HOST"
    test -n "$REMOTEDIR"
    if test ! -d "$BACKUPDIR"; then
        echo "Creating backup directory $BACKUPDIR."
        mkdir --parents --verbose "$BACKUPDIR"
    fi
    if test ! -f "$BACKUPDIR/db/fs-type"; then
        echo
        echo "$BACKUPDIR/db/fs-type not found."
        echo Please fetch the first copy of the repository database yourself.
        echo I only assist in doing incremental backups of repository databases.
        exit 1
    fi
    echo "Checking that repository format is FSFS."
    test "`cat "$BACKUPDIR/db/fs-type"`" == fsfs
    echo "Changing to database directory $BACKUPDIR/db."
    cd "$BACKUPDIR/db"
    echo "Getting local current revision number."
    LOCALREVNUM="`cat current | sed 's/ .*//'`"
    echo "Checking that current.new doesn't exist."
    if test -f current.new; then
        echo
        echo "Make sure that no other instance of this script"
        echo "is running and delete the following file:"
        echo "$BACKUPDIR/db/current.new"
        exit 2
    fi
    echo "Getting remote 'current' file."
    ssh "$USERNAME@$HOST" "cat '$REMOTEDIR/db/current'" > current.new
    echo "Getting remote current revision number."
    REMOTEREVNUM="`cat current.new | sed 's/ .*//'`"
    echo "Checking that we got a response from the server."
    test -n "$REMOTEREVNUM"
    if ! test "$LOCALREVNUM" -le "$REMOTEREVNUM"; then
        echo
        echo "ERROR: Local revision number ($LOCALREVNUM) greater"
        echo "       than remote revision number ($REMOTEREVNUM)."
        echo "Wrong backup directory or changed repository?"
        exit 2
    fi
    if test "$LOCALREVNUM" -eq "$REMOTEREVNUM"; then
        echo "No backup needed; at revision $LOCALREVNUM."
        echo "Removing 'current.new'."
        rm current.new
        echo "Done."
        exit 0
    fi
    LOCALREVNUM="$[$LOCALREVNUM+1]"
    echo "Backing up from revision $LOCALREVNUM to revision $REMOTEREVNUM."
    ssh "$USERNAME@$HOST" "
        set -e;
        cd $REMOTEDIR/db/;
        nice -n 10 tar cf - \`seq -f revs/%g $LOCALREVNUM $REMOTEREVNUM\` \`seq -f revprops/%g $LOCALREVNUM $REMOTEREVNUM\` | nice -n 10 bzip2 -c" \
        | tar xjvf -
    echo "Renaming 'current.new' to 'current'."
    mv current.new current
    echo "Done."
}

echo 'Reading ~/.fsfsbackup.'
source ~/.fsfsbackup
