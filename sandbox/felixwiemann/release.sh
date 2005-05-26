#!/bin/bash

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.

set -e

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

function confirm()
{
    # Print, let the user confirm and run "$@".
    echo 'Press enter to run (or enter anything to skip):'
    print_command "$@"
    read
    test "$REPLY" && echo Skipped. || "$@"
}

function checkin()
{
    # Parameters: log_message, file, file, file ...
    log_message="$1"
    shift
    confirm svn diff "$@"
    confirm svn ci -m "$log_prefix $log_message" "$@"
}

function set_ver()
{
    # Parameters: old_version new_version
    shopt -s extglob
    files="`ls docutils/__init__.py setup.py test/functional/expected/!(.svn)`"
    echo "Now I'll change the version number to $2 in the following files:"
    echo "$files"
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
    checkin "set version number to $2" $files
}

function usage()
{
    echo 'Usage:'
    echo
    echo '    release.sh new_version svn_version [stage]'
    echo
    echo 'The following things will be done:'
    echo
    echo '* Change version number to new_version.  (stage 1)'
    echo '* SVN-export, test and release Docutils version new_version.  (stage 2)'
    echo '* Change version number to svn_version.  (stage 3)'
    echo
    echo 'If stage is supplied (1, 2 or 3), only the specified stage will'
    echo 'be executed.  Otherwise, it defaults to executing all stages.'
    echo
    echo 'Before doing dangerous things, you will be asked to press enter.'
    exit 1
}

function initialize()
{
    if [ "$#" -lt 2 ]; then
        usage
    fi
    echo 'Initializing...'
    python_versions='2.1 2.2 2.3 2.4'
    for py_ver in $python_versions; do
        echo -n "Checking for Python $py_ver (python$py_ver)... "
        if ! echo 'print "OK"' | python$py_ver; then
            echo "Python $py_ver not found."
            echo Aborting.
            exit 1
        fi
    done
    echo -n 'Clearing $PYTHONPATH... '
    export PYTHONPATH=
    echo 'done'
    echo -n 'Checking whether we are in a working copy... '
    if [ -f HISTORY.txt ]; then
        echo yes
    else
        echo "no (HISTORY.txt doesn't exist)"
        echo 'Aborting.'
        echo 'Please cd to a working copy before running this script.'
        exit 1
    fi
    working_copy="`pwd`"
    echo -n 'Detecting Subversion root... '
    svnroot="`svn info . | grep ^URL: | sed 's/URL: //'`"
    echo "$svnroot"
    echo -n 'Detecting current Docutils version... '
    old_ver="`python -c 'import docutils; print docutils.__version__'`"
    echo "$old_ver"
    new_ver="$1"
    # log_prefix is for SVN logs.
    log_prefix="Release $1:"
    echo "New version number (for releasing): $new_ver"
    svn_ver="$2"
    echo "New Subversion version number (after releasing): $svn_ver"
    echo 'Initialization completed.'
    echo
}

function test_tarball()
{
    # Assume we have the tarball in the current directory.
    # Pass test number as first parameter.
    echo 'Testing the release tarball.'
    run mkdir test$1/
    run cd test$1/
    confirm tar xzvf "../$tarball"
    echo
    run cd docutils-"$new_ver"
    echo 'Installing the distribution.'
    # Extra files, with leading comma.
    extras="`cd extras; for extrafile in *.py; do echo -n ",$extrafile{,c,o}"; done`"
    confirm su -c '
        for py_ver in '"$python_versions"'; do
            echo "Deleting and installing Docutils on Python $py_ver.  Press enter."
            read
            rm -rfv /usr/{local,}lib/python$py_ver/site-packages/{docutils'"$extras"'}
            python$py_ver setup.py install
            echo "Press enter to continue."
            read
        done'
    run cd test
    echo 'Running the test suite with all Python versions.'
    echo 'Press enter (or enter anything to skip):'
    read
    if [ ! "$REPLY" ]; then
        for py_ver in $python_versions; do
            run python$py_ver -u alltests.py
            run find -name \*.pyc -exec rm {} \;
        done
    fi
    run cd ../..
    echo
}

function upload_tarball()
{
    # Assume we have the tarball in the working area.
    run cd "$working_area"
    # You may need to adjust the command line for clients other than tnftp.
    (echo 'bin'; echo 'cd /incoming'; echo "put $tarball") | \
        ftp ftp://anonymous:none@upload.sourceforge.net/
    echo 'Upload completed.'
}

function upload_htdocs()
{
    # Assume we have the tarball in the working area.
    run cd "$working_area"
    echo "Upload htdocs for $new_ver"
    run mkdir htdocs
    run cd htdocs
    confirm tar xzvf "../$tarball"
    run cd docutils-"$new_ver"/tools/
    confirm ./buildhtml.py ..
    run cd ..
    echo '$ find -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null'
    find -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null
    echo 'Upload the tarball to your home directory on SF.net...'
    confirm scp docutils-docs.tar.bz2 "$username"@shell.sourceforge.net:
    echo
    echo 'Unpack the tarball on SF.net...'
    echo 'Press enter (or enter anything to skip).'
    read
    if [ ! "$REPLY" ]; then
        ssh "$username"@shell.sourceforge.net<<-EOF
            set -x
            cd /home/groups/d/do/docutils/htdocs/
            mkdir -m g+rwxs $new_ver
            cd $new_ver
            tar -xjvf ~/docutils-docs.tar.bz2
            rm ~/docutils-docs.tar.bz2
EOF
    fi
}

function run_stage()
{
    if [ ! "$1" ]; then
        run_stage 1
        echo
        run_stage 2
        echo
        run_stage 3
    else
        echo "Press enter to run stage $1 (or enter anything to skip this stage)."
        read
        if [ ! "$REPLY" ]; then
            cd "$working_copy"
            if [ "$1" == 1 ]; then
                stage_1
            elif [ "$1" == 2 ]; then
                stage_2
            elif [ "$1" == 3 ]; then
                stage_3
            else
                echo 'Invalid stage.'
                echo
                usage
            fi
            echo
            echo "Stage $1 completed."
        else
            echo "Skipped stage $1."
        fi
    fi
}

function stage_1()
{
    confirm svn up
    echo
    set_ver "$old_ver" "$new_ver"
    echo
    echo 'Now updating HISTORY.txt...'
    old_string="Changes Since [0-9.]+"
    new_string="Release $new_ver (`date --utc --iso-8601`)"
    echo 'Press enter to replace "'"$old_string"'" by "'"$new_string"\",
    echo 'or enter anything to skip.'
    read
    test "$REPLY" || python -c "if 1: # for indentation
        import re
        h = file('HISTORY.txt').read()
        h = re.sub('$old_string\\n=+', '$new_string\\n' + '=' * len('$new_string'), h, count=1)
        file('HISTORY.txt', 'w').write(h)"
    checkin 'closed "Changes Since ..." section' HISTORY.txt
}

function stage_2()
{
    echo 'Creating working area...'
    working_area="`echo ~/tmp/docutils-release.$$`"
    run mkdir -p "$working_area"
    echo
    echo 'Getting a fresh export.'
    run cd "$working_area"
    confirm svn export "$svnroot"
    echo
    echo 'Building the release tarball.'
    run cd docutils
    confirm ./setup.py sdist
    run cd ..
    echo 'Tarball built.'
    tarball=docutils-"$new_ver".tar.gz
    run cp docutils/dist/"$tarball" .
    confirm test_tarball 1
    echo "Testing documentation and uploading htdocs of version $new_ver..."
    confirm upload_htdocs
    echo "Uploading $tarball to SF.net."
    confirm upload_tarball
    echo 'Now go to https://sourceforge.net/project/admin/editpackages.php?group_id=38414'
    echo 'and follow the instructions at'
    echo 'http://docutils.sf.net/docs/dev/release.html#file-release-system'
    echo
    echo 'Then press enter.'
    read
    echo 'Rebuilding the working area.'
    confirm su -c "rm -rf $working_area"
    run mkdir $working_area
    run cd $working_area
    echo 'Downloading the tarball to verify its integrity.'
    while true; do
        confirm wget http://osdn.dl.sourceforge.net/sourceforge/docutils/"$tarball"
        echo 'Was the download successful?'
        echo 'If yes, press enter to continue, otherwise enter anything to repeat'
        echo '(it is possible that the file will show up in a few minutes).'
        read
        test "$REPLY" || break
    done
    confirm test_tarball 2
    echo
    echo 'Registering at PyPI...'
    run cd $working_area
    run cd test2
    confirm ./setup.py register
}

function stage_3()
{
    confirm svn up
    echo
    set_ver "$new_ver" "$svn_ver"
    echo
    echo 'Now updating HISTORY.txt...'
    add_string="Changes Since $new_ver"
    before="Release "
    echo 'Press enter to replace "'"$add_string"'" section,'
    echo 'or enter anything to skip.'
    read
    test "$REPLY" || python -c "if 1: # for indentation
        import re
        h = file('HISTORY.txt').read()
        h = re.sub('$before', '$add_string\\n' + '=' * len('$add_string') +
                   '\\n\\n\\n$before', h, count=1)
        file('HISTORY.txt', 'w').write(h)"
    checkin "added empty \"Changes Since $new_ver\" section" HISTORY.txt
    echo
    echo 'Please update the web page now (web/index.txt).'
    echo "Press enter when you're done."
    read
    echo 'Press enter to run docutils-update on SF.net (or enter anything to skip):'
    read
    if [ ! "$REPLY" ]; then
        echo 'Running docutils-update on the server...'
        echo 'This may take some time.'
        echo
        echo '$ echo /home/groups/d/do/docutils/snapshots/sandbox/davidg/infrastructure/docutils-update -p | \' #'
        echo "    ssh $username@shell.sourceforge.net"
        test "$REPLY" || echo /home/groups/d/do/docutils/snapshots/sandbox/davidg/infrastructure/docutils-update -p | ssh $username@shell.sourceforge.net
        echo
        echo 'docutils-update completed.'
    fi
    echo 
}

initialize "$@"
run_stage "$3"
echo
echo 'Finished.'
