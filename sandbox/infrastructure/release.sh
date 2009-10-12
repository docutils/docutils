#!/bin/bash

# Author: Lea Wiemann
# Contact: LeWiemann@gmail.com
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

function svn_up()
{
    if test $svn == svk; then
        confirm svk sync "$depot"
    fi
    confirm $svn up
}

function checkin()
{
    # Parameters: log_message, file, file, file ...
    log_message="$1"
    shift
    confirm $svn diff "$@"
    confirm $svn ci -m "$log_prefix $log_message" "$@"
}

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
    checkin "set version number to $2" $files
}

function usage()
{
    echo 'Usage:'
    echo
    echo '    release.sh new_version svn_version[:branch_version] [stage]'
    echo
    echo 'The following things will be done:'
    echo
    echo '* Change version number to new_version.  (stage 1)'
    echo '* SVN-export, test, and release Docutils version new_version.  (stage 2)'
    echo '* Change version number to svn_version.  (stage 3)'
    echo
    echo 'If stage is supplied (1, 2 or 3), only the specified stage will'
    echo 'be executed.  Otherwise, it defaults to executing all stages.'
    echo
    echo 'Before doing dangerous things, you will be asked to press enter.'
    echo
    echo 'A maintenance branch called docutils-new_version will be created'
    echo 'if branch_version is given.  The version number inside the'
    echo 'maintenance branch will be set to branch_version.'
    echo 
    echo 'Access ssh,scp and sftp access to shell/frs.sourceforge.net'
    echo 'must be configured.'
    exit 1
}

function initialize()
{
    if [ "$#" -lt 2 ]; then
        usage
    fi
    echo 'Initializing...'
    python_versions='2.3 2.4 2.5 2.6'
    for py_ver in $python_versions; do
        echo -n "Checking for Python $py_ver (python$py_ver)... "
        if ! echo 'print "OK"' | python$py_ver; then
            echo "Python $py_ver (python$py_ver) not found."
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
    echo -n 'Subversion binary to use: '
    if [ -d .svn ]; then
        svn=svn
    else
        svn=svk
    fi
    echo $svn
    working_copy="`pwd -P`"
    echo "Working copy: $working_copy"
    if test $svn = svk; then
        depot_path="`svk info . | grep ^Depot\ Path: | sed 's/Depot Path: //'`"
        depot="`echo "$depot_path" | sed 's|\(//[^/]\+/[^/]\+\).*|\1|'`"
        echo "SVK depot: $depot"
        mirrored_from="`svk info . | grep ^Mirrored\ From: | sed 's/Mirrored From: //;s/, Rev\. .*//'`"
        svnurl="$mirrored_from`echo "$depot_path" | sed 's|//[^/]\+/[^/]\+||'`"
    else
        svnurl="`$svn info . | grep ^URL: | sed 's/URL: //'`"
    fi
    if test -z "$svnurl"; then
        echo 'Unable to detect Subversion URL.  Aborting.'
        exit 1
    fi
    echo "Subversion URL: $svnurl"
    if ! echo "$svnurl" | grep -q 'branches\|trunk'; then
        echo 'Subversion URL contains neither "branches" nor "trunk".'
        echo 'Aborting.'
        exit 1
    fi
    svnroot="`echo "$svnurl" | sed 's/\/\(branches\|trunk\).*//'`"
    echo "Subversion root URL: $svnroot"
    if test "$svnurl" = "$svnroot"; then
        echo 'Error: Subversion URL and Subversion root URL are the same.'
        exit 1
    fi
    echo -n 'Detecting current Docutils version... '
    old_ver="`python -c 'import docutils; print docutils.__version__'`"
    echo "$old_ver"
    new_ver="$1"
    # log_prefix is for SVN logs.
    log_prefix="Release $new_ver:"
    echo "New version number (for releasing): $new_ver"
    svn_ver="$2"
    if echo "$svn_ver" | grep -q :; then
        # Split at colon: svn_ver:branch_ver
        branch_ver="${svn_ver#*:}"
        svn_ver="${svn_ver%:*}"
    else
        branch_ver=
    fi
    echo "New Subversion version number (after releasing): $svn_ver"
    echo -n 'Create maintenance branch: '
    if test "$branch_ver"; then
        echo yes
        echo "New version number on maintenance branch: $branch_ver"
    else
        echo no
    fi
    if test "$branch_ver"; then
        echo -n 'Checking that we have a full checkout... '
        if echo "$working_copy" | grep -q 'branches\|trunk'; then
            echo OK
        else
            echo 'no'
            echo 'Working copy path contains neither "branches" nor "trunk".'
            echo 'You need a full checkout in order to branch.'
            echo 'Aborting.'
            exit 1
        fi
        wcroot="`echo "$working_copy" | sed 's/\/\(branches\|trunk\).*//'`"
        echo "Working copy root: $wcroot"
    fi
    tarball=docutils-"$new_ver".tar.gz
    echo "Tarball name: $tarball"
    echo 'Initialization completed.'
    echo
}

function test_tarball()
{
    # Assume we have the tarball in the current directory.
    # Pass test number as first parameter.
    echo 'Testing the release tarball.'
    run mkdir tarball_test/
    run cd tarball_test/
    confirm tar xzvf "../$tarball"
    echo
    run cd docutils-"$new_ver"
    echo 'Installing the distribution.'
    # Extra files, with leading comma.
    extras="`cd extras; for extrafile in *.py; do echo -n ",$extrafile{,c,o}"; done`"
    confirm su -c '
        for py_ver in '"$python_versions"'; do
            echo "Deleting and installing Docutils on Python $py_ver."
            echo "Press enter."
            read
            site_packages="/usr/local/lib/python$py_ver/site-packages"
            if test ! -d "$site_packages"; then
                site_packages="/usr/lib/python$py_ver/site-packages"
            fi
            if test ! -d "$site_packages"; then
                echo "Error: \"$site_packages\" does not exist."
                exit 1
            fi
            if test -e "$site_packages/docutils-test"; then
                echo "Error: \"$site_packages/docutils-test\" exists."
                exit 1
            fi
            rm -rfv /usr/{local,}lib/python$py_ver/site-packages/{docutils'"$extras"'}
            python$py_ver setup.py install
            echo
            echo "Copying the test suite to the site-packages directory of Python $py_ver."
            echo "Press enter."
            read
            cp -rv test "$site_packages/docutils-test"
        done'
    echo
    echo 'Running the test suite as root with all Python versions.'
    for py_ver in $python_versions; do
        site_packages="/usr/local/lib/python$py_ver/site-packages"
        if test ! -d "$site_packages"; then
            site_packages="/usr/lib/python$py_ver/site-packages"
        fi
        if test ! -d "$site_packages"; then
            echo "Error: \"$site_packages\" does not exist."
            exit 1
        fi
        confirm su -c "python$py_ver -u \"$site_packages/docutils-test/alltests.py\""
        # BUG shell script exits after alltests.py
    done
    run cd ../..
    echo "Cleaning up..."
    confirm su -c "run rm -rf tarball_test"
    confirm su -c '
        for py_ver in '"$python_versions"'; do
            rm -rfv /usr{/local,}/lib/python$py_ver/site-packages/docutils{-test,}
        done'
    echo
}

function upload_tarball()
{
    # Assume we have the tarball in the working area.
    run cd "$working_area"
    # You may need to adjust the command line for clients other than tnftp.
	# BUG this changed: echo "put docutils-$new_ver.tar.gz" | sftp frs.sourceforge.net:uploads/
	# upload releasenotes too
    # upload via sftp should work, but might not show up on project feed.
	echo "use sftp to put the file into /home/frs/project/d/do/docutils/docutils/$new_ver"
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
    # BUG no docutils installation left.
    # BUG and it breaks on test/functional/input/standalone_rst_newlatex.txt:
    #     1020: (SEVERE/4) Title level inconsistent
    #     because this is an include file.
    confirm ./buildhtml.py ..
    run cd ..
    echo '$ find -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null'
    find -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null
    echo 'Upload the tarball to your home directory on SF.net...'
    # BUG: hostname changed to web.sourceforge.net
    # BUG: we do not have shell access.
    # Try::
    #  mkdir $new_ver
    #  cd $new_ver
    #  tar xjvf ../docutils-docs.tar.bz2
	#  cd ..
	#  chmod -R g+rw $new_ver
	#  scp -r -p -C $new_ver web.sourceforge.net:/home/groups/d/do/docutils/htdocs
    confirm scp docutils-docs.tar.bz2 shell.sourceforge.net:
    echo
    echo 'Unpack the tarball on SF.net...'
    echo 'Press enter (or enter anything to skip).'
    read
    if [ ! "$REPLY" ]; then
        ssh shell.sourceforge.net<<-EOF
            set -x
            umask 002
            cd /home/groups/d/do/docutils/htdocs/
            mkdir -m g+rwxs $new_ver
            cd $new_ver
            tar -xjvf ~/docutils-docs.tar.bz2
            rm ~/docutils-docs.tar.bz2
EOF
    fi
}

function create_maintenance_branch()
{
    echo 'Creating maintenance branch.'
    branch_name="docutils-$new_ver"
    echo "Branch name: $branch_name"
    branch_url="$svnroot/branches/$branch_name"
    echo "Branch URL: $branch_url"
    confirm svn cp "$svnurl" "$branch_url" -m \
        "$log_prefix created maintenance branch for version $new_ver"
    cd "$wcroot"
    svn_up
    cd branches/"$branch_name"
    set_ver "$new_ver" "$branch_ver"
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
    svn_up
    echo
    # update __version_details__ string
    (echo ",s/^__version_details__ = .*\$/__version_details__ = 'release'/";
        echo wq) | ed docutils/__init__.py 2> /dev/null
    set_ver "$old_ver" "$new_ver"
    echo
    history_files='HISTORY.txt RELEASE-NOTES.txt'
    echo "Now updating the following files: $history_files"
    old_string="Changes Since [0-9.]+"
    new_string="Release $new_ver (`date --utc --iso-8601`)"
    echo 'Press enter to replace "'"$old_string"'" with "'"$new_string"\",
    echo 'or enter anything to skip.'
    read
    test "$REPLY" || python -c "for filename in '$history_files'.split():
        import re
        h = file(filename).read()
        h = re.sub('$old_string\\n=+', '$new_string\\n' + '=' * len('$new_string'), h, count=1)
        file(filename, 'w').write(h)"
    checkin 'closed "Changes Since ..." section' $history_files
}

function stage_2()
{
    echo 'Creating working area...'
    working_area="`echo ~/tmp/docutils-release.$$`"
    run mkdir -p "$working_area"
    echo
    echo 'Getting a fresh export.'
    echo 'Press enter to proceed (or enter anything to skip)...'
    read
    if [ ! "$REPLY" ]; then
        run cd "$working_area"
        confirm svn export "$svnurl"
        echo
        echo 'Building the release tarball.'
        run cd docutils
        confirm ./setup.py sdist
        run cd ..
        echo 'Tarball built.'
        run cp docutils/dist/"$tarball" .
        confirm test_tarball
        echo "Testing documentation and uploading htdocs of version $new_ver..."
        confirm upload_htdocs
        echo "Tagging current revision..."
        confirm svn cp "$svnurl" "$svnroot/tags/docutils-$new_ver/" -m "$log_prefix tagging released revision"
        echo "Uploading $tarball to SF.net."
        confirm upload_tarball
        echo 'Now go to https://sourceforge.net/project/admin/editpackages.php?group_id=38414'
        echo 'and follow the instructions at'
        echo 'http://docutils.sf.net/docs/dev/release.html#file-release-system'
        echo 'BUG find your way.'
        echo
        echo 'Then press enter.'
        read
    fi
    run cd $working_area
    echo 'Downloading the tarball to verify its integrity.'
    while true; do
	    # BUG path is wrong. project admin filemanager shows md5sum
        confirm wget http://osdn.dl.sourceforge.net/sourceforge/docutils/"$tarball"
        echo 'Was the download successful?'
        echo 'If yes, press enter to continue, otherwise enter anything to repeat'
        echo '(it is possible that the file will show up in a few minutes).'
        read
        test "$REPLY" || break
    done
    confirm test_tarball
    echo 'Registering with PyPI...'
    echo 'Press enter to proceed (or enter anything to skip)...'
    read
    if [ ! "$REPLY" ]; then
        echo "Unpacking tarball..."
        ls -l
        pwd
        run tar xzvf "$tarball"
        run cd docutils-"$new_ver"
        confirm ./setup.py register
    fi
}

function stage_3()
{
    svn_up
    echo
    # update __version_details__ string
    (echo ",s/^__version_details__ = .*\$/__version_details__ = 'repository'/";
        echo wq) | ed docutils/__init__.py 2> /dev/null
    checkin 'set __version_details__ to "repository"'
    echo
    history_files='HISTORY.txt RELEASE-NOTES.txt'
    echo "Now updating the following files: $history_files"
    add_string="Changes Since $new_ver"
    before="Release "
    echo 'Press enter to add "'"$add_string"'" section,'
    echo 'or enter anything to skip.'
    read
    test "$REPLY" || python -c "for filename in '$history_files'.split():
        import re
        h = file(filename).read()
        h = re.sub('\n$before', '\\n$add_string\\n' + '=' * len('$add_string') +
                   '\\n\\n\\n$before', h, count=1)
        file(filename, 'w').write(h)"
    checkin "added empty \"Changes Since $new_ver\" section" $history_files
    echo
    if test "$branch_ver"; then
        create_maintenance_branch
        cd "$working_copy"
    fi
    set_ver "$new_ver" "$svn_ver"
    echo
    echo 'Please update the web page now (web/index.txt).'
    echo "Press enter when you're done."
    read
    echo 'Running docutils-update on the server...'
    echo 'This may take some time.'
    confirm ssh shell.berlios.de docutils-update
}

initialize "$@"
run_stage "$3"
echo
echo 'Finished.'
echo 'Run alltests.py on svn version now.'

# Local Variables:
# indent-tabs-mode: nil
# End:
