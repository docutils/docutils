#!/bin/bash

# Author: Lea Wiemann
# Contact: grubert@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This script has been placed in the public domain.

# USAGE see: docutils/docs/dev/release.txt

# must be run from docutils trunk/docutils, 
# because HISTORY and RELEASE_NOTES.txt are modified.

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
    python_versions='2.6 2.7'
    for py_ver in $python_versions; do
        echo -n "Checking for Python $py_ver (python$py_ver)... "
        if ! echo 'print "OK"' | python$py_ver; then
            echo "WARN: Python $py_ver (python$py_ver) not found."
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
    # TODO breaks for newer svn, .svn is in upper. is this for svk 
    svn=svn
    echo $svn
    working_copy="`pwd -P`"
    echo "Working copy: $working_copy"
    svnurl="`$svn info . | grep ^URL: | sed 's/URL: //'`"
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
    if test `uname` = "Darwin" ; then
        svnroot="`echo "$svnurl" | sed -E 's/\/(branches|trunk).*//'`"
    else
        svnroot="`echo "$svnurl" | sed 's/\/\(branches\|trunk\).*//'`"
    fi
    echo "Subversion root URL: $svnroot"
    if test "$svnurl" = "$svnroot"; then
        echo 'Error: Subversion URL and Subversion root URL are the same.'
        echo '  probably a MacOSX sed problem.'
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
    echo 'Deleteing old installations. Installing the distribution.'
    echo "WARN: might not find installation."
    for py_ver in '"$python_versions"'; do
        echo "python$py_ver install/update and test."
        bash release-test.sh
        echo "Enter to test next."
        read
    done
}

function upload_tarball()
{
    # Assume we have the tarball in the working area.
    run cd "$working_area"
    mkdir $new_ver
    cp docutils-$new_ver.tar.gz $new_ver
    cp docutils/RELEASE-NOTES.txt $new_ver
    # README.txt would be displayed automatically on sf.
    # BUG user grubert hardcoded
    # short path  "/home/frs/project/docutils/docutils/" also exists
    scp -r $new_ver grubert,docutils@frs.sourceforge.net:/home/frs/project/d/do/docutils/docutils/
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
    echo "BUG no docutils installation left."
    echo "DO NOT call manually in $(pwd)"
    confirm ./buildhtml.py --local ..
    confirm ./buildhtml.py ../docs
    run cd ..
    echo '$ find . -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null'
    find . -name test -type d -prune -o -name \*.css -print0 \
        -o -name \*.html -print0 -o -name \*.txt -print0 \
        | tar -cjvf docutils-docs.tar.bz2 -T - --null
    echo 'Upload docs to SF.net...'
    echo 'Press enter (or enter anything to skip).'
    read
    if [ ! "$REPLY" ]; then
       mkdir $new_ver
       cd $new_ver
       tar xjvf ../docutils-docs.tar.bz2
	   cd ..
	   chmod -R g+rw $new_ver
	   scp -r -p -C $new_ver web.sourceforge.net:/home/groups/d/do/docutils/htdocs
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
        echo 'BETTER run release-test.sh manually for each installed python version.'
        echo "DO: cd $working_area and call sandbox/infrastructure/release-test.sh py_ver docutils_ver"
        confirm test_tarball
        echo "Testing documentation and uploading htdocs of version $new_ver..."
        confirm upload_htdocs
        echo "Tagging current revision..."
        confirm svn cp "$svnurl" "$svnroot/tags/docutils-$new_ver/" -m "$log_prefix tagging released revision"
        echo "Uploading $tarball to SF.net."
        confirm upload_tarball
        echo 'Now go to https://sourceforge.net/projects/docutils/files/docutils'
        echo 'and follow the instructions at'
        echo 'http://docutils.sf.net/docs/dev/release.html#file-release-system'
        echo
        echo 'Then press enter.'
        read
    fi
    run cd $working_area
    echo 'Downloading the tarball to verify its integrity.'
    while true; do
	    # BUG path is wrong. project admin filemanager shows md5sum
        confirm wget http://sourceforge.net/projects/docutils/files/"$tarball"
        echo 'Was the download successful?'
        echo 'If yes, press enter to continue, otherwise enter anything to repeat'
        echo '(it is possible that the file will show up in a few minutes).'
        read
        test "$REPLY" || break
    done
    confirm test_tarball
    echo 'we are registered with PyPI...'
    echo 'upload to pypi with twine or set download url for this release'

    echo 'Press enter to proceed (or enter anything to skip)...'
    read
    if [ ! "$REPLY" ]; then
        echo "Unpacking tarball..."
        ls -l
        pwd
        run twine upload "$tarball"
        echo "verify on PyPI. hide older releases."
        echo "TODO TEST build wheels and upload"
        run pip wheel docutils
        run twine upload docutils-*-py2-none-any.whl
        run pip3 wheel docutils
        run twine upload wheelhouse/docutils-${new_ver}-py3-none-any.whl
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
    echo 'Please change version number in README.txt'
    echo
    echo 'Please update the web page now (web/index.txt).'
    echo 'cd into sandbox/infrastructure'
    echo 'and call docutils-update.local (requires linux, macosx cp misses something)'
    echo "Press enter when you're done."
    read
}

initialize "$@"
run_stage "$3"
echo
echo 'Finished.'
echo 'Run alltests.py on svn version now.'

# Local Variables:
# indent-tabs-mode: nil
# End:
