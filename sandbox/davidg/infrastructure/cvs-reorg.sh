#!/bin/sh
# CVS reorganization script for Docutils (docutils.sf.net).
# By Felix Wiemann and David Goodger
# Part of the plan described in
# <http://docutils.sf.net/spec/notes.html#documentation-cleanup>.

# Paths should be absolute (implied by
# http://sf.net/docman/display_doc.php?docid=768&group_id=1#repositoryrestructure).

# Exit on error.
set -e

############################################################

echo
echo 'Remove module & files added by mistake...'

rm -rfv /cvsroot/docutils/modulename     # duplicate of sandbox module
rm -rfv /cvsroot/docutils/docutils/Attic # duplicates of CVSROOT files
rm -rfv /cvsroot/docutils/docutils/CVSROOT # another duplicate

############################################################

echo
echo 'Create new empty directory structure...'

# introductory/tutorial material for end-users
mkdir -v /cvsroot/docutils/docutils/docs/user/

# for core-developers
mkdir -v /cvsroot/docutils/docutils/docs/dev/
mkdir -v /cvsroot/docutils/docutils/docs/dev/rst/

# reference material for all groups
mkdir -v /cvsroot/docutils/docutils/docs/ref/   
mkdir -v /cvsroot/docutils/docutils/docs/ref/rst/

# Python Enhancement Proposals
mkdir -v /cvsroot/docutils/docutils/docs/peps/  

############################################################

echo
echo 'Move entire subdirectories...'

mv -iv /cvsroot/docutils/docutils/docs/rst \
       /cvsroot/docutils/docutils/docs/user/rst

# for component-developers and core-developers
mv -iv /cvsroot/docutils/docutils/spec/howto \
       /cvsroot/docutils/docutils/docs/howto

############################################################

echo
echo 'Move (& rename) individual files...'

# All file names have to end with ",v"; this is the CVS repository!

mv -iv /cvsroot/docutils/docutils/docs/config.txt,v \
       /cvsroot/docutils/docutils/docs/user/
mv -iv /cvsroot/docutils/docutils/docs/latex.txt,v \
       /cvsroot/docutils/docutils/docs/user/
mv -iv /cvsroot/docutils/docutils/docs/tools.txt,v \
       /cvsroot/docutils/docutils/docs/user/

mv -iv /cvsroot/docutils/docutils/spec/pysource.dtd,v \
       /cvsroot/docutils/docutils/docs/dev/
mv -iv /cvsroot/docutils/docutils/spec/pysource.txt,v \
       /cvsroot/docutils/docutils/docs/dev/
mv -iv /cvsroot/docutils/docutils/spec/semantics.txt,v \
       /cvsroot/docutils/docutils/docs/dev/
mv -iv /cvsroot/docutils/docutils/spec/notes.txt,v \
       /cvsroot/docutils/docutils/docs/dev/todo.txt,v # rename

mv -iv /cvsroot/docutils/docutils/spec/doctree.txt,v \
       /cvsroot/docutils/docutils/docs/ref/
mv -iv /cvsroot/docutils/docutils/spec/docutils.dtd,v \
       /cvsroot/docutils/docutils/docs/ref/
mv -iv /cvsroot/docutils/docutils/spec/soextblx.dtd,v \
       /cvsroot/docutils/docutils/docs/ref/

mv -iv /cvsroot/docutils/docutils/spec/transforms.txt,v \
       /cvsroot/docutils/docutils/docs/ref/

mv -iv /cvsroot/docutils/docutils/spec/rst/alternatives.txt,v \
       /cvsroot/docutils/docutils/docs/dev/rst/
mv -iv /cvsroot/docutils/docutils/spec/rst/problems.txt,v \
       /cvsroot/docutils/docutils/docs/dev/rst/

mv -iv /cvsroot/docutils/docutils/spec/rst/reStructuredText.txt,v \
       /cvsroot/docutils/docutils/docs/ref/rst/restructuredtext.txt,v # rename
mv -iv /cvsroot/docutils/docutils/spec/rst/directives.txt,v \
       /cvsroot/docutils/docutils/docs/ref/rst/
mv -iv /cvsroot/docutils/docutils/spec/rst/interpreted.txt,v \
       /cvsroot/docutils/docutils/docs/ref/rst/roles.txt,v # rename
mv -iv /cvsroot/docutils/docutils/spec/rst/introduction.txt,v \
       /cvsroot/docutils/docutils/docs/ref/rst/

mv -iv /cvsroot/docutils/docutils/spec/pep-????.txt,v \
       /cvsroot/docutils/docutils/docs/peps/

############################################################

echo
echo 'Remove old, unused, empty directories...'

# nothing interesting in here
rm -rfv /cvsroot/docutils/docutils/spec/Attic

# empty
rmdir -v /cvsroot/docutils/docutils/spec/rst
rmdir -v /cvsroot/docutils/docutils/spec

############################################################

echo
echo Finished.
