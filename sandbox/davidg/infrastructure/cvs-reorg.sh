#!/bin/sh

cd /cvsroot/docutils/docutils/

set -e  # Exit on error.

# Ensure that we really are in the docutils module.
test -d ./docs -a -d ./spec

alias 'md=mkdir -v'
alias 'mv=mv -v'

md docs/user/  # introductory/tutorial material for end-users
md docs/dev/   # for core-developers
md docs/ref/   # reference material for all groups
md docs/lib/   # API reference material for library-user/developers
md docs/peps/  # Python Enhancement Proposals
md docs/dev/rst/
md docs/ref/rst/

mv docs/config.txt,v docs/user/
mv docs/latex.txt,v docs/user/
mv docs/tools.txt,v docs/user/

mv docs/rst/ docs/user/

mv spec/howto/ docs/howto/  # for component-developers and core-developers

mv spec/pysource.dtd,v docs/dev/
mv spec/pysource.txt,v docs/dev/
mv spec/semantics.txt,v docs/dev/

mv spec/doctree.txt,v docs/ref/
mv spec/docutils.dtd,v docs/ref/
mv spec/soextblx.dtd,v docs/ref/

mv spec/transforms.txt,v docs/lib/

mv spec/rst/alternatives.txt,v docs/dev/rst/
mv spec/rst/introduction.txt,v docs/dev/rst/
mv spec/rst/problems.txt,v docs/dev/rst/

mv spec/rst/reStructuredText.txt,v docs/ref/rst/
mv spec/rst/directives.txt,v docs/ref/rst/
mv spec/rst/interpreted.txt,v docs/ref/rst/roles.txt

mv spec/pep-????.txt,v docs/peps/

echo Finished.
