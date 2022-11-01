# Makefile for docutils conversion using reposurgeon
#
# Steps to using this:
# 1. Make sure reposurgeon and repotool are on your $PATH.
# 2. Run 'make' to build a converted repository.

.PHONY: convert
convert: docutils-git prest-git sandbox-git web-git

# Install reposurgeon
# ===========================

# requires reposurgeon 4.31; this is the latest version in Ubuntu 22.04
.PHONY: reposurgeon-install
reposurgeon-install:
	sudo apt install --yes make rsync cvs-fast-export subversion cvs reposurgeon

# Get local copies of the SVN and CVS sources
# ===========================================

# rsync short flags are -ahPvi
docutils-mirror:
	rsync --archive --human-readable --partial --progress --verbose --itemize-changes svn.code.sf.net::p/docutils/code/* docutils-mirror

docstring-mirror:
	rsync --archive --human-readable --partial --progress --verbose --itemize-changes a.cvs.sourceforge.net::cvsroot/docstring/ docstring-mirror

structuredtext-mirror:
	rsync --archive --human-readable --partial --progress --verbose --itemize-changes a.cvs.sourceforge.net::cvsroot/structuredtext/ structuredtext-mirror

# Dump streams for reposurgeon
# ============================

docutils.svn: docutils-mirror
	repotool export -d docutils-mirror > docutils.svn

docstring.fi: docstring-mirror
	repotool export -d docstring-mirror > docstring.fi

structuredtext.fi: structuredtext-mirror
	repotool export -d structuredtext-mirror > structuredtext.fi

# Create the git repositories
# ===========================

common-git: docutils.svn docstring.fi structuredtext.fi
	# prepare the two
	reposurgeon 'script common_cvs.lift'
	reposurgeon 'script common_load.lift'
	mkdir -p docutils_tmp_early
	reposurgeon 'read <docutils_early.fi' 'prefer git' 'write docutils_tmp_early'
	mkdir -p docutils_tmp_late
	reposurgeon 'read <docutils_late.fi' 'prefer git' 'write docutils_tmp_late'
	./common_combine.sh
	# TODO replace symlinks with copies
	reposurgeon 'script common_clean.lift' 'write >common.fi' 'rebuild common-git'

docutils-git: common-git docutils.lift
	reposurgeon 'script docutils.lift' 'rebuild docutils-git'
	./post-convert.sh docutils-git

prest-git: common-git prest.lift
	reposurgeon 'script prest.lift' 'rebuild prest-git'
	./post-convert.sh prest-git

sandbox-git: common-git sandbox.lift
	reposurgeon 'script sandbox.lift' 'rebuild sandbox-git'
	./post-convert.sh sandbox-git

web-git: common-git web.lift
	reposurgeon 'script web.lift' 'rebuild web-git'
	./post-convert.sh web-git

# Comparison commands
# ===================

# Make a local checkout of the source mirror for inspection
docutils-svn-export:
	svn export --ignore-keywords https://svn.code.sf.net/p/docutils/code/trunk docutils-svn-export

# Compare the histories of the unconverted and converted repositories at HEAD.
.PHONY: compare
compare: docutils-svn-export docutils-git 
	repotool compare docutils-svn-export docutils-git

# Bundle the conversion files
# ===========================

SOURCES = Makefile docutils.map common.lift cvs.lift docutils.lift prest.lift sandbox.lift web.lift post-convert.sh
docutils-conversion.tar.gz: $(SOURCES)
	tar --dereference --transform 's:^:docutils-conversion/:' -czvf docutils-conversion.tar.gz $(SOURCES)

.PHONY: dist
dist: docutils-conversion.tar.gz
