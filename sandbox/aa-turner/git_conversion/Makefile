# Makefile for docutils conversion using reposurgeon
#
# Steps to using this:
# 1. Make sure reposurgeon and repotool are on your $PATH.
# 2. Run 'make' to build a converted repository.

.PHONY: convert
convert: docutils-git prest-git sandbox-git web-git
# Install reposurgeon
# ===========================

# requires reposurgeon 4.38; this is the latest version in Ubuntu 24.04
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

# Get local copies of the pre-CVS release archives
# ================================================

dps-0.1.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/docstring/dps/0.1/dps-0.1.tar.gz
dps.0.2.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/docstring/dps/0.2/dps.0.2.tar.gz
dps.0.3.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/docstring/dps/0.3/dps.0.3.tar.gz
rst-0.1.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/structuredtext/restructuredtext/0.1/rst-0.1.tar.gz
rst.0.2.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/structuredtext/restructuredtext/0.2/rst.0.2.tar.gz
rst.0.3.tar.gz:
	wget --no-verbose https://master.dl.sourceforge.net/project/structuredtext/restructuredtext/0.3/rst.0.3.tar.gz
# no need for 0.4 archives, as CVS history begins at 0.3
# wget --no-verbose https://master.dl.sourceforge.net/project/docstring/dps/0.4/dps-0.4.tar.gz
# wget --no-verbose https://master.dl.sourceforge.net/project/structuredtext/restructuredtext/0.4/restructuredtext-0.4.tar.gz

.PHONY: repack_archives
repack_archives: dps-0.1.tar.gz dps.0.2.tar.gz dps.0.3.tar.gz rst-0.1.tar.gz rst.0.2.tar.gz rst.0.3.tar.gz
	# due to inscrutable tar errors, we repack every archive
	mkdir -p tmp_repack
	cd tmp_repack && tar -xzf ../dps-0.1.tar.gz --one-top-level=dps-0.1 --strip-components=0
	cd tmp_repack && tar -xzf ../dps.0.2.tar.gz --one-top-level=dps-0.2 --strip-components=1
	cd tmp_repack && tar -xzf ../dps.0.3.tar.gz --one-top-level=dps-0.3 --strip-components=1
	cd tmp_repack && tar -xzf ../rst-0.1.tar.gz --one-top-level=rst-0.1 --strip-components=0
	cd tmp_repack && tar -xzf ../rst.0.2.tar.gz --one-top-level=rst-0.2 --strip-components=1
	cd tmp_repack && tar -xzf ../rst.0.3.tar.gz --one-top-level=rst-0.3 --strip-components=1

	cd tmp_repack && tar -cf docstring-0.1.tar dps-0.1
	cd tmp_repack && tar -cf docstring-0.2.tar dps-0.2
	cd tmp_repack && tar -cf docstring-0.3.tar dps-0.3
	cd tmp_repack && tar -cf structuredtext-0.1.tar rst-0.1
	cd tmp_repack && tar -cf structuredtext-0.2.tar rst-0.2
	cd tmp_repack && tar -cf structuredtext-0.3.tar rst-0.3
	mv tmp_repack/*.tar ./
	rm -rf tmp_repack

# Dump streams for reposurgeon
# ============================

# Dump reposurgeon streams (done automatically during conversion)
.PHONY: dump-streams
dump-streams: docutils.svn docstring.fi structuredtext.fi

docutils.svn: docutils-mirror
	repotool export -d docutils-mirror > docutils.svn

docstring.fi: docstring-mirror
	repotool export -d docstring-mirror > docstring.fi

structuredtext.fi: structuredtext-mirror
	repotool export -d structuredtext-mirror > structuredtext.fi

# Create the git repositories
# ===========================

# Also outputs docutils_early.fi
docutils_late.fi: common_load.lift docutils.svn
	reposurgeon 'script common_load.lift'

docutils_tmp_early: docutils_late.fi
	reposurgeon 'read <docutils_early.fi' 'prefer git' 'write docutils_tmp_early/'

docutils_tmp_late: docutils_late.fi
	reposurgeon 'read <docutils_late.fi' 'prefer git' 'write docutils_tmp_late/'

cvs_docstring.fi: common_cvs_docstring.lift docstring.fi
	reposurgeon 'script common_cvs_docstring.lift'

cvs_tmp_docstring: cvs_docstring.fi
	reposurgeon 'read <cvs_docstring.fi' 'prefer git' 'write cvs_tmp_docstring/'

cvs_structuredtext.fi: common_cvs_structuredtext.lift structuredtext.fi
	reposurgeon 'script common_cvs_structuredtext.lift'

cvs_tmp_structuredtext: cvs_structuredtext.fi
	reposurgeon 'read <cvs_structuredtext.fi' 'prefer git' 'write cvs_tmp_structuredtext/'

tmp_combined: common_combine.sh docutils_tmp_early cvs_tmp_docstring cvs_tmp_structuredtext
	./common_combine.sh

tmp_combined.fi: tmp_combined
	repotool export -d tmp_combined > tmp_combined.fi

common.fi: common_clean.lift tmp_combined.fi docutils_late.fi
	reposurgeon 'script common_clean.lift' 'prefer git' 'rebuild common-git'

common-git: common.fi post-convert.sh
	./post-convert.sh common-git

docutils-git: docutils.lift common.fi post-convert.sh
	reposurgeon 'script docutils.lift' 'rebuild docutils-git'
	./post-convert.sh docutils-git

prest-git: prest.lift common.fi post-convert.sh
	reposurgeon 'script prest.lift' 'rebuild prest-git'
	./post-convert.sh prest-git

sandbox-git: sandbox.lift common.fi post-convert.sh
	reposurgeon 'script sandbox.lift' 'rebuild sandbox-git'
	./post-convert.sh sandbox-git

web-git: web.lift common.fi post-convert.sh
	reposurgeon 'script web.lift' 'rebuild web-git'
	./post-convert.sh web-git

# Comparison commands
# ===================

# Make a local checkout of the source mirror for inspection
docutils-svn-export:
	# --ignore-keywords keeps RCS keywords ("$Date$" etc) unchanged
	svn export --ignore-keywords https://svn.code.sf.net/p/docutils/code/trunk docutils-svn-export

# Compare the histories of the unconverted and converted repositories at HEAD.
.PHONY: compare
compare: docutils-svn-export docutils-git
	repotool compare docutils-svn-export docutils-git

# Bundle the conversion files
# ===========================

SOURCES = Makefile Justfile docutils.map common_load.lift common_cvs_docstring.lift common_cvs_structuredtext.lift common_combine.sh common_clean.lift docutils.lift prest.lift sandbox.lift web.lift post-convert.sh
docutils-conversion.tar.gz: $(SOURCES)
	tar --dereference --transform 's:^:docutils-conversion/:' -czvf docutils-conversion.tar.gz $(SOURCES)

.PHONY: dist
dist: docutils-conversion.tar.gz

# Publish the git repositories
# ============================

.PHONY: push-remote
push-remote:
	# Docutils
	cd docutils-git; git remote add origin https://github.com/docutils-conversion/docutils.git
	cd docutils-git; git push -u origin --tags
	# Prest
	cd prest-git; git remote add origin https://github.com/docutils-conversion/prest.git
	cd prest-git; git push -u origin --tags
	# Sandbox
	cd sandbox-git; git remote add origin https://github.com/docutils-conversion/sandbox.git
	cd sandbox-git; git push -u origin --tags
	# Web
	cd web-git; git remote add origin https://github.com/docutils-conversion/web.git
	cd web-git; git push -u origin --tags
