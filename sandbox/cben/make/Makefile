# .. -*- Makefile -*-

# This makefile only builds documention for ``Makefile.docutils`` (which see).

DOCS = Makefile.docutils.txt README.txt
ALL = html

include Makefile.docutils

Makefile.docutils.txt: Makefile.docutils
	./make2rst.py $< > $@
