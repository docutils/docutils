# .. -*- Makefile -*-

# This makefile pertains build documention about ``Makefile.docutils`` only.

DOCS = Makefile.docutils.txt README.txt
ALL = html

include Makefile.docutils

Makefile.docutils.txt: Makefile.docutils
	./make2rst.py $< > $@
