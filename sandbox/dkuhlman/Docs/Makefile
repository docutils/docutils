
#
# Change the following so that they point to the locations
#   where your Python source and DocUtils are installed.
#
PYTHON_SRC = /w1/Python/Python-2.3b2
DOCUTILS_DIR = /w1/Python/DocUtils/docutils


DOCUTILSTOOLS_DIR = $(DOCUTILS_DIR)/tools
TRANSFORMDOCPY = $(DOCUTILSTOOLS_DIR)/python_latex.py \
	--documentclass=howto


HTMLFLAGS= --image-type png --html -s 1 --favicon ../icons/pyfav.gif

MKHOWTO = $(PYTHON_SRC)/Doc/tools/mkhowto

TARGETS = docpy_intro/docpy_intro.html

all:	html

html:	$(TARGETS)

docpy_intro/docpy_intro.html: docpy_intro.tex
	$(MKHOWTO) $(HTMLFLAGS) docpy_intro.tex

docpy_intro.tex: docpy_intro.rst
	$(TRANSFORMDOCPY) docpy_intro.rst > docpy_intro.tex

clean:
	rm -f *~ *.aux *.idx *.ilg *.ind *.log *.toc *.bkm *.syn \
		*.pla *.eps *.pdf *.ps *.lof *.l2h *.tex2 *.dvi
	rm -f docpy_intro.tex
	rm -f docpy_intro/*
	rmdir docpy_intro
	rm -f docpy_intro.tex


