# Makefile to process a file LaTeX with the Python LaTeX system.
#  Optionally processes reStructuredText files.

#
# Change the following to point to your Python source code.
#
PYTHON_SRC = /w1/Python/Python-2.3b2
DOCUTILS_DIR = /w1/Python/DocUtils/docutils

#
# Change the following to point to your Docutils installation.
#
DOCUTILSTOOLS_DIR = $(DOCUTILS_DIR)/tools
TRANSFORMDOCPY = $(DOCUTILSTOOLS_DIR)/python_latex.py \
	--documentclass=howto

#
# Change these flags for your purposes.
#
#HTMLFLAGS= --image-type png --html -s 1 --favicon ../icons/pyfav.gif
HTMLFLAGS= --iconserver ../icons --html -s 1 --favicon ../icons/pyfav.gif

MKHOWTO = $(PYTHON_SRC)/Doc/tools/mkhowto

PAPER = a4

#
# Change this to the name of your source file (without extension).
#
#SRC_NAME=zip-ls
ifndef SRC_NAME
$(error Must define SRC_NAME (no extension), e.g. make SRC_NAME=mysourcefile)
endif

TARGETS = $(SRC_NAME)/$(SRC_NAME).html

FIGURES = 

all:	html

html:	$(TARGETS) $(FIGURES)

$(SRC_NAME)/$(SRC_NAME).html: $(SRC_NAME).tex
	$(MKHOWTO) $(HTMLFLAGS) $(SRC_NAME).tex

#
# Remove or Comment out the following lines if your source file
#   is a LaTeX file not a reST file.
#
$(SRC_NAME).tex: $(SRC_NAME).txt
	$(TRANSFORMDOCPY) $(SRC_NAME).txt > $(SRC_NAME).tex

clean:
	rm -f *~ *.aux *.idx *.ilg *.ind *.log *.toc *.bkm *.syn \
*.pla *.eps *.pdf *.ps *.lof *.l2h *.tex2 *.dvi
	-rm -f $(SRC_NAME).tex
	-rm -f $(SRC_NAME)/*
	-rmdir $(SRC_NAME)
	-rm -f $(SRC_NAME).tex


