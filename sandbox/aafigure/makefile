# this makefile is for my convenience beacuse my editor has a handy shortcut to
# run make

# aafigure must have been installed before running this makefile.

# clean up generated images, but only when the text file has been changed

README.html: README.txt *.py aafigure/*.py
	rm -f aafigure-*.svg benford.svg
	rst2html.py --traceback README.txt README.html
