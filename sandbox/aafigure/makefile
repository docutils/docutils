# this makefile is for my convenience beacuse my editor has a handy shortcut to
# run make

# clean up generated images, but only when the text file has been changed

README.html: README.txt *.py
	rm -f aafigure-*.svg benford.svg
	python rst2html.py --traceback README.txt README.html
