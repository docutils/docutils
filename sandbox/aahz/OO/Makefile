export PYTHON=python
export PYTHONPATH=$(HOME)/src/Lib-Python
MAIN=$(HOME)/write/effective/book

CHAPTER=Way
CHAPTER=Control
CHAPTER=Objects
CHAPTER=Data

SRCDIR=$(MAIN)/tmp/$(CHAPTER)
SRCDIR=$(MAIN)/Chapters/$(CHAPTER)
SRC=$(SRCDIR)/$(CHAPTER)

DESTDIR=$(MAIN)/Output/$(CHAPTER)
DESTDIR=$(MAIN)/Output/upload
DEST=$(DESTDIR)/$(CHAPTER).sxw

#DEST=$(MAIN)/Output/foo.xml

GRAPH_TYPE=pdf
GRAPH_TYPE=eps

test:
	#$(PYTHON) force_mkdir.py $(DESTDIR)
	$(PYTHON) graphics_output.py $(GRAPH_TYPE) $(SRCDIR) $(DESTDIR)
	$(PYTHON) open_office.py $(SRC) $(DEST)
	#$(PYTHON) publish.py $(SRC) > $(DEST)
