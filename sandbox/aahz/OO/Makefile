PYTHON=python
export PYTHONPATH=$(HOME)/src/Lib-Python
MAIN=$(HOME)/write/effective/book
DEST=../test/foo.xml
DEST=../test/foo.sxw
DEST=$(MAIN)/Output/Way.sxw
SRC=$(MAIN)/Chapters/Way/Way

test:
	$(PYTHON) open_office.py $(SRC) $(DEST)
	#$(PYTHON) publish.py $(SRC) > $(DEST)
