export PYTHON=python
export PYTHONPATH=$(HOME)/src/Lib-Python
MAIN=$(HOME)/write/effective/book
DEST=../test/foo.sxw

CHAPTER=Objects
CHAPTER=Way
#SRC=$(MAIN)/tmp/$(CHAPTER)/$(CHAPTER)
SRC=$(MAIN)/Chapters/$(CHAPTER)/$(CHAPTER)
DEST=$(MAIN)/Output/$(CHAPTER).sxw

#DEST=$(MAIN)/Output/foo.xml

test:
	$(PYTHON) open_office.py $(SRC) $(DEST)
	#$(PYTHON) publish.py $(SRC) > $(DEST)
