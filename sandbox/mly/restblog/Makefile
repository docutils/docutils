DOCUTILTOOLPATH = /home/mly/palanga/python/docutils/docutils/tools/
HTML = $(DOCUTILTOOLPATH)html.py
MAIN_INDEX = ./main_index.py
DATE_INDEX = ./date_index.py
SUBJ_INDEX = ./subj_index.py
KEYW_INDEX = ./keyw_index.py

.SUFFIXES: .html .txt

blogs := $(patsubst %.txt,%.html,$(wildcard *.txt))

all: $(blogs) index.html date_index.html subject_index.html keyword_index.html
	
index.html: $(blogs)
	$(MAIN_INDEX)

date_index.html: $(blogs)
	$(DATE_INDEX)
	
subject_index.html: $(blogs)
	$(SUBJ_INDEX)
	
keyword_index.html: $(blogs)
	$(KEYW_INDEX)

# --trim-footnote-reference-space ersätter --snap... och funkar även i
#  HTML

.txt.html: 
	$(HTML) -len -o iso-8859-1 $< > $@

install:
	scp *.html default.css thinkwar@thinkware.se:/home/t/thinkwar/www/home/magnus/blog/

clean:
	rm *~ *.html 
