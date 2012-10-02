# create docs
all: force_displacment.png methods_title_page.xsl method_of_least_squares.pdf  xmlformat

draft: force_displacment_draft.png methods_title_page.xsl method_of_least_squares.pdf  xmlformat

default: methods_title_page.xsl method_of_least_squares_default.pdf


methods_title_page.xsl: methods_title_page.xml
	xsltproc make_title_page.xsl methods_title_page.xml > methods_title_page.xsl

method_of_least_squares.xml: method_of_least_squares.rst
	rst2xml.py --no-doctype --trim-footnote-reference-space --input-encoding=utf8 method_of_least_squares.rst method_of_least_squares.xml
	rstxml2xml.py  -o tmp.xml method_of_least_squares.xml 
	cat tmp.xml > method_of_least_squares.xml

method_of_least_squares_default.xml: method_of_least_squares_clean.rst
	rst2xml.py --no-doctype --trim-footnote-reference-space method_of_least_squares_clean.rst |rstxml2xml.py  > method_of_least_squares_default.xml

method_of_least_squares_clean.rst:  method_of_least_squares.rst
	python make_default.py method_of_least_squares.rst > method_of_least_squares_clean.rst

method_of_least_squares_default_docbook.xml: method_of_least_squares_default.xml
	xsltproc rst_xml_to_docbook_default.xsl method_of_least_squares_default.xml > method_of_least_squares_default_docbook.xml

method_of_least_squares_docbook.xml: method_of_least_squares.xml
	xsltproc rst_xml_to_docbook.xsl method_of_least_squares.xml > method_of_least_squares_docbook.xml

method_of_least_squares_docbook.fo:method_of_least_squares_docbook.xml
	xsltproc docbook_fo.xsl method_of_least_squares_docbook.xml > method_of_least_squares_docbook.fo

method_of_least_squares_default_docbook.fo:method_of_least_squares_default_docbook.xml
	xsltproc docbook_default_fo.xsl method_of_least_squares_default_docbook.xml > method_of_least_squares_default_docbook.fo

method_of_least_squares.pdf: method_of_least_squares_docbook.fo
	fop -fo method_of_least_squares_docbook.fo -pdf method_of_least_squares.pdf

method_of_least_squares_default.pdf: method_of_least_squares_default_docbook.fo
	fop -fo method_of_least_squares_default_docbook.fo -pdf method_of_least_squares_default.pdf


xmlformat:
	xmlformat.pl -i method_of_least_squares_docbook.xml
	xmlformat.pl -i method_of_least_squares.xml
	xmlformat.pl -i method_of_least_squares_docbook.fo

clean:
	rm -f method_of_least_squares.xml method_of_least_squares_docbook.xml method_of_least_squares_docbook.fo\
		method_of_least_squares.pdf methods_title_page.xsl force_displacment.png  force_displacment_draft.png\
		method_of_least_squares_default.xml  method_of_least_squares_default_docbook.xml\
		method_of_least_squares_default_docbook.fo  method_of_least_squares_clean.rst\
		method_of_least_squares_default.pdf
	

force_displacment_draft.png: make_graph.R
	Rscript make_graph.R --draft

force_displacment.png: make_graph.R
	Rscript make_graph.R 

test: method_of_least_squares.xml method_of_least_squares_docbook.xml method_of_least_squares_docbook.fo xmlformat
	 
validate: 
	validate_docbook.sh validate_docbook.sh method_of_least_squares_docbook.xml 
