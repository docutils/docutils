# create docs
all: force_displacment.png methods_title_page.xsl method_of_least_squares.pdf  xmlformat

draft: force_displacment_draft.png methods_title_page.xsl method_of_least_squares.pdf  xmlformat


methods_title_page.xsl: methods_title_page.xml
	xsltproc make_title_page.xsl methods_title_page.xml > methods_title_page.xsl

method_of_least_squares.xml: method_of_least_squares.rst
	rst2xml.py --no-doctype --trim-footnote-reference-space method_of_least_squares.rst |rstxml2xml.py  > method_of_least_squares.xml

method_of_least_squares_docbook.xml: method_of_least_squares.xml
	xsltproc rst_xml_to_docbook.xsl method_of_least_squares.xml > method_of_least_squares_docbook.xml

method_of_least_squares_docbook.fo:method_of_least_squares_docbook.xml
	xsltproc docbook_fo.xsl method_of_least_squares_docbook.xml > method_of_least_squares_docbook.fo

method_of_least_squares.pdf: method_of_least_squares_docbook.fo
	fop -fo method_of_least_squares_docbook.fo -pdf method_of_least_squares.pdf

xmlformat:
	xmlformat.pl -i method_of_least_squares_docbook.xml
	xmlformat.pl -i method_of_least_squares.xml
	xmlformat.pl -i method_of_least_squares_docbook.fo

clean:
	rm -f method_of_least_squares.xml method_of_least_squares_docbook.xml method_of_least_squares_docbook.fo\
		method_of_least_squares.pdf methods_title_page.xsl force_displacment.png  force_displacment_draft.png

force_displacment_draft.png: make_graph.R
	Rscript make_graph.R --draft

force_displacment.png: make_graph.R
	Rscript make_graph.R 

test: method_of_least_squares.xml method_of_least_squares_docbook.xml method_of_least_squares_docbook.fo xmlformat
	 
validate: 
	validate_docbook.sh validate_docbook.sh method_of_least_squares_docbook.xml 
