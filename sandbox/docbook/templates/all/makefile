RST_COMMAND=rst2xml.py --no-doctype --trim-footnote-reference-space --input-encoding utf8 
# can add  --no-doc-title to above command 
all: force_displacment.png title_page.xsl document.pdf  xmlformat

force_displacment.png: make_graph.R
	Rscript make_graph.R

title_page.xsl: title_page.xml
	xsltproc make_title_page.xsl title_page.xml > title_page.xsl

document.xml: document.rst
	$(RST_COMMAND) document.rst  document.xml
	rstxml2xml.py -o tmp.xml document.xml 
	cat tmp.xml > document.xml
	rm tmp.xml
	xsltproc format_numbers.xsl document.xml > tmp.xml
	cat tmp.xml > document.xml
	rm tmp.xml
	xsltproc long_tables.xsl document.xml > tmp.xml
	cat tmp.xml > document.xml
	rm tmp.xml


document_docbook.xml: document.xml
	xsltproc rst_docbook.xsl document.xml > document_docbook.xml

document_docbook.fo: document_docbook.xml
	xsltproc docbook_fo.xsl document_docbook.xml > document_docbook.fo


document.pdf: document_docbook.fo
	fop -fo document_docbook.fo -pdf document.pdf

xmlformat:
	xmlformat.pl -i document_docbook.xml
	xmlformat.pl -i document.xml
	xmlformat.pl -i document_docbook.fo

clean: 
	rm -f document.xml document_docbook.xml document_docbook.fo document.pdf \
		*png
