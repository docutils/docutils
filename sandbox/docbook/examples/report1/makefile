all: title_page.xsl charts/problem_solve_2010_2011.png charts/receive_rate_adjusted.png\
	report.pdf

title_page.xsl: title_page.xml
	xsltproc make_title_page.xsl title_page.xml > title_page.xsl

charts/problem_solve_2010_2011.png: charts/problem_solve.R
	cd charts; Rscript problem_solve.r


charts/receive_rate_adjusted.png: charts/receive_rates_wit_adj.R
	cd charts; Rscript receive_rates_with_adj.R

report.xml: report.rst
	rst2xml.py --no-doctype --trim-footnote-reference-space --input-encoding=utf8 report.rst report.xml
	rstxml2xml.py -o tmp.xml report.xml 
	cat tmp.xml > report.xml
	rm tmp.xml
	xsltproc format_numbers.xsl report.xml > tmp
	cat tmp > report.xml
	rm tmp
	xsltproc long_tables.xsl report.xml > tmp
	cat tmp > report.xml
	rm tmp


report_docbook.xml: report.xml
	xsltproc --nonet rst_docbook.xsl report.xml > report_docbook.xml

report_docbook.fo: report_docbook.xml
	xsltproc docbook_fo.xsl report_docbook.xml > report_docbook.fo

report.pdf: report_docbook.fo
	fop -fo report_docbook.fo -pdf report.pdf

clean: 
	rm -f charts/*png report.xml report_docbook.xml report_docbook.fo report.pdf report_bw.pdf title_page.xsl\
	    charts/.Rhistory charts/.Rprofile
