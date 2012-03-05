all: title_page.xsl charts/problem_solve_2010_2011.png charts/receive_rate_adjusted.png\
	report.pdf

draft:title_page.xsl charts/receive_rate_adjusted_draft.png charts/problem_solve_2010_2011_draft.png report_draft\
	report.pdf

bw: title_page.xsl charts/receive_rate_adjusted_bw.png charts/problem_solve_2010_2011_bw.png\
	report_bw.pdf

bwdraft: title_page.xsl charts/receive_rate_adjusted_bw_draft.png charts/problem_solve_2010_2011_bw_draft.png\
	report_draft report_bw.pdf


title_page.xsl: title_page.xml
	xsltproc make_title_page.xsl title_page.xml > title_page.xsl

charts/problem_solve_2010_2011.png: charts/problem_solve.R
	cd charts; Rscript problem_solve.r

charts/problem_solve_2010_2011_draft.png: charts/problem_solve.R
	cd charts; Rscript problem_solve.r --draft

charts/problem_solve_2010_2011_bw.png: charts/problem_solve.R
	cd charts; Rscript problem_solve.r --bw;\
	cp problem_solve_2010_2011.png  problem_solve_2010_2011_bw.png 

charts/problem_solve_2010_2011_bw_draft.png: charts/problem_solve.R
	cd charts; Rscript problem_solve.r --bw --draft;\
	cp problem_solve_2010_2011_draft.png  problem_solve_2010_2011_bw_draft.png 

charts/receive_rate_adjusted.png: charts/problem_solve.R
	cd charts; Rscript receive_rates_with_adj.R

charts/receive_rate_adjusted_draft.png: charts/problem_solve.R
	cd charts; Rscript receive_rates_with_adj.R --draft

charts/receive_rate_adjusted_bw.png: charts/problem_solve.R
	cd charts; Rscript receive_rates_with_adj.R --bw;\
	cp  receive_rate_adjusted.png receive_rate_adjusted_bw.png 

charts/receive_rate_adjusted_bw_draft.png: charts/problem_solve.R
	cd charts; Rscript receive_rates_with_adj.R --bw --draft;\
	cp  receive_rate_adjusted_draft.png receive_rate_adjusted_bw_draft.png 

report.xml: report.rst
	rst2xml.py --no-doctype --trim-footnote-reference-space report.rst |rstxml2xml.py  > report.xml
	xsltproc format_numbers.xsl report.xml > tmp
	cat tmp > report.xml
	rm tmp
	xsltproc long_tables.xsl report.xml > tmp
	cat tmp > report.xml
	rm tmp


report_docbook.xml: report.xml
	xsltproc rst_docbook.xsl report.xml > report_docbook.xml

report_draft: report.xml
	xsltproc rst_docbook_draft.xsl report.xml > report_docbook.xml

report_docbook.fo: report_docbook.xml
	xsltproc docbook_fo.xsl report_docbook.xml > report_docbook.fo

report.pdf: report_docbook.fo
	fop -fo report_docbook.fo -pdf report.pdf

report_bw.pdf: report_docbook.fo
	fop -fo report_docbook.fo -pdf report_bw.pdf

clean: 
	rm -f charts/*png report.xml report_docbook.xml report_docbook.fo report.pdf report_bw.pdf title_page.xsl
