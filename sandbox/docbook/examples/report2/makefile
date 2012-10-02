all: charts/indirect_out_vs_outbound_percent.png charts/direct_vs_total_outbound_arrows.png\
	charts/foot_vs_non_foot_rate_2011.png charts/inbound_rate_2010_2011.png\
	charts/inbound_rate_2011.png charts/inbound_cost.png\
	charts/indirect_out_vs_total_2010_2011.png charts/direct_vs_total_2010_2011.png\
	charts/direct_vs_inbound_total_2010_2011.png charts/indirect_inbound_2010_2011.png\
	charts/multis_rate_2010_2011.png charts/outbound_rate_2010_2011.png\
	charts/outbound_rate_2011.png charts/picking_rate_2010_2011.png\
	charts/projected_rate_receive.png charts/projected_rate_returns.png\
	charts/projected_rate_static_pick.png charts/problem_solve_2010_2011.png\
	charts/putaway_rate_2010_2011.png charts/receive_rate_2010_2011.png\
	charts/receive_direct_indirect.png charts/return_rate_2010_2011.png\
	charts/return_percent_indirect.png charts/return_rate_2011_2.png\
	charts/shipping_rate_2010_2011.png charts/shipping_rate_2011.png\
	charts/singles_rate_2010_2011.png charts/static_picking_2010_2011.png\
	charts/static_picking_2010_2011_adj2.png charts/static_picking_2010_2011_adj4.png\
    	charts/static_picking_2010_2011_adj5.png charts/static_vs_other.png\
	charts/total_costs.png charts/units_hours_2011.png \
 	charts/units_hours_2010.png charts/outbound_costs.png\
	charts/outbound_rate_2011_all.png charts/direct_vs_total_outbound2_arrows.png\
	charts/projected_exceptions_vol.png\
	title_page.xsl report.xml report_docbook.xml report_docbook.fo\
	report.pdf


charts/indirect_out_vs_outbound_percent.png: charts/direct_outbound.R
	cd charts; Rscript direct_outbound.R

charts/direct_vs_total_outbound_arrows.png: charts/direct_vs_total_arrows.R
	cd charts; Rscript direct_vs_total_arrows.R

charts/foot_vs_non_foot_rate_2011.png: charts/footwear_vs_nonfootwear_2011.R
	cd charts; Rscript footwear_vs_nonfootwear_2011.R

charts/inbound_rate_2010_2011.png:charts/inbound_2010_2011.R 
	cd charts; Rscript inbound_2010_2011.R

charts/inbound_rate_2011.png:charts/inbound_2011.R 
	cd charts; Rscript inbound_2011.R

charts/inbound_cost.png units_hours_inbound_2010.png charts/units_hours_inbound_2011.png:charts/inbound_costs.R 
	cd charts; Rscript inbound_costs.R

charts/indirect_out_vs_total_2010_2011.png charts/direct_vs_total_2010_2011.png: charts/indirect_2010_vs_2011.R
	cd charts; Rscript indirect_2010_vs_2011.R

charts/direct_vs_inbound_total_2010_2011.png charts/indirect_inbound_2010_2011.png:\
    charts/indirect_inbound_2010_vs_2011.R
	cd charts; Rscript indirect_inbound_2010_vs_2011.R

charts/multis_rate_2010_2011.png: charts/multis_2010_2011.R
	cd charts; Rscript multis_2010_2011.R

charts/outbound_rate_2010_2011.png:charts/outbound_2010_2011.R
	cd charts; Rscript outbound_2010_2011.R

charts/outbound_rate_2011.png: charts/outbound_2011.R
	cd charts; Rscript outbound_2011.R

charts/picking_rate_2010_2011.png:charts/picking_2010_2011.R
	cd charts; Rscript picking_2010_2011.R

charts/projected_rate_receive.png:charts/predicted_costs_receiving.R
	cd charts; Rscript predicted_costs_receiving.R

charts/projected_rate_returns.png:charts/predicted_costs_returns.R
	cd charts; Rscript predicted_costs_returns.R

charts/projected_rate_static_pick.png: charts/predicted_costs_static_pick.R
	cd charts; Rscript predicted_costs_static_pick.R

charts/problem_solve_2010_2011.png:charts/problem_solve_2010_vs_2011.R
	cd charts; Rscript problem_solve_2010_vs_2011.R

charts/putaway_rate_2010_2011.png:charts/putaway_2010_2011.R
	cd charts; Rscript putaway_2010_2011.R

charts/receive_rate_2010_2011.png charts/receive_rate_indirect_total_2010_2011.png: charts/receive_2010_2011.R
	cd charts; Rscript receive_2010_2011.R

charts/receive_direct_indirect.png:charts/receive_direct_indirect.r
	cd charts; Rscript receive_direct_indirect.r

charts/return_rate_2010_2011.png:charts/return_2010_2011.R
	cd charts; Rscript return_2010_2011.R

charts/return_percent_indirect.png:charts/return_direct_indirect.R
	cd charts; Rscript return_direct_indirect.R

charts/return_rate_2011_2.png:charts/return_rate_2011.R
	cd charts; Rscript return_rate_2011.R

charts/shipping_rate_2010_2011.png:charts/shipping_2010_2011.R
	cd charts; Rscript shipping_2010_2011.R

charts/shipping_rate_2011.png:charts/shipping_2011.R
	cd charts; Rscript shipping_2011.R

charts/singles_rate_2010_2011.png:charts/singles_2010_2011.R
	cd charts; Rscript singles_2010_2011.R

charts/static_picking_2010_2011.png:charts/static_picking.R
	cd charts; Rscript static_picking.R

charts/static_picking_2010_2011_adj2.png charts/static_picking_2010_2011_adj4.png\
    charts/static_picking_2010_2011_adj5.png:charts/static_picking_adj.R
	cd charts; Rscript static_picking_adj.R

charts/static_vs_other.png:charts/static_picking_compared.R
	cd charts; Rscript static_picking_compared.R

charts/total_costs.png:charts/total_costs_bar.R
	cd charts; Rscript total_costs_bar.R

charts/units_hours_2011.png charts/units_hours_2010.png charts/outbound_costs.png:\
	charts/units_vs_hours_2010_2011.R
	cd charts; Rscript units_vs_hours_2010_2011.R

charts/outbound_rate_2011_all.png:charts/outbound_all.R
	cd charts; Rscript outbound_all.R

charts/direct_vs_total_outbound2_arrows.png:charts/direct_vs_total_arrows.R
	cd charts; Rscript direct_vs_total_arrows.R

charts/projected_exceptions_vol.png:charts/predicted_vol_ex_handling.R
	cd charts; Rscript predicted_vol_ex_handling.R

# END OF PICTURES

title_page.xsl: title_page.xml
	xsltproc make_title_page.xsl title_page.xml > title_page.xsl

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
	xsltproc rst_docbook.xsl report.xml > report_docbook.xml

report_docbook.fo: report_docbook.xml
	xsltproc docbook_fo.xsl report_docbook.xml > report_docbook.fo

report.pdf: report_docbook.fo
	fop -fo report_docbook.fo -pdf report.pdf

clean:
	rm -f charts/*png title_page.xsl report.xml report_docbook.xml report_docbook.fo report.pdf charts/.Rprofile
