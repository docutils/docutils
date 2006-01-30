IDXRSTs = $(wildcard idxtest*.rst )
IDXHTMLs := $(IDXRSTs:.rst=.html)
RSTDIR = ../../../../bin
RSTCMD = $(RSTDIR)/prest
TOCs := $(wildcard toc*.toc)
TOCCHKs := $(TOCs:.toc=.chk)
toc01 = -W symbol='=' -W top_in_list
toc02 = -W symbol='=' -W exclude_top 
toc03 = -W symbol='='
toc04 = 
toc05 = -W top_in_list
toc06 = -W exclude_top -W depth=2
toc07 = -W top_in_list=0
toc08 = -W exclude_top
toc09 = -W top_in_list -W depth=1
toc10 = -W symbol="=" -W top_in_list -W include_noheader
toc11 = -W top_in_list -W include_noheader
toc12 = -W symbol="=" -W include_noheader

test:	xreftest.chk index.chk $(TOCCHKs)

index.html: $(IDXHTMLs) $(RST2DOC) $(RSTDIR)/html.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(IDXRSTs) -w index | $(RSTCMD) - > $@

# Generate *.html, *.dom, index (*_idx.rst) and TOC (*_toc.rst)
# for single rst source file.
%.htm %.html:	%.rst $(RST2DOC) $(RSTDIR)/html.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst > $@

%.dom:	%.rst $(RST2DOC) $(RSTDIR)/dom.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w dom $*.rst > $@

%_xref.rst: %.rst $(RST2DOC) $(RSTDIR)/xref.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w xref $*.rst > $@

%_idx.rst: %.rst $(RST2DOC) $(RSTDIR)/index.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w index $*.rst > $@

%_toc.rst: %.rst $(RST2DOC) $(RSTDIR)/toc.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w toc $*.rst > $@

index.chk: $(IDXRSTs) index.idx $(RST2DOC) $(RSTDIR)/index.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(IDXRSTs) -w index -W short_titles=0 | diff index.idx - | tee $@ | perl -e '@IN = <>; print "index: ",@IN==0 ? "OK" : "FAIL","\n"'

index.out: $(IDXRSTs) index.idx $(RST2DOC) $(RSTDIR)/index.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(IDXRSTs) -w index -W short_titles=0

xreftest.chk:	xreftest.rst xreftest.xref $(RST2DOC) $(XREFWRT) $(RSTDIR)/*.pm $(RSTDIR)/xref.wrt
	@$(RSTCMD) -w xref -W file_suffix=htm xreftest.rst | diff xreftest.xref - | tee $@ | perl -e '@IN = <>; print "xreftest: ",@IN==0 ? "OK" : "FAIL","\n"'

xreftest.out:	xreftest.rst xreftest.xref $(RST2DOC) $(XREFWRT) $(RSTDIR)/*.pm $(RSTDIR)/xref.wrt
	@$(RSTCMD) -w xref -W file_suffix=htm xreftest.rst

toc%.chk: $(IDXRSTs) toc%.toc $(RST2DOC) $(RSTDIR)/toc.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(IDXRSTs) -w toc $(toc$*) | diff toc$*.toc - | tee $@ | perl -e '@IN = <>; print "toc$*: ",@IN==0 ? "OK" : "FAIL","\n"'

toc%.out: $(IDXRSTs) toc%.toc $(RST2DOC) $(RSTDIR)/toc.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(IDXRSTs) -w toc $(toc$*)

clean:	
	rm -rf *.html *.dom *.chk *_idx.rst *_toc.rst *_xref.rst
