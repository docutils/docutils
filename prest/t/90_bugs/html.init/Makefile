TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/html.wrt
RSTCMD = $(RST2DOC) -D align=0 -D trusted
HELPERDIR = ../../helpers
REHTML = $(HELPERDIR)/rehtml
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
LOCALDIR = /usr/local/bin
RST_TO_HTML = $(LOCALDIR)/rst-to-html.py
RST_TO_HTML_CMD = $(RST_TO_HTML) -g -t -s

RST_FLAG_method = -D xformoff='DocTitle'

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.htmlc

%.html:	%.rst $(RST_TO_HTML)
	$(RST_TO_HTML_CMD) $< > $@

%.htmlc:	%.myhtml $(REHTML)
	@perl $(REHTML) $< > $@

%.htmlc:	%.html $(REHTML)
	@perl $(REHTML) $< > $@

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.dom:	%.rst $(RST2DOC) $(RSTDIR)/dom.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w dom $*.rst

%.chk:	%.rst %.htmlc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | $(DIFFRE) $*.htmlc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@perl -d $(RSTCMD) $(RST_FLAG_$(*)) $*.rst


