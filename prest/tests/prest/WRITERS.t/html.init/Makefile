TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/html.wrt
RSTCMD = $(RST2DOC) -D align=0 -D xformoff='Decorations' -W stylesheet=default.css -W target_tag=span
HELPERDIR = ../../helpers
REHTML = $(HELPERDIR)/rehtml
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
PERL = $(shell perl -I ../../../../bin -e 'use TripConfig; print "$$TripConfig::SAFE_PERL\n"')

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.htmlc

%.htmlc:	%.myhtml $(REHTML)
	@perl $(REHTML) $< > $@

%.htmlc:	%.html $(REHTML)
	@perl $(REHTML) $< > $@

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $*.rst

%.dom:	%.rst $(RST2DOC) $(RSTDIR)/dom.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w dom $*.rst

%.chk:	%.rst %.htmlc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst | $(DIFFRE) $*.htmlc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
