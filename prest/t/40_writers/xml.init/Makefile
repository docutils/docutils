TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D align=0 -w xml
HELPERDIR = ../../helpers
REXML = $(HELPERDIR)/rexml
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
PERL = $(shell perl -I ../../../../bin -e 'use PrestConfig; print "$$PrestConfig::SAFE_PERL\n"')

test:	$(CHKs)

.PRECIOUS:	%.xmlc

%.xmlc:	%.myxml $(REXML)
	@$(PERL) $(REXML) $< > $@

%.xmlc:	%.xml $(REXML)
	@$(PERL) $(REXML) $< > $@

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $*.rst

%.chk:	%.rst %.xmlc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst | $(DIFFRE) $*.xmlc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
