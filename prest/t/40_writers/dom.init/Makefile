TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D align=0 -w dom -W nobackn
HELPERDIR = ../../helpers
REDOM = $(HELPERDIR)/redom
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
PERL = $(shell perl -I ../../../../bin -e 'use PrestConfig; print "$$PrestConfig::SAFE_PERL\n"')

rsts:	$(RST01s)

%01.rst:	test_%.py $(EXTRACT_TESTS)
	perl $(EXTRACT_TEST_FLAGS) $(EXTRACT_TESTS) $<

test:	$(CHKs)

.PRECIOUS:	%.domc

%.domc:	%.mydom
	@mv $< $@; touch $@

%.domc:	%.dom $(REDOM)
	@$(PERL) $(REDOM) $< > $@

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $*.rst

%.chk:	%.rst %.domc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $*.rst | $(DIFFRE) $*.domc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
