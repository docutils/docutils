TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
DOMWRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D source="test data" -D xformoff='.*' -D align=0 -w dom -W nobackn
HELPERS = ../../helpers
EXTRACT_TESTS = $(HELPERS)/extract_tests
PERL = $(shell perl -I ../../../../bin -e 'use TripConfig; print "$$TripConfig::SAFE_PERL\n"')

include Flags.mak

rsts:	$(RST01s)

%01.rst:	test_%.py $(EXTRACT_TESTS)
	perl $(EXTRACT_TEST_FLAGS) $(EXTRACT_TESTS) $<

Flags.mak:
	@touch $@

test:	$(CHKs)

.PRECIOUS:	%.cmpdom

%.cmpdom:	%.mydom
	@mv $< $@; touch $@

%.cmpdom:	%.dom
	@mv $< $@

%.out:	%.rst $(RST2DOC) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.dbg:	%.rst $(RST2DOC) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.chk:	%.rst %.cmpdom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | diff $*.cmpdom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
