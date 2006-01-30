TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
DOMWRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D source="test data" -D xformoff='DocTitle|Messages|Decorations' -w dom -W nobackn
HELPERS = ../../helpers
REDOM = $(HELPERS)/redom
EXTRACT_TESTS = $(HELPERS)/extract_tests
PERL = $(shell perl -I ../../../../bin -e 'use TripConfig; print "$$TripConfig::SAFE_PERL\n"')
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre

include Flags.mak

# RST_FLAG_final_checks01 = -D xformoff='\.Messages|Decorations'
# RST_FLAG_footnotes04 = -D xformoff='\.References|Decorations'
# RST_FLAG_footnotes05 = -D xformoff='\.References|Decorations'
# RST_FLAG_footnotes08 = -D xformoff='DocTitle|Decorations'
# RST_FLAG_hyperlinks06 = -D xformoff='\.Messages|Decorations'
# RST_FLAG_hyperlinks11 = -D xformoff='\.Messages|Unreferenced|Decorations'
# RST_FLAG_hyperlinks22 = -D xformoff='\.Messages|Unreferenced|Decorations'
# RST_FLAG_messages01 = -D xformoff='DocTitle|Decorations'
# RST_FLAG_substitutions02 = -D xformoff='\.Messages|Decorations'

rsts:	$(RST01s)

%01.rst:	test_%.py $(EXTRACT_TESTS)
	perl $(EXTRACT_TEST_FLAGS) $(EXTRACT_TESTS) $<

Flags.mak:
	@touch $@

test:	$(CHKs)

.PRECIOUS:	%.cmpdom %.redom

%.cmpdom:	%.mydom
	@mv $< $@; touch $@

%.cmpdom:	%.dom
	@mv $< $@

%.redom:	%.cmpdom $(REDOM)
	@perl $(REDOM) $< > $@

%.out:	%.rst $(RST2DOC) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.dbg:	%.rst $(RST2DOC) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.chk:	%.rst %.redom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | $(DIFFRE) $*.redom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'

#%.chk:	%.rst %.cmpdom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
#	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | diff $*.cmpdom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
