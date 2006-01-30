TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
DOMWRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D trusted -w dom -D source="test data" -D xformoff='.*'

RST_FLAG_keeptitle = -D xformoff='Decoration' -D keep_title_section
RST_FLAG_nestinline = -D nestinline
RST_FLAG_tabs2 = -D tabstops=2

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.cmpdom

%.cmpdom:	%.mydom
	@mv $< $@; touch $@

%.cmpdom:	%.dom
	@mv $< $@

%.out:	%.rst $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.chk:	%.rst %.cmpdom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | diff $*.cmpdom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
