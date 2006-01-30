TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/xref.wrt
RSTCMD = $(RST2DOC) -w xref
HELPERDIR = ../../helpers
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
LOCALDIR = /usr/local/bin

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.dom:	%.rst $(RST2DOC) $(RSTDIR)/dom.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) -w dom $*.rst

%.chk:	%.rst %.xrefc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | diff $*.xrefc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@perl -d $(RSTCMD) $(RST_FLAG_$(*)) $*.rst


