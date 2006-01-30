RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
WRT = $(RSTDIR)/latex.wrt
RSTCMD = $(RST2DOC) -w latex $(RSTCMDOPTS)
RSTCMDOPTS = -D generator=0 -D time=0 -D source_link=0 \
	-W documentclass_opts=times,10pt,twocolumn \
	-W caption=before \
	-W inputs=docsize
HELPERDIR = ../../helpers
RETEX = $(HELPERDIR)/retex
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre
PERL = $(shell perl -I ../../../../bin -e 'use TripConfig; print "$$TripConfig::SAFE_PERL\n"')

RST_FLAG_test = -W index

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.texc

#%.texc:	%.mytex $(RETEX)
#	@perl $(RETEX) $< > $@

%.texc:	%.cmptex $(RETEX)
	@perl $(RETEX) $< > $@

%.out:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.dom:	%.rst $(RST2DOC) $(RSTDIR)/dom.wrt $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) -w dom $*.rst

%.chk:	%.rst %.texc $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | $(DIFFRE) $*.texc - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'

%.tex:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst > $@

%.dbg:	%.rst $(RST2DOC) $(WRT) $(RSTDIR)/*.pm
	@$(PERL) -d $(RSTCMD) $(RST_FLAG_$(*)) $*.rst 

%.dvi:	%.tex
	latex $*.tex

%.ps:	%.dvi
	dvips -o $@ $*.dvi
