TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RSTDIR = ../../../../bin
RST2DOC = $(RSTDIR)/prest
DOMWRT = $(RSTDIR)/dom.wrt
RSTCMD = $(RST2DOC) -D source="test data" -D trusted -D xformoff='.*' -w dom
HELPERDIR = ../../helpers
REDOM = $(HELPERDIR)/redom
DIFFREDIR = ../../../../helpers/bin
DIFFRE = $(DIFFREDIR)/diffre

RST_FLAG_perl08 = -D xformoff='(?!Pending).......$$'
RST_FLAG_include02 = -D includepath=includes
RST_FLAG_include03 = -D includepath="includes/:<.>:."
RST_FLAG_if04 = -D perl='$$a=0;$$b=1'
RST_FLAG_if05 = -D perl=4/0
RST_FLAG_perl09 = -D perl='$$a=0;$$b=1'
RST_FLAG_perl10 = -D perl=4/0
RST_FLAG_trusted01 = -D trusted=0

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.cmpdom %.redom

%.cmpdom:	%.mydom
	@mv $< $@; touch $@

%.cmpdom:	%.dom
	@mv $< $@

%.redom:        %.cmpdom $(REDOM)
	@perl $(REDOM) $< > $@

%.out:	%.rst $(RST2DOC)
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst

%.chk:  %.rst %.redom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm $(RSTDIR)/Directive/*.pm
	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | $(DIFFRE) $*.redom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'

#%.chk:	%.rst %.cmpdom $(RST2DOC) $(DOMWRT) $(RSTDIR)/*.pm
#	@$(RSTCMD) $(RST_FLAG_$(*)) $*.rst | diff $*.cmpdom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
