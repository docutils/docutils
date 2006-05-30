TEST_PYs := $(wildcard test_*.py)
RST01s := $(patsubst test_%.py,%01.rst,$(TEST_PYs))
RSTs := $(wildcard *.rst)
CHKs := $(RSTs:.rst=.chk)
RST_DIR = ../../../../bin
RST2DOC = $(RST_DIR)/prest
DOMWRT = $(RST_DIR)/dom.wrt
RSTCMD = $(RST2DOC) -w dom -W nobackn=1 -D xformoff='Decorations|Unreferenced' $*.rst
RSTCMD_FLAGS_autofootnote = -D xformoff='Decorations|Unreferenced|CitationReferences|DocTitle|SectionSubTitle'
RSTCMD_FLAGS_badinline = -D nestinline
RSTCMD_FLAGS_fakerefinref = -D xformoff='.*'
RSTCMD_FLAGS_litinref = -D xformoff='.*'
RSTCMD_FLAGS_litref = -D xformoff='.*'
RSTCMD_FLAGS_defotherlist = -D xformoff='.*'
RSTCMD_FLAGS_table = -D align=0

rsts:	$(RST01s)

%01.rst:	test_%.py
	extract_tests $<

test:	$(CHKs)

.PRECIOUS:	%.cmpdom

%.cmpdom:	%.mydom
	@mv $< $@; touch $@

%.cmpdom:	%.dom
	@mv $< $@

%.out:	%.rst $(RST2DOC)
	@$(RSTCMD) $(RSTCMD_FLAGS_$(notdir $*))

%.chk:	%.rst %.cmpdom $(RST2DOC) $(DOMWRT) $(RST_DIR)/*.pm
	@$(RSTCMD) $(RSTCMD_FLAGS_$(notdir $*)) | diff $*.cmpdom - | tee $@ | perl -e '@IN = <>; print "$*: ",@IN==0 ? "OK" : "FAIL","\n"'
