# -*-makefile-*-
# Note: Since this makefile is checking three writers, it's doesn't
# fit cleanly in the Common.mk framework.

# Tests for the idx writer
IDXRSTs := $(wildcard idxtest*.rst)
TOCs    := $(wildcard toc*.toc)
TOCCHKs = $(TOCs:.toc=.chk)

CHKs	:= index.chk $(TOCCHKs) xreftest.chk 

peek:
	@echo $(CHKs)

RST_FLAG_toc01 = -w toc -W symbol='=' -W top_in_list
RST_FLAG_toc02 = -w toc -W symbol='=' -W exclude_top
RST_FLAG_toc03 = -w toc -W symbol='='
RST_FLAG_toc04 =
RST_FLAG_toc05 = -w toc -W top_in_list
RST_FLAG_toc06 = -w toc -W exclude_top -W depth=2
RST_FLAG_toc07 = -w toc -W top_in_list=0
RST_FLAG_toc08 = -w toc -W exclude_top
RST_FLAG_toc09 = -w toc -W top_in_list -W depth=1
RST_FLAG_toc10 = -w toc -W symbol="=" -W top_in_list -W include_noheader
RST_FLAG_toc11 = -w toc -W top_in_list -W include_noheader
RST_FLAG_toc12 = -w toc -W symbol="=" -W include_noheader

INDEXWRT = $(LIBDIR)/Restructured/Writer/index.wrt
INDEXCMD = $(PRESTCMD) -w index -W short_titles=0
TOCWRT = $(LIBDIR)/Restructured/Writer/toc.wrt
TOCCMD = $(PRESTCMD) -w toc
XREFWRT = $(LIBDIR)/Restructured/Writer/xref.wrt
XREFCMD = $(PRESTCMD) -w xref -W file_suffix=htm

index.out: $(IDXRSTs) index.idx $(RST2DOC) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(INDEXCMD) $(IDXRSTs)

index.dbg: $(IDXRSTs) index.idx $(RST2DOC) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(INDEXCMD) $(IDXRSTs)

index.chk: $(IDXRSTs) index.idx $(PREST) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(INDEXCMD) $(IDXRSTs) | diff index.idx - > $@

toc%.out: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs)

toc%.dbg: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs)

toc%.chk: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs) | diff toc$*.toc - > $@
xreftest.out:	 xreftest.rst xreftest.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(XREFCMD) xreftest.rst 

xreftest.dbg:	 xreftest.rst xreftest.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(XREFCMD) xreftest.rst 

xreftest.chk:   xreftest.rst xreftest.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(XREFCMD) xreftest.rst | diff xreftest.xref - > $@
