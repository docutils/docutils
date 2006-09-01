# -*-makefile-*-
# Note: Since this makefile is checking three writers, it's doesn't
# fit cleanly in the Common.mk framework.

# Tests for the idx writer
IDXRSTs := $(wildcard idxtest*)

TOCs    := $(wildcard toc*.toc)
TOCCHKs = $(TOCs:.toc=.chk)
IDXs	:= $(wildcard *.idx)
IDXCHKs := $(IDXs:.idx=.chk)
XREFs	:= $(wildcard *.xref)
XREFCHKs := $(XREFs:.xref=.chk)
CHKs	:= $(IDXCHKs) $(TOCCHKs) $(XREFCHKs)

peek:
	@echo $(CHKs)

RST_FLAG_index01 = -W short_titles=0
RST_FLAG_index02 =
RST_FLAG_toc01 = -W symbol='=' -W top_in_list
RST_FLAG_toc02 = -W symbol='=' -W exclude_top
RST_FLAG_toc03 = -W symbol='='
RST_FLAG_toc04 =
RST_FLAG_toc05 = -W top_in_list -W file_suffix=htm -W filename_ext=_main
RST_FLAG_toc06 = -W exclude_top -W depth=2
RST_FLAG_toc07 = -W top_in_list=0
RST_FLAG_toc08 = -W exclude_top
RST_FLAG_toc09 = -W top_in_list -W depth=1
RST_FLAG_toc10 = -W symbol="=" -W top_in_list -W include_noheader
RST_FLAG_toc11 = -W top_in_list -W include_noheader
RST_FLAG_toc12 = -W symbol="=" -W include_noheader
RST_FLAG_toc13 = -D keep_title_section
RST_FLAG_xref01 = -W file_suffix=htm
RST_FLAG_xref02 =
RST_FLAG_xref03 = -W filename_ext=_main -W file_path=.
RST_FLAG_xref04 = -W sprintf='.. perl:: $$s=q(%s); $$s =~ /raw/ ? $$s : ""'
# Note: A final -D swallows up the original file name
RST_FLAG_xref05 = idxtest4 -D 
RST_FLAG_xref06 = dupname.rst -W file_path=./ -D

INDEXWRT = $(LIBDIR)/Restructured/Writer/index.wrt
INDEXCMD = $(PRESTCMD) -w index
TOCWRT = $(LIBDIR)/Restructured/Writer/toc.wrt
TOCCMD = $(PRESTCMD) -w toc
XREFWRT = $(LIBDIR)/Restructured/Writer/xref.wrt
XREFCMD = $(PRESTCMD) -w xref

index%.out: $(IDXRSTs) index%.idx $(RST2DOC) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(INDEXCMD) $(RST_FLAG_index$*) $(IDXRSTs)

index%.dbg: $(IDXRSTs) index%.idx $(RST2DOC) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(INDEXCMD) $(RST_FLAG_index$*) $(IDXRSTs) $(DEBUG_FLAGS)

index%.chk: $(IDXRSTs) index%.idx $(PREST) $(INDEXWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(INDEXCMD) $(RST_FLAG_index$*) $(IDXRSTs) | diff index$*.idx - > $@

toc%.out: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs)

toc%.dbg: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs) $(DEBUG_FLAGS)

toc%.chk: $(IDXRSTs) toc%.toc $(PREST) $(TOCWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(TOCCMD) $(RST_FLAG_toc$*) $(IDXRSTs) | diff toc$*.toc - > $@
xref%.out:	 xreftest.rst xref%.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(XREFCMD) $(RST_FLAG_xref$*) xreftest.rst 

xref%.dbg:	 xreftest.rst xref%.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) -d $(XREFCMD) $(RST_FLAG_xref$*) xreftest.rst $(DEBUG_FLAGS)

xref%.chk:   xreftest.rst xref%.xref $(PREST) $(XREFWRT) $(PMFILES)
	@$(PERL) $(PERL_FLAGS) $(XREFCMD) $(RST_FLAG_xref$*) xreftest.rst | diff xref$*.xref - > $@
