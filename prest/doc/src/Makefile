# -*-Makefile-*-
PERL := $(shell perl -e '%C = eval("(".`cat ../../config.log`.")"); print $$C{perl}')
DEST_DIR = ../html
TOOLS_DIR = ../tools
DIFFRE_DIR = ../../tbin

RSTS := $(wildcard *.rst)
HTMLS := $(wildcard *.html)
DEST_HTMLS = $(addprefix $(DEST_DIR)/,$(RSTS:.rst=.html))
OTHER_TARGETS = $(addprefix $(DEST_DIR)/,index.html quickref.html)
LATEST_HTMLS := $(notdir $(wildcard latest/*.html))
DIFF_TARGETS = $(addprefix $(DEST_DIR)/,$(LATEST_HTMLS:.html=_diff.html))

PREST_BIN = ../../blib/script
PREST_LIB = ../../blib/lib/Text/Restructured
PREST_SRC := $(wildcard $(PREST_BIN)/prest $(PREST_BIN)/*.pm)
HTML_WRITER := $(wildcard $(PREST_LIB)/Writer/html.wrt)
XREF_WRITER := $(wildcard $(PREST_LIB)/Writer/xref.wrt)
HTML_FLAGS = -D source-link=0 \
	-W footnote-references=brackets -W field-limit=20 \
	-W cloak-email-addresses
XREF_FLAGS = -w xref
PREST_CMD = $(PERL) -I ../../blib/lib $(PREST_BIN)/prest
HTMLDIFF = $(TOOLS_DIR)/htmldiff.prl
HTMLDIFF_CMD = $(PERL) $(HTMLDIFF) -p '$(DIFFRE_DIR)/diffre -r "id\d+"' \
	-i '<U><FONT COLOR=\#008000>$$_</FONT></U>' -k -l
PERLSYNOPSIS = $(TOOLS_DIR)/perlsynopsis.prl
PERLSYNOPSIS_CMD = $(PERL) $(PERLSYNOPSIS)
BLANK = $(subst X,,X X)
PM_FILES = $(wildcard ../../blib/lib/Text/Restructured.pm ../../blib/lib/Text/Restructured/*.pm)

HTML_ALTS = ($(subst $(BLANK),|,$(RSTS:.rst=) $(HTMLS:.html=)))\.html

PREST_FLAGS_prest_usage = -D trusted
PREST_FLAGS_prest_internals = -D trusted
PREST_FLAGS_prest_extend = -D trusted

targets:	$(DEST_DIR) $(DEST_HTMLS) $(OTHER_TARGETS) $(DIFF_TARGETS)

debug:
	@echo '$(HTML_ALTS)'

%.xref:	%.rst $(PREST_SRC) $(XREF_WRITER)
	$(PREST_CMD) $(XREF_FLAGS) $(PREST_FLAGS_$(notdir $*)) $< > $@ 

# Create the destination directory.
$(DEST_DIR):
	mkdir -p $(DEST_DIR)
	-chmod 775 $(DEST_DIR)

$(DEST_DIR)/%.html: %.rst $(PREST_SRC) $(HTML_WRITER)
	$(PERL) -pe 's!../../\w+/rst/($(HTML_ALTS))!./$$1!g; s!\.\./!http://docutils.sourceforge.net/spec/!g' $< | \
	$(PREST_CMD) $(HTML_FLAGS) $(PREST_FLAGS_$(notdir $*)) -- - > $@ 
	-chmod 664 $@

$(DEST_DIR)/%_diff.html: $(DEST_DIR)/%.html latest/%.html $(HTMLDIFF)
	$(HTMLDIFF_CMD) latest/$*.html $(DEST_DIR)/$*.html > $@
	-chmod 664 $@

$(DEST_DIR)/index.html:
	(cd $(DEST_DIR); ln -s prest_intro.html $@)

$(DEST_DIR)/%: %
	cp -p $< $@

clean:
	rm -rf $(DEST_DIR) *.xref

## Dependencies
$(DEST_DIR)/prest_internals.html:	$(PERLSYNOPSIS) $(PM_FILES)

$(DEST_DIR)/prest_extend.html:	prest_internals.xref
