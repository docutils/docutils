MAKS := $(wildcard *.mak)
MAK_TARGETS = $(MAKS)
DEST_DIR = ../../lib
DEST_COPYFILES = $(DEST_DIR)/copyfiles
COPYFILES := $(filter-out %/CVS,$(filter-out %~,$(filter-out %.root,$(wildcard copyfiles/*))))
TARGETS = $(addprefix $(DEST_DIR)/,$(MAK_TARGETS) $(COPYFILES))

default: $(DEST_DIR) $(DEST_COPYFILES) $(TARGETS)

testmake:
	@echo $(TARGETS)

$(DEST_DIR):
	mkdir -p $(DEST_DIR)

$(DEST_COPYFILES):
	mkdir -p $(DEST_COPYFILES)

$(DEST_DIR)/%:	%
	cp -f $< $@
	chmod -w $@

$(DEST_COPYFILES)/%:	copyfiles/%
	cp -f $< $@
	chmod -w $@
