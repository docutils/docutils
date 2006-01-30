PRL_FILES := $(wildcard *.prl)
PM_FILES := $(wildcard *.pm)
MAK_FILES := $(wildcard *.mak)
BIN_DIR = ../../bin
TARGETS = $(notdir $(PRL_FILES:.prl=) $(PM_FILES) $(MAK_FILES))
BINS = $(addprefix $(BIN_DIR)/,$(TARGETS))

default:	$(BIN_DIR) $(BINS)

$(BIN_DIR):	
	mkdir $@

$(BIN_DIR)/%:	%
	rm -f $@; cp $< $@; chmod 444 $@

$(BIN_DIR)/%: %.prl
	cp -f $< $@; chmod 555 $@
