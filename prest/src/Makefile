PRL_FILES := $(wildcard *.prl)
PM_FILES := $(wildcard *.pm)
WRT_FILES := $(wildcard writers/*.wrt)
BIN_DIR = ../bin
BIN_TARGETS = $(notdir $(PRL_FILES:.prl=) $(PM_FILES) $(WRT_FILES))
DIREC_PM_FILES := $(filter-out %~,$(wildcard directives/*))
DIREC_TARGETS = $(subst directives,Directive,$(DIREC_PM_FILES))
BINS = $(addprefix $(BIN_DIR)/,$(BIN_TARGETS) $(DIREC_TARGETS))

default:	$(BIN_DIR) $(BIN_DIR)/Directive $(BINS)

$(BIN_DIR):	
	mkdir $@

$(BIN_DIR)/Directive:
	mkdir $(BIN_DIR)/Directive

$(INSTALL_DIR)/%:	$(BIN_DIR)/%
	rm -f $@; cp $< $@

$(BIN_DIR)/Directive/%:	directives/%
	rm -f $@; cp $< $@; chmod 444 $@

$(BIN_DIR)/%:	writers/%
	rm -f $@; cp $< $@; chmod 444 $@

$(BIN_DIR)/%:	%
	rm -f $@; cp $< $@; chmod 444 $@

$(BIN_DIR)/%: %.prl insertperl.pl
	rm -f $@; perl insertperl.pl $< > $@; chmod 555 $@


