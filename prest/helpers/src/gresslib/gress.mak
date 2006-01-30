# -*- Makefile -*-
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

TBIN = ..

# To run a specific test case or set of test cases, override the TC variable
# on the make command line to point to some combination of directories or 
# .re files in subdirectories.
TC = *

# Elide can exist either in our local directory (if it's only for this
# tool) or in the TBIN directory (if it's for all the tools in the 
# regression_tests suite), or it may not exist at all.
ELIDE := $(firstword $(wildcard elide.prl $(TBIN)/elide.prl))
ELIDE_CMD := perl -p $(firstword $(ELIDE) /dev/null)
DRIVERS := $(wildcard */*.sh)
#DRIVER_DIRS = $(sort $(dir $(DRIVERS)))
SRC = ../../src
BIN = ../../bin
SRC_FILES := $(wildcard $(SRC)/*)
BIN_FILES := $(wildcard $(BIN)/*)
RES := $(filter %.re,$(wildcard $(TC:=/*.re) $(TC)))
SHS = $(RES:.re=.sh)
#INITS := $(shell find $(wildcard */*init) /dev/null -exec perl -e '$$_ = "{}"; s/ /\\ /g; print "$$_\n"' \;)
INITS := $(shell find $(wildcard */*init) /dev/null -not -path '*/\.svn/*' -and -not -path '*/CVS/*' -and -exec perl -e '$$_ = "{}"; s/ /\\ /g; print "$$_\n"' \;)

default:	runtest

.PHONY:	runtest clean realclean

include depends.mak

depends.mak:	$(GRESS_LIB)/gress.mak $(wildcard */*init)
	@echo Building depends.mak
	@perl -e 'foreach (@ARGV){my ($$f, $$d) = m|((.*)/(.*)).sh|; print qq($$f.out: \$$(shell find \$$(wildcard $$d/init $$f.init) /dev/null -type f | egrep -v "/(CVS|RCS)")\n\n)}' $(DRIVERS) > $@

runtest:	$(SHS)
	$(GRESS_BIN)/run_gress $(RUN_GRESS_FLAGS) $(TC)

.PRECIOUS:	%.sh

debug:
	@echo $(INITS)

$(INITS):
	@echo Cannot modify init directories with Makefile

%.out:	%.sh $(BIN_FILES) $(SRC_FILES)
	$< > $@ 2>&1

%.re:	%.out $(ELIDE)
	perl -pe 's/([\[\]\(\)\\\.\|\*\+\?\{\}\$$\@\%\^])/\\$$1/g' $*.out | \
	$(ELIDE_CMD) > $@

%.sh:	%.job $(GRESS_BIN)/gen_gress Makefile
	if ( test "./" != "$(dir $*)" ) ; then \
	$(GRESS_BIN)/gen_gress -s $(notdir $*).dir -i $(notdir $*).init $(GEN_GRESS_FLAGS) $< > $@; \
	chmod +x $@; fi

clean:	
	rm -f *.log *.plog */*.out depends.mak `find . -name '*~'`
	rm -rf */*.dir

realclean:	clean
	rm -f */*.sh
