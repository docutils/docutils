# This makefile is used only for generating a code coverage database for 
# the regression suite and should not be needed for ordinary users of the
# Text::Restructured package

SOURCES := $(shell find ../blib -type f -and -not -name '.*')
UNCOVERABLE = ../tbin/uncoverable
PERL := $(shell perl -I../blib/lib -e 'use Text::Restructured::PrestConfig; print "$$Text::Restructured::PrestConfig::SAFE_PERL\n"')
COVER = $(shell which cover)

peek:
	@echo $(PERL)

ifdef COVER
cover_db/runs:	$(SOURCES)
	-rm -rf cover_db/runs cover_db/structure
	env COVER=1 MAKELEVEL= make -C .. test

.PHONY:	cover cover_only

cover:	.uncoverable cover_db/runs
	$(PERL) $(COVER)

cover_only:  .uncoverable
	$(PERL) $(COVER)

.uncoverable:	$(SOURCES) $(UNCOVERABLE)
	-rm -f $@
	mkdir -p cover_db
	$(PERL) $(UNCOVERABLE) $(SOURCES)

endif
