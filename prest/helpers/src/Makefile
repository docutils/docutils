SUBDIRS = $(dir $(wildcard */Makefile))
MAKESUBDIRS = $(addprefix make, $(SUBDIRS))

default: subdirs

subdirs:	$(MAKESUBDIRS)

debug:
	@echo $(MAKESUBDIRS)

.PHONY: $(SUBDIRS)

make%:
	@cd $*; make
