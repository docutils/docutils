Tests an elementary code block.

.. code_block:: makefile

   # A make file
   BIN_DIR = ../bin
   BIN_TARGETS = $(notdir $(PRL_FILES:.prl=) $(PM_FILES) $(WRT_FILES))
   DIREC_PM_FILES := $(filter-out %~,$(wildcard directives/*))
   DIREC_TARGETS = $(subst directives,Directive,$(DIREC_PM_FILES))
   BINS = $(addprefix $(BIN_DIR)/,$(BIN_TARGETS) $(DIREC_TARGETS))

   default:	$(BIN_DIR) $(BIN_DIR)/Directive $(BINS)

   $(BIN_DIR):	
	   mkdir $@
