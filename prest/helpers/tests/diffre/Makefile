GRESS_DIR = ../..

# Use the following to customize flags to gen_gress
EXECS = diff head
GEN_GRESS_FLAGS = -p $(subst $(subst X,,X X),:,$(sort $(dir $(shell which $(EXECS)))))

GRESS_LIB = $(GRESS_DIR)/lib
GRESS_BIN = $(GRESS_DIR)/bin
include $(GRESS_LIB)/gress.mak
