export TOP_DIR := $(PWD)
export SRC_FILES := $(wildcard $(TOP_DIR)/*.el)
export TEST_FILES
export REFERENCE_FILES
export TEST_EXTS
export LATEX ?= lualatex

EMACS ?= emacs
INIT_EL ?= "(progn (require 'omnix) (require 'batch-ox) (omnix-global))"
LOAD_PATH ?= -L $(TOP_DIR) -L $(TOP_DIR)/vendor/batch-ox
export CONVERT := $(EMACS) -Q --batch $(LOAD_PATH) --eval $(INIT_EL)


.PHONY: check
check:
	$(MAKE) -C tests check

.PHONY: references
references:
	$(MAKE) -C tests references

.PHONY: check-pdfs
check-pdfs:
	$(MAKE) -C tests pdfs

.PHONY: clean
clean:
	$(MAKE) -C tests clean
